/*
 *  Copyright 2014 Frank Hunleth
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPI port implementation.
 *
 * This code has been heavily modified from Erlang/ALE.
 * Copyright (C) 2013 Erlang Solutions Ltd.
 * See http://opensource.erlang-solutions.com/erlang_ale/.
 */

#include <err.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#ifndef _IOC_SIZE_BITS
// Include <asm/ioctl.h> manually on platforms that don't include it
// from <sys/ioctl.h>.
#include <asm/ioctl.h>
#endif
#include <linux/spi/spidev.h>

#include "erlcmd.h"

//#define DEBUG
#ifdef DEBUG
#define debug(...) do { fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\r\n"); } while(0)
#else
#define debug(...)
#endif

// Max SPI transfer size that we support
#define SPI_TRANSFER_MAX 256

struct spi_info
{
    int fd;

    struct spi_ioc_transfer transfer;
};

/**
 * @brief        Initialize a SPI device
 *
 * @param        spi     Handle to initialize
 * @param        devpath Path to SPI device file
 * @param        mode    SPI mode
 * @param        bits_per_word    Number of bits
 * @param        speed_hz   Bus speed
 * @param        delay_usecs   Delay between transfers
 *
 * @return       1 if success, -1 if fails
 */
static void spi_init(struct spi_info *spi,
                     const char *devpath,
                     uint8_t mode,
                     uint8_t bits_per_word,
                     uint32_t speed_hz,
                     uint16_t delay_usecs)
{
    memset(spi, 0, sizeof(*spi));

    spi->transfer.speed_hz = speed_hz;
    spi->transfer.delay_usecs = delay_usecs;
    spi->transfer.bits_per_word = bits_per_word;

    // Fail hard on error. May need to be nicer if this makes the
    // Erlang side too hard to debug.
    spi->fd = open(devpath, O_RDWR);
    if (spi->fd < 0)
        err(EXIT_FAILURE, "open %s", devpath);

    if (ioctl(spi->fd, SPI_IOC_WR_MODE, &mode) < 0)
        err(EXIT_FAILURE, "ioctl(SPI_IOC_WR_MODE %d)", mode);

    // Set these to check for bad values given by the user. They get
    // set again on each transfer.
    if (ioctl(spi->fd, SPI_IOC_WR_BITS_PER_WORD, &bits_per_word) < 0)
        err(EXIT_FAILURE, "ioctl(SPI_IOC_WR_BITS_PER_WORD %d)", bits_per_word);

    if (ioctl(spi->fd, SPI_IOC_WR_MAX_SPEED_HZ, &speed_hz) < 0)
        err(EXIT_FAILURE, "ioctl(SPI_IOC_WR_MAX_SPEED_HZ %d)", speed_hz);
}

/**
 * @brief	spi transfer operation
 *
 * @param	tx      Data to write into the device
 * @param	rx      Data to read from the device
 * @param	len     Length of data
 *
 * @return 	1 for success, 0 for failure
 */
static int spi_transfer(struct spi_info *spi, const char *tx, char *rx, unsigned int len)
{
    struct spi_ioc_transfer tfer = spi->transfer;

    tfer.tx_buf = (__u64) tx;
    tfer.rx_buf = (__u64) rx;
    tfer.len = len;

    if (ioctl(spi->fd, SPI_IOC_MESSAGE(1), &tfer) < 1)
        err(EXIT_FAILURE, "ioctl(SPI_IOC_MESSAGE)");

    return 1;
}

static void spi_handle_request(const char *req, void *cookie)
{
    struct spi_info *spi = (struct spi_info *) cookie;

    // Commands are of the form {Command, Arguments}:
    // { atom(), term() }
    int req_index = sizeof(uint16_t);
    if (ei_decode_version(req, &req_index, NULL) < 0)
        errx(EXIT_FAILURE, "Message version issue?");

    int arity;
    if (ei_decode_tuple_header(req, &req_index, &arity) < 0 ||
            arity != 2)
        errx(EXIT_FAILURE, "expecting {cmd, args} tuple");

    char cmd[MAXATOMLEN];
    if (ei_decode_atom(req, &req_index, cmd) < 0)
        errx(EXIT_FAILURE, "expecting command atom");

    char resp[SPI_TRANSFER_MAX + 64];
    int resp_index = sizeof(uint16_t); // Space for payload size
    ei_encode_version(resp, &resp_index);
    if (strcmp(cmd, "transfer") == 0) {
        char data[SPI_TRANSFER_MAX];
        int len;
        int type;
        long llen;
        if (ei_get_type(req, &req_index, &type, &len) < 0 ||
                type != ERL_BINARY_EXT ||
                len < 1 ||
                len > SPI_TRANSFER_MAX ||
                ei_decode_binary(req, &req_index, &data, &llen) < 0)
            errx(EXIT_FAILURE, "transfer: need a binary between 1 and %d bytes (%d, %d)", SPI_TRANSFER_MAX,
                    type, len);

        char rxbuffer[SPI_TRANSFER_MAX];

        if (spi_transfer(spi,
                         data,
                         rxbuffer,
                         len))
            ei_encode_binary(resp, &resp_index, rxbuffer, len);
        else {
            ei_encode_tuple_header(resp, &resp_index, 2);
            ei_encode_atom(resp, &resp_index, "error");
            ei_encode_atom(resp, &resp_index, "spi_transfer_failed");
        }
    } else
        errx(EXIT_FAILURE, "unknown command: %s", cmd);

    debug("sending response: %d bytes", resp_index);
    erlcmd_send(resp, resp_index);
}

/**
 * @brief The main function.
 * It waits for data in the buffer and calls the driver.
 */
int spi_main(int argc, char *argv[])
{
    if (argc != 7)
        errx(EXIT_FAILURE, "%s spi <device path> <SPI mode (0-3)> <bits/word (8)> <speed (1000000 Hz)> <delay (10 us)>", argv[0]);

    const char *devpath = argv[2];
    uint8_t mode = (uint8_t) strtoul(argv[3], 0, 0);
    uint8_t bits = (uint8_t) strtoul(argv[4], 0, 0);
    uint32_t speed = (uint32_t) strtoul(argv[5], 0, 0);
    uint16_t delay = (uint16_t) strtoul(argv[6], 0, 0);

    struct spi_info spi;
    spi_init(&spi, devpath, mode, bits, speed, delay);

    struct erlcmd handler;
    erlcmd_init(&handler, spi_handle_request, &spi);

    for (;;) {
        // Loop forever and process requests from Erlang.
        erlcmd_process(&handler);
    }

    return 1;
}
