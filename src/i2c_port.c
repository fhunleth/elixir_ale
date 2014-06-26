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
 * I2C port implementation.
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

#include <linux/i2c.h>
#include <linux/i2c-dev.h>

#include "erlcmd.h"

//#define DEBUG
#ifdef DEBUG
#define debug(...) do { fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\r\n"); } while(0)
#else
#define debug(...)
#endif

struct i2c_info
{
    int fd;
    unsigned int addr;
};

static void i2c_init(struct i2c_info *i2c, const char *devpath, unsigned int addr)
{
    memset(i2c, 0, sizeof(*i2c));

    // Fail hard on error. May need to be nicer if this makes the
    // Erlang side too hard to debug.
    i2c->fd = open(devpath, O_RDWR);
    if (i2c->fd < 0)
        err(EXIT_FAILURE, "open %s", devpath);

    if (ioctl(i2c->fd, I2C_SLAVE, addr) < 0)
        err(EXIT_FAILURE, "ioctl(I2C_SLAVE %d)", addr);

    i2c->addr = addr;
}

/**
 * @brief	I2C write operation
 *
 * @param	data	Data to write into the device
 * @param	len     Length of data
 *
 * @return 	1 for success, 0 for failure
 */
static int i2c_write(struct i2c_info *i2c, char *data, unsigned int len)
{
    if (write(i2c->fd, data, len) != len) {
        warn("I2C write (address: 0x%X) of %d bytes failed", i2c->addr, len);
        return 0;
    }

    return 1;
}

/**
 * @brief	I2C read operation
 *
 * @param	data	Pointer to the read buffer
 * @param	len	Length of data
 *
 * @return 	1 for success, 0 for failure
 */
static int i2c_read(struct i2c_info *i2c, char *data, unsigned int len)
{
    if (read(i2c->fd, data, len) != len) {
        warn("I2C read (address: 0x%X) of %d bytes failed", i2c->addr, len);
        return 0;
    }
    return 1;
}

static void i2c_handle_request(const char *req, void *cookie)
{
    struct i2c_info *i2c = (struct i2c_info *) cookie;

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

    char resp[256];
    int resp_index = sizeof(uint16_t); // Space for payload size
    ei_encode_version(resp, &resp_index);
    if (strcmp(cmd, "read") == 0) {
        long int len;
        if (ei_decode_long(req, &req_index, &len) < 0 ||
                len < 1 ||
                len > I2C_SMBUS_BLOCK_MAX)
            errx(EXIT_FAILURE, "read amount: min=1, max=%d", I2C_SMBUS_BLOCK_MAX);

        char data[I2C_SMBUS_BLOCK_MAX];

        // calls the i2c_read function and returns an erlang tuple with data
        if (i2c_read(i2c, data, len))
            ei_encode_binary(resp, &resp_index, data,len);
        else {
            ei_encode_tuple_header(resp, &resp_index, 2);
            ei_encode_atom(resp, &resp_index, "error");
            ei_encode_atom(resp, &resp_index, "i2c_read_failed");
        }
    } else if (strcmp(cmd, "write") == 0) {
        char data[I2C_SMBUS_BLOCK_MAX];
        int len;
        int type;
        long llen;
        if (ei_get_type(resp, &resp_index, &type, &len) < 0 ||
                type != ERL_BINARY_EXT ||
                len < 1 ||
                len > I2C_SMBUS_BLOCK_MAX ||
                ei_decode_binary(resp, &resp_index, &data, &llen) < 0)
            errx(EXIT_FAILURE, "write: need a binary between 1 and %d bytes", I2C_SMBUS_BLOCK_MAX);

        // calls the i2c_write function and returns 1 if success or -1 if fails
        if (i2c_write(i2c, data, len))
            ei_encode_atom(resp, &resp_index, "ok");
        else {
            ei_encode_tuple_header(resp, &resp_index, 2);
            ei_encode_atom(resp, &resp_index, "error");
            ei_encode_atom(resp, &resp_index, "i2c_write_failed");
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
int i2c_main(int argc, char *argv[])
{
    if (argc != 4)
        errx(EXIT_FAILURE, "Must pass device path and device address as arguments");

    struct i2c_info i2c;
    i2c_init(&i2c, argv[2], strtoul(argv[3], 0, 0));

    struct erlcmd handler;
    erlcmd_init(&handler, i2c_handle_request, &i2c);

    for (;;) {
        // Loop forever and process requests from Erlang.
        erlcmd_process(&handler);
    }

    return 1;
}
