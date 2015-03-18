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
 * @brief	I2C combined write/read operation
 *
 * This function can be used to individually read or write
 * bytes across the bus. Additionally, a write and read
 * operation can be combined into one transaction. This is
 * useful for communicating with register-based devices that
 * support setting the current register via the first one or
 * two bytes written.
 *
 * @param	to_write	Optional write buffer
 * @param	to_write_len	Write buffer length
 * @param	to_read	        Optional read buffer
 * @param	to_read_len	Read buffer length
 *
 * @return 	1 for success, 0 for failure
 */
static int i2c_transfer(const struct i2c_info *i2c,
                        const char *to_write, size_t to_write_len,
                        char *to_read, size_t to_read_len)
{
    struct i2c_rdwr_ioctl_data data;
    struct i2c_msg msgs[2];

    msgs[0].addr = i2c->addr;
    msgs[0].flags = 0;
    msgs[0].len = to_write_len;
    msgs[0].buf = (uint8_t *) to_write;

    msgs[1].addr = i2c->addr;
    msgs[1].flags = I2C_M_RD;
    msgs[1].len = to_read_len;
    msgs[1].buf = (uint8_t *) to_read;

    if (to_write_len != 0)
        data.msgs = &msgs[0];
    else
        data.msgs = &msgs[1];

    data.nmsgs = (to_write_len != 0 && to_read_len != 0) ? 2 : 1;

    int rc = ioctl(i2c->fd, I2C_RDWR, &data);
    if (rc < 0)
        return 0;
    else
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

        if (i2c_transfer(i2c, 0, 0, data, len))
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
        if (ei_get_type(req, &req_index, &type, &len) < 0 ||
                type != ERL_BINARY_EXT ||
                len < 1 ||
                len > I2C_SMBUS_BLOCK_MAX ||
                ei_decode_binary(req, &req_index, &data, &llen) < 0)
            errx(EXIT_FAILURE, "write: need a binary between 1 and %d bytes", I2C_SMBUS_BLOCK_MAX);

        if (i2c_transfer(i2c, data, len, 0, 0))
            ei_encode_atom(resp, &resp_index, "ok");
        else {
            ei_encode_tuple_header(resp, &resp_index, 2);
            ei_encode_atom(resp, &resp_index, "error");
            ei_encode_atom(resp, &resp_index, "i2c_write_failed");
        }
    } else if (strcmp(cmd, "wrrd") == 0) {
        char write_data[I2C_SMBUS_BLOCK_MAX];
        char read_data[I2C_SMBUS_BLOCK_MAX];
        int write_len;
        long int read_len;
        int type;
        long llen;

        if (ei_decode_tuple_header(req, &req_index, &arity) < 0 ||
            arity != 2)
            errx(EXIT_FAILURE, "wrrd: expecting {write_data, read_count} tuple");

        if (ei_get_type(req, &req_index, &type, &write_len) < 0 ||
                type != ERL_BINARY_EXT ||
                write_len < 1 ||
                write_len > I2C_SMBUS_BLOCK_MAX ||
                ei_decode_binary(req, &req_index, &write_data, &llen) < 0)
            errx(EXIT_FAILURE, "wrrd: need a binary between 1 and %d bytes", I2C_SMBUS_BLOCK_MAX);
        if (ei_decode_long(req, &req_index, &read_len) < 0 ||
                read_len < 1 ||
                read_len > I2C_SMBUS_BLOCK_MAX)
            errx(EXIT_FAILURE, "wrrd: read amount: min=1, max=%d", I2C_SMBUS_BLOCK_MAX);

        if (i2c_transfer(i2c, write_data, write_len, read_data, read_len))
            ei_encode_binary(resp, &resp_index, read_data, read_len);
        else {
            ei_encode_tuple_header(resp, &resp_index, 2);
            ei_encode_atom(resp, &resp_index, "error");
            ei_encode_atom(resp, &resp_index, "i2c_wrrd_failed");
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
