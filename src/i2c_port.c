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
#define debug(...) fprintf(stderr, __VA_ARGS__)
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
static int i2c_write(struct i2c_info *i2c, unsigned char *data, unsigned int len)
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

static void i2c_handle_request(ETERM *emsg, void *cookie)
{
    struct i2c_info *i2c = (struct i2c_info *) cookie;

    // Commands are of the form {Command, Arguments}:
    // { atom(), [term()] }

    ETERM *cmd = erl_element(1, emsg);
    ETERM *args = erl_element(2, emsg);
    if (cmd == NULL || args == NULL)
        errx(EXIT_FAILURE, "Expecting { cmd, args }");

    debug("i2c_request_handler: %s\n", ERL_ATOM_PTR(cmd));

    ETERM *resp;
    if (strcmp(ERL_ATOM_PTR(cmd), "read") == 0) {
        ETERM *elen = erl_hd(args);
        int len = ERL_INT_VALUE(elen);
        if (len > I2C_SMBUS_BLOCK_MAX)
            errx(EXIT_FAILURE, "Can't get more than %d bytes at time: %d", I2C_SMBUS_BLOCK_MAX, len);

        char data[len];

        // calls the i2c_read function and returns an erlang tuple with data
        if (i2c_read(i2c, data, len))
            resp = erl_mk_binary(data, len);
        else
            resp = erl_format("{error, i2c_read_failed}");
    } else if (strcmp(ERL_ATOM_PTR(cmd), "write") == 0) {
        ETERM *edata = erl_hd(args);
        if (edata == NULL)
            errx(EXIT_FAILURE, "write: didn't get value to write");

        // calls the i2c_write function and returns 1 if success or -1 if fails
        if (i2c_write(i2c,
                      ERL_BIN_PTR(edata),
                      ERL_BIN_SIZE(edata)))
            resp = erl_format("ok");
        else
            resp = erl_format("{error, i2c_write_failed}");
        erl_free_term(edata);
    } else {
        resp = erl_format("error");
    }
    erlcmd_send(resp);

    erl_free_term(resp);
    erl_free_term(cmd);
    erl_free_term(args);
}

/**
 * @brief The main function.
 * It waits for data in the buffer and calls the driver.
 */
int main(int argc, char *argv[])
{
    if (argc != 3)
        errx(EXIT_FAILURE, "Must pass device path and device address as arguments");

    struct i2c_info i2c;
    i2c_init(&i2c, argv[1], strtoul(argv[2], 0, 0));

    struct erlcmd handler;
    erlcmd_init(&handler, i2c_handle_request, &i2c);

    for (;;) {
        // Loop forever and process requests from Erlang.
        erlcmd_process(&handler);
    }

    return 1;
}
