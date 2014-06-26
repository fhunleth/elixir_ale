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
 * Common Erlang->C port communications code
 */

#include "erlcmd.h"

#include <err.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/**
 * Initialize an Erlang command handler.
 *
 * @param handler the structure to initialize
 * @param request_handler callback for each message received
 * @param cookie optional data to pass back to the handler
 */
void erlcmd_init(struct erlcmd *handler,
		 void (*request_handler)(const char *req, void *cookie),
		 void *cookie)
{
    memset(handler, 0, sizeof(*handler));

    handler->request_handler = request_handler;
    handler->cookie = cookie;
}

/**
 * @brief Synchronously send a response back to Erlang
 *
 * @param response what to send back
 */
void erlcmd_send(char *response, size_t len)
{
    uint16_t be_len = htons(len - sizeof(uint16_t));
    memcpy(response, &be_len, sizeof(be_len));

    size_t wrote = 0;
    do {
	ssize_t amount_written = write(STDOUT_FILENO, response + wrote, len - wrote);
	if (amount_written < 0) {
	    if (errno == EINTR)
		continue;

	    err(EXIT_FAILURE, "write");
	}

	wrote += amount_written;
    } while (wrote < len);
}

/**
 * @brief Dispatch commands in the buffer
 * @return the number of bytes processed
 */
static size_t erlcmd_try_dispatch(struct erlcmd *handler)
{
    /* Check for length field */
    if (handler->index < sizeof(uint16_t))
	return 0;

    uint16_t be_len;
    memcpy(&be_len, handler->buffer, sizeof(uint16_t));
    size_t msglen = ntohs(be_len);
    if (msglen + sizeof(uint16_t) > sizeof(handler->buffer))
	errx(EXIT_FAILURE, "Message too long");

    /* Check whether we've received the entire message */
    if (msglen + sizeof(uint16_t) > handler->index)
	return 0;

    handler->request_handler(handler->buffer, handler->cookie);

    return msglen + sizeof(uint16_t);
}

/**
 * @brief call to process any new requests from Erlang
 */
void erlcmd_process(struct erlcmd *handler)
{
    ssize_t amount_read = read(STDIN_FILENO, handler->buffer + handler->index, sizeof(handler->buffer) - handler->index);
    if (amount_read < 0) {
	/* EINTR is ok to get, since we were interrupted by a signal. */
	if (errno == EINTR)
	    return;

	/* Everything else is unexpected. */
	err(EXIT_FAILURE, "read");
    } else if (amount_read == 0) {
	/* EOF. Erlang process was terminated. This happens after a release or if there was an error. */
	exit(EXIT_SUCCESS);
    }

    handler->index += amount_read;
    for (;;) {
	size_t bytes_processed = erlcmd_try_dispatch(handler);

	if (bytes_processed == 0) {
	    /* Only have part of the command to process. */
	    break;
	} else if (handler->index > bytes_processed) {
	    /* Processed the command and there's more data. */
	    memmove(handler->buffer, &handler->buffer[bytes_processed], handler->index - bytes_processed);
	    handler->index -= bytes_processed;
	} else {
	    /* Processed the whole buffer. */
	    handler->index = 0;
	    break;
	}
    }
}
