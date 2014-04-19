/**
 * @file   erlcmd.c
 * @author Frank Hunleth
 * @brief  Erlang interface
 * @description
 *
 * @section LICENSE
 * Copyright (C) 2014 Frank Hunleth
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
		 void (*request_handler)(ETERM *emsg, void *cookie),
		 void *cookie)
{
    erl_init(NULL, 0);
    memset(handler, 0, sizeof(*handler));

    handler->request_handler = request_handler;
    handler->cookie = cookie;
}

/**
 * @brief Synchronously send a response back to Erlang
 *
 * @param response what to send back
 */
void erlcmd_send(ETERM *response)
{
    unsigned char buf[1024];

    if (erl_encode(response, buf + sizeof(uint16_t)) == 0)
	errx(EXIT_FAILURE, "erl_encode");

    ssize_t len = erl_term_len(response);
    uint16_t be_len = htons(len);
    memcpy(buf, &be_len, sizeof(be_len));

    len += sizeof(uint16_t);
    ssize_t wrote = 0;
    do {
	ssize_t amount_written = write(STDOUT_FILENO, buf + wrote, len - wrote);
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
static ssize_t erlcmd_try_dispatch(struct erlcmd *handler)
{
    /* Check for length field */
    if (handler->index < sizeof(uint16_t))
	return 0;

    uint16_t be_len;
    memcpy(&be_len, handler->buffer, sizeof(uint16_t));
    ssize_t msglen = ntohs(be_len);
    if (msglen + sizeof(uint16_t) > sizeof(handler->buffer))
	errx(EXIT_FAILURE, "Message too long");

    /* Check whether we've received the entire message */
    if (msglen + sizeof(uint16_t) > handler->index)
	return 0;

    ETERM *emsg = erl_decode(handler->buffer + sizeof(uint16_t));
    if (emsg == NULL)
	errx(EXIT_FAILURE, "erl_decode");

    handler->request_handler(emsg, handler->cookie);

    erl_free_term(emsg);

    return msglen + sizeof(uint16_t);
}

/**
 * @brief call to process any new requests from Erlang
 */
void erlcmd_process(struct erlcmd *handler)
{
    ssize_t amount_read = read(STDIN_FILENO, handler->buffer, sizeof(handler->buffer) - handler->index);
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
	ssize_t bytes_processed = erlcmd_try_dispatch(handler);

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
