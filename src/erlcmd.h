/**
 * @file   erlcmd.c
 * @author Frank Hunleth
 * @brief  Erlang interface
 * @description
 *
 * @section LICENSE
 * Copyright (C) 2014 Frank Hunleth
 */

#ifndef ERLCMD_H
#define ERLCMD_H

#include <erl_interface.h>
#include <ei.h>

/*
 * Erlang request/response processing
 */
#define ERLCMD_BUF_SIZE 1024
struct erlcmd
{
    unsigned char buffer[ERLCMD_BUF_SIZE];
    ssize_t index;

    void (*request_handler)(ETERM *emsg, void *cookie);
    void *cookie;
};

void erlcmd_init(struct erlcmd *handler,
		 void (*request_handler)(ETERM *emsg, void *cookie),
		 void *cookie);
void erlcmd_send(ETERM *response);
void erlcmd_process(struct erlcmd *handler);

#endif
