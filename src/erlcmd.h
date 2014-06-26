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
 * Common Erlang->C port communications declarations
 */

#ifndef ERLCMD_H
#define ERLCMD_H

#include <ei.h>

/*
 * Erlang request/response processing
 */
#define ERLCMD_BUF_SIZE 1024
struct erlcmd
{
    char buffer[ERLCMD_BUF_SIZE];
    size_t index;

    void (*request_handler)(const char *emsg, void *cookie);
    void *cookie;
};

void erlcmd_init(struct erlcmd *handler,
		 void (*request_handler)(const char *req, void *cookie),
		 void *cookie);
void erlcmd_send(char *response, size_t len);
void erlcmd_process(struct erlcmd *handler);

#endif
