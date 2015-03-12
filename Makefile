# Variables to override
#
# CC            C compiler
# CROSSCOMPILE	crosscompiler prefix, if any
# CFLAGS	compiler flags for compiling all C files
# ERL_CFLAGS	additional compiler flags for files using Erlang header files
# ERL_EI_LIBDIR path to libei.a
# LDFLAGS	linker flags for linking all binaries
# ERL_LDFLAGS	additional linker flags for projects referencing Erlang libraries
# MIX		path to mix

# Look for the EI library and header files
ERL_EI_INCLUDE_DIR ?= $(shell find /usr/local/lib/erlang /usr/lib/erlang -name ei.h -printf '%h\n' 2> /dev/null | head -1)
ERL_EI_LIBDIR ?= $(shell find /usr/local/lib/erlang /usr/lib/erlang -name libei.a -printf '%h\n' 2> /dev/null | head -1)

ifeq ($(ERL_EI_INCLUDE_DIR),)
   $(error Could not find include directory for ei.h. Check that Erlang header files are available)
endif
ifeq ($(ERL_EI_LIBDIR),)
   $(error Could not find libei.a. Check your Erlang installation)
endif

# Set Erlang-specific compile and linker flags
ERL_CFLAGS ?= -I$(ERL_EI_INCLUDE_DIR)
ERL_LDFLAGS ?= -L$(ERL_EI_LIBDIR) -lei

LDFLAGS +=
CFLAGS ?= -O2 -Wall -Wextra -Wno-unused-parameter
CC ?= $(CROSSCOMPILER)gcc
MIX ?= mix

.PHONY: all elixir-code clean

all: elixir-code

elixir-code:
	$(MIX) compile

%.o: %.c
	$(CC) -c $(ERL_CFLAGS) $(CFLAGS) -o $@ $<

priv/ale: src/ale_main.o src/gpio_port.o src/i2c_port.o src/spi_port.o src/erlcmd.o
	@mkdir -p priv
	$(CC) $^ $(ERL_LDFLAGS) $(LDFLAGS) -o $@

clean:
	$(MIX) clean
	rm -f priv/ale src/*.o

realclean:
	rm -fr _build priv/ale src/*.o
