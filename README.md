# Elixir-ALE -- Elixir Actor Library for Embedded

Elixir/ALE provides high level abstractions for interfacing to hardware
peripherals on embedded platforms. If this sounds similar to
[Erlang/ALE](https://github.com/esl/erlang-ale), that's because it is. This
library is a Elixir-ized implementation of the library. It does differ from Erlang/ALE
in that the C port side has been simplified and all Raspberry PI-specific code
removed. It still runs on the Raspberry PI, but it interfaces to the hardware
only through Linux interfaces.

# Getting started

    git clone https://github.com/fhunleth/elixir-ale.git
    mix compile

# Example

    iex> {:ok, pid} = Gpio.start_link(4, :output)
    iex> Gpio.write(pid, 1)

# License

This library draws much of its design and code from the Erlang/ALE project which
is licensed under the Apache License, Version 2.0. As such, it is licensed
similarly.
