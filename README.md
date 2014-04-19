# Elixir-ALE -- Elixir Actor Library for Embedded

Elixir/ALE provides high level abstractions for interfacing to hardware
peripherals on embedded platforms. If this sounds similar to
[Erlang/ALE](https://github.com/esl/erlang-ale), that's because it is. This
library is a Elixir-ized version of the library. It does differ from Erlang/ALE
in that the C port side has been simplified and all Raspberry PI-specific code
removed. It still runs on the Raspberry PI, but it interfaces to the hardware
only through Linux interfaces.

# Warning

This is a work in progress. Expect major changes to the API as my knowledge of
Elixir improves (likely, I hope) and as the Elixir language evolves (less
likely).

# Getting started

    git clone https://github.com/fhunleth/elixir-ale.git
    mix compile

# Example

    iex> Gpio.start_link(4, :output)
    iex> Gpio.write(4, 1)

# License

This library draws much of its design and code from the Erlang/ALE project which
is licensed under the Apache License, Version 2.0. As such, it is licensed
similarly.
