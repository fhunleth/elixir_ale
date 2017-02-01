# elixir_ale - Elixir Actor Library for Embedded

[![Build Status](https://travis-ci.org/fhunleth/elixir_ale.svg)](https://travis-ci.org/fhunleth/elixir_ale)
[![Hex version](https://img.shields.io/hexpm/v/elixir_ale.svg "Hex version")](https://hex.pm/packages/elixir_ale)

`elixir_ale` provides high level abstractions for interfacing to GPIOs, I2C
buses and SPI peripherals on Linux platforms. Internally, it uses the Linux
sysclass interface so that it does not require platform-dependent code.

`elixir_ale` works great with LEDs, buttons, many kinds of sensors, and simple
control of motors. In general, if a device requires high speed transactions or
has hard real-time constraints in its interactions, this is not the right
library. For those devices, it is recommended to look at other driver options, such
as using a Linux kernel driver.

If this sounds similar to [Erlang/ALE](https://github.com/esl/erlang-ale), that's because it is. This
library is a Elixir-ized implementation of the original project with some updates
to the C side. (Many of those changes have made it back to the original project
now.)

# Getting started

If you're natively compiling elixir_ale, everything should work like any other
Elixir library. Normally, you would include elixir_ale as a dependency in your
`mix.exs` like this:

    defp deps do
      [{:elixir_ale, "~> 0.5.7"}]
    end

If you just want to try it out, you can do the following:

    git clone https://github.com/fhunleth/elixir_ale.git
    cd elixir_ale
    mix compile
    iex -S mix

If you're cross-compiling, you'll need to setup your environment so that the
right C compiler is called. See the `Makefile` for the variables that will need
to be overridden. At a minimum, you will need to set `CROSSCOMPILE`,
`ERL_CFLAGS`, and `ERL_EI_LIBDIR`.

`elixir_ale` doesn't load device drivers, so you'll need to make sure that any
necessary ones for accessing I2C or SPI are loaded beforehand. On the Raspberry
Pi, the [Adafruit Raspberry Pi I2C
instructions](https://learn.adafruit.com/adafruits-raspberry-pi-lesson-4-gpio-setup/configuring-i2c)
may be helpful.

If you're trying to compile on a Raspberry Pi and you get errors indicated that Erlang headers are missing
(`ie.h`), you may need to install erlang with `apt-get install
erlang-dev` or build Erlang from source per instructions [here](http://elinux.org/Erlang).

# Examples

`elixir_ale` only supports simple uses of the GPIO, I2C, and SPI interfaces in
Linux, but you can still do quite a bit. The following examples were tested on a
Raspberry Pi that was connected to an [Erlang Embedded Demo
Board](http://solderpad.com/omerk/erlhwdemo/). There's nothing special about
either the demo board or the Raspberry Pi, so these should work similarly on
other embedded Linux platforms.

## GPIO

A GPIO is just a wire that you can use as an input or an output. It can only be
one of two values, 0 or 1. A 1 corresponds to a logic high voltage like 3.3 V
and a 0 corresponds to 0 V. The actual voltage depends on the hardware.

Here's an example setup:

![GPIO schematic](assets/images/schematic-gpio.png)

To turn on the LED that's connected to the net labeled
`PI_GPIO18`, you can run the following:

    iex> {:ok, pid} = Gpio.start_link(18, :output)
    {:ok, #PID<0.96.0>}

    iex> Gpio.write(pid, 1)
    :ok

Input works similarly:

    iex> {:ok, pid} = Gpio.start_link(17, :input)
    {:ok, #PID<0.97.0>}

    iex> Gpio.read(pid)
    0

    # Push the button down

    iex> Gpio.read(pid)
    1

If you'd like to get a message when the button is pressed or released, call the
`set_int` function. You can trigger on the `:rising` edge, `:falling` edge or
`:both`.

    iex> Gpio.set_int(pid, :both)
    :ok

    iex> flush
    {:gpio_interrupt, 17, :rising}
    {:gpio_interrupt, 17, :falling}
    :ok

Note that after calling `set_int`, the calling process will receive an initial message with the state of the pin.
This prevents the race condition between getting the initial state of the pin and turning on interrupts. Without it, you could get the state of the pin, it could change states, and then you could start waiting on it for interrupts. If that happened, you would be out of sync.

## SPI

A SPI bus is a common multi-wire bus used to connect components on a circuit
board. A clock line drives the timing of sending bits between components. Bits
on the `MOSI` line go from the master (usually the processor running Linux) to
the slave, and bits on the `MISO` line go the other direction. Bits transfer
both directions simultaneously. However, much of the time, the protocol used
across the SPI bus has a request followed by a response and in these cases, bits
going the "wrong" direction are ignored.

The following shows an example ADC that reads from either a temperature sensor
on CH0 or a potentiometer on CH1.

![SPI schematic](assets/images/schematic-adc.png)

The protocol for talking to the ADC is described in the [MCP3202
Datasheet](http://www.microchip.com/wwwproducts/Devices.aspx?dDocName=en010532).
Sending a 0x64 first reads the temperature and sending a 0x74 reads the
potentiometer.

    # Make sure that you've enabled or loaded the SPI driver or this will
    # fail.
    iex> {:ok, pid} = Spi.start_link("spidev0.0")
    {:ok, #PID<0.124.0>}

    # Read the potentiometer

    # Use binary pattern matching to pull out the ADC counts (low 12 bits)
    iex> <<_::size(4), counts::size(12)>> = Spi.transfer(pid, <<0x74, 0x00>>)
    <<1, 197>>

    iex> counts
    453

    # Convert counts to volts (1023 = 3.3 V)
    iex> volts = counts / 1023 * 3.3
    1.461290322580645

## I2C

An I2C bus is similar to a SPI bus in function, but uses fewer wires. It
supports addressing hardware components and bidirectional use of the data line.

The following shows a bus IO expander connected via I2C to the processor.

![I2C schematic](assets/images/schematic-i2c.png)

The protocol for talking to the IO expander is described in the [MCP23008
Datasheet](http://www.microchip.com/wwwproducts/Devices.aspx?product=MCP23008).
Here's a simple example of using it.

    # On the Raspberry Pi, the IO expander is connected to I2C bus 1 (i2c-1).
    # Its 7-bit address is 0x20. (see datasheet)
    iex> {:ok, pid} = I2c.start_link("i2c-1", 0x20)
    {:ok, #PID<0.102.0>}

    # By default, all 8 GPIOs are set to inputs. Set the 4 high bits to outputs
    # so that we can toggle the LEDs. (Write 0x0f to register 0x00)
    iex> I2c.write(pid, <<0x00, 0x0f>>
    :ok

    # Turn on the LED attached to bit 4 on the expander. (Write 0x10 to register
    # 0x09)
    iex> I2c.write(pid, <<0x09, 0x10>>)
    :ok

    # Read all 11 of the expander's registers to see that the bit 0 switch is
    # the only one on and that the bit 4 LED is on.
    iex> I2c.write(pid, <<0>>)  # Set the next register to be read to 0
    :ok

    iex> I2c.read(pid, 11)
    <<15, 0, 0, 0, 0, 0, 0, 0, 0, 17, 16>>

    # The operation of writing one or more bytes to select a register and
    # then reading is very common, so a shortcut is to just run the following:
    iex> I2c.write_read(pid, <<0>>, 11)
    <<15, 0, 0, 0, 0, 0, 0, 0, 0, 17, 16>>

    # The 17 in register 9 says that bits 0 and bit 4 are high
    # We could have just read register 9.

    iex> I2c.write_read(pid, <<9>>, 1)
    <<17>>

## FAQ

### Where can I get help?

Most issues people have are on how to communicate with hardware for the first
time. Since `elixir_ale` is a thin wrapper on the Linux sys class interface, you
may find help by searching for similar issues when using Python or C.

For help specifically with `elixir_ale`, you may also find help on the
nerves channel on the [elixir-lang Slack](https://elixir-slackin.herokuapp.com/).
Many [Nerves](http://nerves-project.org) users also use `elixir_ale`.

### Why isn't elixir_ale a NIF?

While `elixir_ale` should never crash, it's hard to guarantee that weird
conditions on the I2C or SPI buses wouldn't hang the Erlang VM. `elixir_ale`
errors on the side of safety of the VM.

### I tried turning on and off a GPIO as fast as I could. Why was it slow?

Please don't do that - there are so many better ways of addressing whatever
you're trying to do:

  1. If you're trying to drive a servo or dim an LED, look into PWM. Many
     platforms have PWM hardware and you won't tax your CPU at all. If your
     platform is missing a PWM, several chips are available that take I2C
     commands to drive a PWM output.
  2. If you need to implement a wire level protocol to talk to a device, look
     for a Linux kernel driver. It may just be a matter of loading the right
     kernel module.
  3. If you want a blinking LED to indicate status, `elixir_ale` really should
     be fast enough to do that, but check out Linux's LED class interface. Linux
     can flash LEDs, trigger off events and more. See [nerves_leds](https://github.com/nerves-project/nerves_leds).

If you're still intent on optimizing GPIO access, you may be interested in
[gpio_twiddler](https://github.com/fhunleth/gpio_twiddler).

### Where's PWM support?

On the hardware that I normally use, PWM has been implemented in a
platform-dependent way. For ease of maintenance, `elixir_ale` doesn't have any
platform-dependent code, so supporting it would be difficult. An Elixir PWM
library would be very interesting, though, should anyone want to implement it.

### Can I develop code that uses elixir_ale on my laptop?

You'll need to fake out the hardware. Code to do this depends
on what your hardware actually does, but here's one example:

  * http://www.cultivatehq.com/posts/compiling-and-testing-elixir-nerves-on-your-host-machine/

Please share other examples if you have them.

### How do I debug?

The most common issue is getting connected to a part the first time. If you're
having trouble, check that the device files exist in the `/dev` directory for I2C
and SPI. GPIOs are usually come up easier, but their corresponding files are in
`/sys/class/gpio`. Are ARM-based boards, it is common to need to specify a
device tree file to the Linux kernel that specifies whether pins on I2C, SPI, or
GPIOs. Some boards also support device tree overlays that can be installed at
run time to change the usage of pins (the BeagleBone Black is a good example of
this. See the [Universal I/O
project](https://github.com/cdsteinkuehler/beaglebone-universal-io). If
debugging I2C, see `I2c.detect_devices/1` for scanning the whole bus for
anything in case the device you're using is at a different address than
expected.

### Will it run on Arduino?

No. Elixir ALE only runs on Linux-based boards. If you're interested in controlling an Arduino from a computer that can run Elixir, check out [nerves_uart](https://hex.pm/packages/nerves_uart) for communicating via the Arduino's serial connection or [firmata](https://github.com/mobileoverlord/firmata) for communication using the Arduino's Firmata protocol.

### Can I help maintain elixir_ale?

Yes! If your life has been improved by `elixir_ale` and you want to give back,
it would be great to have new energy put into this project. Please email me.

# License

This library draws much of its design and code from the Erlang/ALE project which
is licensed under the Apache License, Version 2.0. As such, it is licensed
similarly.
