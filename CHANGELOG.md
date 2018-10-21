# Changelog

## v1.2.0

* New features
  * Add `GPIO.pin/1` to get the pin number back. Thanks to @jjcarstens
  * Add `child_specs/1` to support creating GPIOs in supervisors

* Improvements
  * Various spec improvements

## v1.1.0

This release will work with Elixir 1.7. Support for Elixir versions before 1.6 has been dropped.

* Improvements
  * Add `:start_value` to initialize a GPIO. Thanks to @jjcarstens for this
    feature.

## v1.0.3

* Bug fixes
  * Remove catchall receive handlers that would capture GenServer.call
    messages when functions were called by multiple threads
  * Timeout all port calls just in case. This hasn't been observed to my
    knowledge, but seems like a sensable precaution. The timeout is absurdly
    long (500 ms) so it shouldn't trigger.
  * Pass errors detected by C code to Elixir rather than crashing

## v1.0.2

* Bug fixes
  * Fix memory overrun when using large I2C transfers. This only affected a
    few devices since most devices maxed out at much smaller sizes. Also add
    compile-time check to prevent this from happening again.

* Improvements
  * Increase max SPI transfer size to 4096. This aligns it with py-spidev and
    enables communication with the Unicorn Hat HD.

## v1.0.1

* Bug fixes
  * Fixed typespec for `set_int/2`. Thanks to tmecklem for catching the
    ommission.

* Improvements
  * Various documentation and code formatting updates

## v1.0.0

* Same as v0.7.0.

## v0.7.0

* New features
  * Added and improved helper functions for finding I2C devices and I2C and
    SPI buses.

## v0.6.2

* Bug fixes
  * Fix message buffer size that was too small for really large I2C transfers
  * Many documentation improvements from Axel Clark.

## v0.6.1

* Bug fixes
  * Enlarge I2C buffer size to support some OLED displays
  * Support compilation on non-Linux platforms. It won't work, but this makes
    it possible to have unconditional deps on elixir_ale in projects.

## v0.6.0

*Backwards incompatible changes*

This release cleans up the naming of every module. All modules are now
in the `ElixirALE` namespace and capitalized since they're acronyms.
For example, if you were using the `I2c` module, it's now `ElixirALE.I2C`.
You may want to `alias ElixirALE.I2C`.

## v0.5.7

* Bug fixes
  * Clean up warnings especially the Elixir 1.4 bare function ones

## v0.5.6

* Bug fixes
  * I2C transfers may now be up to 512 bytes. Thanks to bendiken for this fix.
    See https://github.com/fhunleth/elixir_ale/pull/21.

## v0.5.5

* New features
  * `true` and `false` can now be passed to `Gpio.write/2`

* Bug fixes
  * Include i2c-dev.h to avoid incompatible version on Raspbian 8. This also
    avoids errors on systems that don't have the header file for whatever
    reason.
  * Include asm/ioctl.h for a MIPS platform that doesn't include it
    automatically.

## v0.5.4

* Bump version of elixir_make to workaround OTP 19 ports issue

## v0.5.3

* Use elixir_make instead of custom mix task

## v0.5.2

* Fix typo in v0.5.1

## v0.5.1

* Bug fixes
  * Fixed race condition when calling `Gpio.read/1` with interrupts
    enabled

## v0.5.0

* New features
  * Add `I2c.detect_devices/1` to scan the I2C bus
  * Add `I2c.read_device/3`, etc. to support devices on more than one I2C
    address without needing to create multiple I2c servers

## v0.4.1

* Bug fixes
  * Previous fix for `priv` directory in v0.4.0 was insufficient. This
    release has what is believe to be the correct fix.

## v0.4.0

* Bug fixes
  * Fix issue where `priv` directory symlink wasn't created with `mix`
  * Add support for retries when bringing up GPIO interfaces. This should be a
    permanent fix for the export/open race condition on Raspberry Pi 2s.

## v0.3.0

* New features
  * Use I2C RDWR ioctl so that write/read combinations are atomic. See
    I2c.write_read/3.

## v0.2.0

Initial release to hex.
