# Changelog

## v0.6.0-dev

*Backwards incompatible changes*

This release modernizes the naming of every module. All modules have been
placed in the `ElixirALE` namespace and capitalized since they're acronyms.
For example, if you were using the `I2c` module, you'll need to `alias ElixirALE.I2C`
and then search and replace `I2c` to `I2C`. The other modules are similar.

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
