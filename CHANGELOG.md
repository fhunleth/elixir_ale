# Changelog

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
