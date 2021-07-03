# spi-flash

[![crates.io](https://img.shields.io/crates/v/spi-flash.svg)](https://crates.io/crates/spi-flash)
[![docs.rs](https://docs.rs/spi-flash/badge.svg)](https://docs.rs/spi-flash)
![CI](https://github.com/adamgreig/spi-flash-rs/workflows/CI/badge.svg)

SPI flash interface crate. Provides basic flash operations such as reading,
erasing, and writing, and can read and parse SFDP parameter tables to
automatically support many flash chips.

By default this crate requires `std`, but by disabling the default `std`
feature, only no-std-compatible operations are exposed. Currently `alloc`
is always required.

## Licence

spi-flash is licensed under either of

* Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
  http://www.apache.org/licenses/LICENSE-2.0)
* MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.
