# Changelog

## [Unreleased]

## [v0.3.0] - 2022-07-31

* Make `spi-flash` `no-std` compatible (still requires `alloc`).
* The `FlashAccess` trait now has an associated `Error` type and its methods
  return corresponding `Result`s; `From<FlashAccess::Error>` must now be
  implemented for `spi_flash::Error`.
* Change progress bar style

## [v0.2.2] - 2021-01-20

* Fix reading v1.0 SFDP table.
* Assume a default page size of 256 bytes if not specified manually nor
  discovered via SFDP data.
* Fix programming when some data before the start address is already erased
  and the start address is not aligned with the erase instruction.

## [v0.2.1] - 2021-01-20

* Fix reading SFDP tables from devices with more than one parameter header.

## [v0.2.0] - 2021-01-02

* Flash write speed performance improvement.
* Erase planning improvement.
* Extra information added to ReadbackError and InvalidAddress.

## [v0.1.0] - 2021-01-01

* Initial release.

[Unreleased]: https://github.com/adamgreig/spi-flash-rs/compare/v0.3.0...HEAD
[v0.3.0]: https://github.com/adamgreig/spi-flash-rs/compare/v0.2.2...v0.3.0
[v0.2.2]: https://github.com/adamgreig/spi-flash-rs/compare/v0.2.1...v0.2.2
[v0.2.1]: https://github.com/adamgreig/spi-flash-rs/compare/v0.2.0...v0.2.1
[v0.2.0]: https://github.com/adamgreig/spi-flash-rs/compare/v0.1.0...v0.2.0
[v0.1.0]: https://github.com/adamgreig/spi-flash-rs/tree/v0.1.0
