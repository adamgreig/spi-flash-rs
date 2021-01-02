# spi-flash

SPI flash interface crate. Provides basic flash operations such as reading,
erasing, and writing, and can read and parse SFDP parameter tables to
automatically support many flash chips.

Currently only supports std (hosted) operation; no_std support may come in the
future.
