/// Status Register 1
#[derive(Copy, Clone, Debug)]
pub struct StatusRegister1(pub u8);

impl StatusRegister1 {
    /// Get BUSY bit.
    pub fn get_busy(&self) -> bool {
        self.0 & 0b0000_0001 != 0
    }

    /// Get (BP0, BP1, BP2) bits.
    pub fn get_block_protect(&self) -> (bool, bool, bool) {
        let bp = (self.0 & 0b0001_1100) >> 2;
        (bp & 0b001 != 0, bp & 0b010 != 0, bp & 0b100 != 0)
    }

    /// Set (BP0, BP1, BP2) bits.
    pub fn set_block_protect(&mut self, bp0: bool, bp1: bool, bp2: bool) {
        self.0 &= 0b1110_0011;
        self.0 |= ((bp0 as u8) << 2) | ((bp1 as u8) << 3) | ((bp2 as u8) << 4);
    }

    /// Get SEC (sector protect) bit.
    pub fn get_sec(&self) -> bool {
        self.0 & 0b0100_0000 != 0
    }

    /// Get TB (top/bottom protection) bit.
    pub fn get_tb(&self) -> bool {
        self.0 & 0b0010_0000 != 0
    }

    /// Get SRP (status register protect) bit.
    pub fn get_srp(&self) -> bool {
        self.0 & 0b1000_0000 != 0
    }
}

/// Status Register 2
#[derive(Copy, Clone, Debug)]
pub struct StatusRegister2(pub u8);

impl StatusRegister2 {
    /// Get CMP (protection complement) bit.
    pub fn get_cmp(&self) -> bool {
        self.0 & 0b0100_0000 != 0
    }
}

/// Status Register 3
#[derive(Copy, Clone, Debug)]
pub struct StatusRegister3(pub u8);

impl StatusRegister3 {
    /// Get WPS (write protect selection) bit.
    ///
    /// This bit is non-standard and its functionality
    /// depends on the specific flash chip.
    pub fn get_wps(&self) -> bool {
        self.0 & 0b0000_0100 != 0
    }

    /// Set WPS (write protect selection) bit.
    ///
    /// This bit is non-standard and its functionality
    /// depends on the specific flash chip.
    pub fn set_wps(&mut self, wps: bool) {
        self.0 &= 0b1111_1011;
        self.0 |= (wps as u8) << 2;
    }
}
