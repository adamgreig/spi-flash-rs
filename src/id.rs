/// Store the ID read off an SPI flash memory.
///
/// The manufacturer ID and (long, 16-bit) device ID are read using the 0x9F command,
/// and the number of 0x7F continuation code bytes present before the manufacturer ID
/// is stored as `manufacturer_bank`.
///
/// The 64-bit unique ID is read using the 0x4B command.
#[derive(Copy, Clone, Debug)]
pub struct FlashID {
    pub manufacturer_bank: u8,
    pub manufacturer_id: u8,
    pub device_id_long: u16,
    pub device_id_short: u8,
    pub unique_id: u64,
}

impl FlashID {
    /// Look up a manufacturer name from the JEDEC ID.
    pub fn manufacturer_name(&self) -> Option<&'static str> {
        let (bank, id) = (self.manufacturer_bank, self.manufacturer_id & 0x7F);
        match jep106::JEP106Code::new(bank, id).get() {
            // Winbond acquired NEXCOM and so the ID 0xEF is commonly used for Winbond memory.
            Some(mfn) if mfn == "NEXCOM" => Some("Winbond/NEXCOM"),
            // GigaDevice flash doesn't use a continuation code, so 0xC8 appears as Apple Computer.
            Some(mfn) if mfn == "Apple Computer" => Some("Apple Computer/GigaDevice Semiconductor"),
            Some(mfn) => Some(mfn),
            None => None,
        }
    }
}

impl std::fmt::Display for FlashID {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mfn = match self.manufacturer_name() {
            Some(mfn) => format!(" ({})", mfn),
            None => "".to_string(),
        };
        let unique_id = match self.unique_id {
            0x0000_0000_0000_0000 | 0xFFFF_FFFF_FFFF_FFFF => "".to_string(),
            id => format!(", Unique ID: {:016X}", id),
        };
        write!(f, "Manufacturer 0x{:02X}{}, Device 0x{:02X}/0x{:04X}{}",
               self.manufacturer_id, mfn, self.device_id_short,
               self.device_id_long, unique_id)
    }
}
