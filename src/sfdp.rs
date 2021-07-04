use crate::{Error, Result};
use alloc::vec::Vec;

#[derive(Clone, Debug)]
pub(crate) struct SFDPHeader {
    pub nph: usize,
    pub major: u8,
    pub minor: u8,
    pub params: Vec<SFDPParameterHeader>,
}

impl SFDPHeader {
    pub fn from_bytes(data: &[u8]) -> Result<Self> {
        log::debug!("Parsing SFDP header from data: {:X?}", data);
        if &data[0..4] != b"SFDP" {
            log::error!("Did not read expected SFDP signature");
            Err(Error::InvalidSFDPHeader)
        } else if data[7] != 0xFF {
            log::error!("Unsupported SFDP access protocol {:02X}", data[7]);
            Err(Error::InvalidSFDPHeader)
        } else {
            let minor = data[4];
            let major = data[5];
            let nph = data[6] as usize + 1;
            log::debug!("Read SFDP header, NPH={} MAJOR={} MINOR={}", nph, major, minor);
            if data.len() < (nph + 1) * 8 {
                log::error!(
                    "Did not read enough SFDP bytes: got {}, needed {}",
                    data.len(),
                    (nph + 1) * 8
                );
                Err(Error::InvalidSFDPHeader)
            } else {
                let params = data[8..].chunks(8).map(SFDPParameterHeader::from_bytes).collect();
                Ok(SFDPHeader { nph, major, minor, params })
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct SFDPParameterHeader {
    pub plen: usize,
    pub major: u8,
    pub minor: u8,
    pub parameter_id: u16,
    pub ptp: u32,
}

impl SFDPParameterHeader {
    fn from_bytes(data: &[u8]) -> Self {
        log::debug!("Reading SFDP parameter header from: {:X?}", data);
        let parameter_id = u16::from_be_bytes([data[7], data[0]]);
        let minor = data[1];
        let major = data[2];
        let plen = data[3] as usize;
        let ptp = u32::from_be_bytes([0, data[6], data[5], data[4]]);
        log::debug!(
            "Read JEDEC parameter header, plen={} major={} minor={} \
                     ID=0x{:04X} PTP=0x{:06X}",
            plen,
            major,
            minor,
            parameter_id,
            ptp
        );
        SFDPParameterHeader { plen, major, minor, parameter_id, ptp }
    }
}

/// SFDP JEDEC Basic Flash Parameter Table
///
/// This table contains standard SFDP information which may be
/// read from a flash memory. Only fields relevant to single I/O
/// operation are parsed.
///
/// Fields are taken from JESD216D-01, supporting parameter versions up to 1.7.
#[derive(Copy, Clone, Debug)]
pub struct FlashParams {
    /// Parameter header major version field.
    pub version_major: u8,
    /// Parameter header minor version field.
    pub version_minor: u8,

    /// Number of address bytes to use in read/write commands.
    pub address_bytes: SFDPAddressBytes,
    /// Flash memory density in bits.
    pub density: u64,

    /// If true, 4kB erase is supported.
    /// Newer memories indicate all erase sizes with `erase_*` fields.
    pub legacy_4kb_erase_supported: bool,
    /// Instruction for 4kB erase, or 0xFF if unsupported.
    /// Newer memories also include this instruction in `erase_*` fields.
    pub legacy_4kb_erase_inst: u8,
    /// Write enable instruction for volatile status register, either 0x50 or 0x06.
    /// Newer memories use `status_1_vol` instead.
    pub legacy_volatile_write_en_inst: u8,
    /// If true, Block Protect bits in status register are only volatile,
    /// otherwise they may be only non-volatile or may be programmed either
    /// as volatile with instruction 0x50 or non-volatile with instruction 0x06.
    /// Newer memories use `status_1_vol` instead.
    pub legacy_block_protect_volatile: bool,
    /// If true, writes can be performed with byte granularity.
    /// Newer memories use `page_size`.
    pub legacy_byte_write_granularity: bool,

    /// Erase instructions.
    ///
    /// Up to four erase instructions may be available,
    /// each specifying the opcode for the instruction
    /// and the number of bytes erased.
    pub erase_insts: [Option<SFDPEraseInst>; 4],

    /// Chip erase and programming times, if available.
    pub timing: Option<SFDPTiming>,

    /// Page size, in bytes.
    pub page_size: Option<u32>,

    // Omitted: Suspend/Resume support and instructions.

    // Omitted: Deep powerdown support and instructions.
    /// If true, polling busy status via the flag status register is supported.
    /// Instruction 0x70 reads the flag register, where bit 7 is 0 if busy and 1 if ready.
    pub busy_poll_flag: Option<bool>,
    /// If true, polling busy status via the status register is supported.
    /// Instruction 0x05 reads the status register, where bit 0 is 0 if ready and 1 if busy.
    pub busy_poll_status: Option<bool>,

    // Omitted: instructions for entering/exiting 4-byte address mode.
    /// If true, the device may be reset using instruction 0xF0.
    pub reset_inst_f0: Option<bool>,
    /// If true, the device may be reset using instruction 0x66 followed by 0x99.
    pub reset_inst_66_99: Option<bool>,

    /// Status register 1 volatility and write-enable instruction.
    pub status_1_vol: Option<SFDPStatus1Volatility>,
}

/// SFDP Address Bytes field.
#[derive(Copy, Clone, Debug)]
pub enum SFDPAddressBytes {
    /// Three-byte only addressing.
    Three,
    /// Three- or four-byte addressing; default is three-byte
    /// but may be configured for four-byte.
    ThreeOrFour,
    /// Four-byte only addressing.
    Four,
    /// Reserved as of JESD216D-01, JEDEC Basic Flash Parameters version 1.7.
    Reserved,
}

impl SFDPAddressBytes {
    fn from_bits(bits: u32) -> Self {
        match bits {
            0b00 => SFDPAddressBytes::Three,
            0b01 => SFDPAddressBytes::ThreeOrFour,
            0b10 => SFDPAddressBytes::Four,
            _ => SFDPAddressBytes::Reserved,
        }
    }
}

/// SFDP Erase Instruction.
#[derive(Copy, Clone, Debug)]
pub struct SFDPEraseInst {
    /// Opcode for erase instruction.
    pub opcode: u8,
    /// Size in bytes of erase instruction.
    pub size: u32,
    /// Typical erase time, if known.
    pub time_typ_ms: Option<u32>,
    /// Maximum erase time, if known.
    pub time_max_ms: Option<u32>,
}

impl core::fmt::Display for SFDPEraseInst {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "Opcode 0x{:02X}: {} bytes", self.opcode, self.size)?;
        if let Some(typ) = self.time_typ_ms {
            write!(f, ", typ {:?}", typ)?;
        }
        if let Some(max) = self.time_max_ms {
            write!(f, ", max {:?}", max)?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug)]
pub enum SFDPStatus1Volatility {
    /// Status register 1 is non-volatile, powers up to its last state, write-enable with 0x06.
    NonVolatile06,
    /// Status register 1 is volatile, powers up to all '1', write-enable with 0x06.
    Volatile06,
    /// Status register 1 is volatile, powers up to all '1', write-enable with 0x50.
    Volatile50,
    /// Status register 1 powers up to its last non-volatile state, use 0x06
    /// to write to non-volatile register, or use 0x50 to active and write
    /// volatile register.
    NonVolatile06Volatile50,
    /// Status register 1 contains a mix of volatile and non-volatile bits.
    /// Use instruction 0x06 to write.
    Mixed06,
    /// Reserved volatility mode.
    Reserved,
}

impl SFDPStatus1Volatility {
    pub fn from_bits(bits: u32) -> Self {
        if bits & 0b000_0001 != 0 {
            SFDPStatus1Volatility::NonVolatile06
        } else if bits & 0b000_0010 != 0 {
            SFDPStatus1Volatility::Volatile06
        } else if bits & 0b000_0100 != 0 {
            SFDPStatus1Volatility::Volatile50
        } else if bits & 0b000_1000 != 0 {
            SFDPStatus1Volatility::NonVolatile06Volatile50
        } else if bits & 0b001_0000 != 0 {
            SFDPStatus1Volatility::Mixed06
        } else {
            SFDPStatus1Volatility::Reserved
        }
    }
}

/// Struct of timing information from JESD216A-compliant tables.
///
/// Note that erase instruction timing is stored inside the respective erase instructions.
#[derive(Copy, Clone, Debug)]
pub struct SFDPTiming {
    /// Typical time to erase the entire chip, if known.
    pub chip_erase_time_typ_ms: u32,
    /// Maximum time to erase the entire chip, if known.
    pub chip_erase_time_max_ms: u32,
    /// Typical time to program the first byte in a sequence, if known.
    pub first_byte_prog_time_typ_ms: u32,
    /// Maximum time to program the first byte in a sequence, if known.
    pub first_byte_prog_time_max_ms: u32,
    /// Typical time to program each successive byte in a sequence, if known.
    pub succ_byte_prog_time_typ_ms: u32,
    /// Maximum time to program each successive byte in a sequence, if known.
    pub succ_byte_prog_time_max_ms: u32,
    /// Typical time to program a full page, if known.
    pub page_prog_time_typ_ms: u32,
    /// Maximum time to program a full page, if known.
    pub page_prog_time_max_ms: u32,
}

/// Bitfield extraction helper macro.
///
/// `bits!(word, length, offset)` extracts `length` number of bits at offset `offset`.
macro_rules! bits {
    ($d:expr, $n:expr, $o:expr) => {
        ($d & (((1 << $n) - 1) << $o)) >> $o
    };
}

impl FlashParams {
    pub fn from_bytes(major: u8, minor: u8, data: &[u8]) -> Result<Self> {
        log::debug!("Reading SFDP JEDEC Basic Flash Parameters from: {:X?}", data);

        // Check we have enough data.
        if data.len() % 4 != 0 {
            log::error!("SFPD data is not a multiple of 4 bytes.");
            return Err(Error::InvalidSFDPParams);
        } else if data.len() < 9 * 4 {
            log::error!("SFPD data is not long enough for version >= 1.0.");
            return Err(Error::InvalidSFDPParams);
        } else if major != 1 {
            log::error!("Only SFPD major version 1 is supported.");
            return Err(Error::InvalidSFDPParams);
        } else if minor > 5 && data.len() < 16 * 4 {
            log::error!("SFPD data is not long enough for version >= 1.5.");
            return Err(Error::InvalidSFDPParams);
        }

        // Convert the bytes into "DWORD"s (u32) for easier reference to the specification.
        let mut dwords = Vec::new();
        for bytes in data.chunks(4) {
            dwords.push(u32::from_be_bytes([bytes[3], bytes[2], bytes[1], bytes[0]]));
        }

        // Parse the first 9 DWORDs, which must always be available if SFDP is supported.
        let mut params = Self::read_jesd216(major, minor, &dwords);

        // 1.5: JESD216A adds DWORDs 10-16.
        if minor >= 5 {
            params.read_jesd216a(&dwords);
        }

        // 1.6: JESD216B adds quad-SPI information to DWORD15 bits 8, 14, and 18.
        // 1.7: JESD216C adds DWORDs 17-20 which describe octal SPI.
        // 1.8: JESD216D doesn't change the basic flash parameters table.

        Ok(params)
    }

    /// Get the flash capacity in bytes.
    pub fn capacity_bytes(&self) -> usize {
        (self.density / 8) as usize
    }

    /// Get the smallest erase granularity and its opcode.
    pub fn sector_erase(&self) -> Option<(usize, u8)> {
        let mut size = u32::MAX;
        let mut opcode = 0u8;
        for inst in self.erase_insts.iter().flatten() {
            if inst.size < size {
                size = inst.size;
                opcode = inst.opcode;
            }
        }

        if size != u32::MAX {
            Some((size as usize, opcode))
        } else {
            None
        }
    }

    /// Read the legacy information from JESD216 (DWORDs 1-9) and create a new FlashParams object.
    fn read_jesd216(major: u8, minor: u8, dwords: &[u32]) -> FlashParams {
        // 1st DWORD
        let address_bytes = SFDPAddressBytes::from_bits(bits!(dwords[0], 2, 17));
        let legacy_4kb_erase_inst = bits!(dwords[0], 8, 8) as u8;
        let legacy_volatile_write_en_inst = match bits!(dwords[0], 1, 4) {
            0 => 0x50,
            1 => 0x06,
            _ => unreachable!(),
        };
        let legacy_block_protect_volatile = bits!(dwords[0], 1, 3) == 1;
        let legacy_byte_write_granularity = bits!(dwords[0], 1, 2) == 1;
        let legacy_4kb_erase_supported = bits!(dwords[0], 2, 0) == 0b01;

        // 2nd DWORD
        let density = if dwords[1] >> 31 == 0 {
            (dwords[1] as u64) + 1
        } else {
            1u64 << (dwords[1] & 0x7FFF_FFFF)
        };

        // DWORDS 3,4, 5, 6, and 7 relate to multiple I/O and are skipped.

        // 8th and 9th DWORD
        let mut erase_insts = [None; 4];
        let erase_size_1 = bits!(dwords[7], 8, 0);
        let erase_size_2 = bits!(dwords[7], 8, 16);
        let erase_size_3 = bits!(dwords[8], 8, 0);
        let erase_size_4 = bits!(dwords[8], 8, 16);
        if erase_size_1 != 0 {
            let opcode = bits!(dwords[7], 8, 8) as u8;
            if opcode != 0 {
                erase_insts[0] = Some(SFDPEraseInst {
                    opcode,
                    size: 1 << erase_size_1,
                    time_typ_ms: None,
                    time_max_ms: None,
                });
            }
        }
        if erase_size_2 != 0 {
            let opcode = bits!(dwords[7], 8, 24) as u8;
            if opcode != 0 {
                erase_insts[1] = Some(SFDPEraseInst {
                    opcode,
                    size: 1 << erase_size_2,
                    time_typ_ms: None,
                    time_max_ms: None,
                });
            }
        }
        if erase_size_3 != 0 {
            let opcode = bits!(dwords[8], 8, 8) as u8;
            if opcode != 0 {
                erase_insts[2] = Some(SFDPEraseInst {
                    opcode,
                    size: 1 << erase_size_3,
                    time_typ_ms: None,
                    time_max_ms: None,
                });
            }
        }
        if erase_size_4 != 0 {
            let opcode = bits!(dwords[8], 8, 24) as u8;
            if opcode != 0 {
                erase_insts[3] = Some(SFDPEraseInst {
                    opcode,
                    size: 1 << erase_size_4,
                    time_typ_ms: None,
                    time_max_ms: None,
                });
            }
        }

        // Return a FlashParams with the legacy information set and further information
        // cleared, which can be filled in if additional DWORDs are available.
        FlashParams {
            version_major: major,
            version_minor: minor,
            address_bytes,
            density,
            legacy_4kb_erase_supported,
            legacy_4kb_erase_inst,
            legacy_volatile_write_en_inst,
            legacy_block_protect_volatile,
            legacy_byte_write_granularity,
            erase_insts,
            timing: None,
            page_size: None,
            busy_poll_flag: None,
            busy_poll_status: None,
            reset_inst_f0: None,
            reset_inst_66_99: None,
            status_1_vol: None,
        }
    }

    /// Parse JESD216A DWORDs 10 to 16.
    fn read_jesd216a(&mut self, dwords: &[u32]) {
        // 10th DWORD: erase instruction timings.
        let erase_scale = bits!(dwords[9], 4, 0);
        if let Some(inst) = self.erase_insts[0].as_mut() {
            let typ = bits!(dwords[9], 7, 4);
            let (typ, max) = Self::sector_erase_durations(typ, erase_scale);
            inst.time_typ_ms = Some(typ);
            inst.time_max_ms = Some(max);
        }
        if let Some(inst) = self.erase_insts[1].as_mut() {
            let typ = bits!(dwords[9], 7, 11);
            let (typ, max) = Self::sector_erase_durations(typ, erase_scale);
            inst.time_typ_ms = Some(typ);
            inst.time_max_ms = Some(max);
        }
        if let Some(inst) = self.erase_insts[2].as_mut() {
            let typ = bits!(dwords[9], 7, 18);
            let (typ, max) = Self::sector_erase_durations(typ, erase_scale);
            inst.time_typ_ms = Some(typ);
            inst.time_max_ms = Some(max);
        }
        if let Some(inst) = self.erase_insts[3].as_mut() {
            let typ = bits!(dwords[9], 7, 25);
            let (typ, max) = Self::sector_erase_durations(typ, erase_scale);
            inst.time_typ_ms = Some(typ);
            inst.time_max_ms = Some(max);
        }

        // 11th DWORD: chip erase and programming timings, page size.
        let typ = bits!(dwords[10], 7, 24);
        let (chip_erase_time_typ, chip_erase_time_max) =
            Self::chip_erase_duration(typ, erase_scale);
        let program_scale = bits!(dwords[10], 4, 0);
        let typ = bits!(dwords[10], 5, 19);
        let (succ_byte_prog_time_typ, succ_byte_prog_time_max) =
            Self::byte_program_duration(typ, program_scale);
        let typ = bits!(dwords[10], 5, 14);
        let (first_byte_prog_time_typ, first_byte_prog_time_max) =
            Self::byte_program_duration(typ, program_scale);
        let typ = bits!(dwords[10], 6, 8);
        let (page_prog_time_typ, page_prog_time_max) =
            Self::page_program_duration(typ, program_scale);
        self.timing = Some(SFDPTiming {
            chip_erase_time_typ_ms: chip_erase_time_typ,
            chip_erase_time_max_ms: chip_erase_time_max,
            first_byte_prog_time_typ_ms: first_byte_prog_time_typ,
            first_byte_prog_time_max_ms: first_byte_prog_time_max,
            succ_byte_prog_time_typ_ms: succ_byte_prog_time_typ,
            succ_byte_prog_time_max_ms: succ_byte_prog_time_max,
            page_prog_time_typ_ms: page_prog_time_typ,
            page_prog_time_max_ms: page_prog_time_max,
        });
        self.page_size = Some(1 << bits!(dwords[10], 4, 4));

        // 12th and 13th DWORDs skipped: suspend/resume.

        // 14th DWORD
        let status_reg_poll = bits!(dwords[13], 6, 2);
        self.busy_poll_flag = Some((status_reg_poll & 0b00_0010) != 0);
        self.busy_poll_status = Some((status_reg_poll & 0b00_0001) != 0);

        // 15th DWORD skipped: multiple I/O.

        // 16th DWORD
        let reset = bits!(dwords[15], 6, 8);
        self.reset_inst_f0 = Some((reset & 0b00_1000) != 0);
        self.reset_inst_66_99 = Some((reset & 0b01_0000) != 0);
        let vol = bits!(dwords[15], 7, 0);
        self.status_1_vol = Some(SFDPStatus1Volatility::from_bits(vol));
    }

    /// Convert SFPD sector erase time to typical and maximum Duration.
    ///
    /// Uses scale factors of 1ms, 16ms, 128ms, and 1s.
    fn sector_erase_durations(typ: u32, max_scale: u32) -> (u32, u32) {
        let scale = match bits!(typ, 2, 5) {
            0b00 => 1,
            0b01 => 16,
            0b10 => 128,
            0b11 => 1000,
            _ => unreachable!(),
        };
        let count = bits!(typ, 5, 0);
        let typ = (count + 1) * scale;
        //TODO check for overflow?
        let max = 2 * (max_scale + 1) * typ;
        (typ, max)
    }

    /// Convert SFPD chip erase time to typical and maximum Durations.
    ///
    /// Uses scale factors of 16ms, 256ms, 4s, and 64s.
    fn chip_erase_duration(typ: u32, max_scale: u32) -> (u32, u32) {
        let scale = match bits!(typ, 2, 5) {
            0b00 => 16,
            0b01 => 256,
            0b10 => 4000,
            0b11 => 64000,
            _ => unreachable!(),
        };
        let count = bits!(typ, 5, 0);
        let typ = (count + 1) * scale;
        //TODO check for overflow?
        let max = 2 * (max_scale + 1) * typ;
        (typ, max)
    }

    /// Convert SFPD byte program times to typical and maximum Durations.
    ///
    /// Uses scale factors of 1µs and 8µs.
    fn byte_program_duration(typ: u32, max_scale: u32) -> (Duration, Duration) {
        let scale = match bits!(typ, 1, 4) {
            0b0 => Duration::from_micros(1),
            0b1 => Duration::from_micros(8),
            _ => unreachable!(),
        };
        let count = bits!(typ, 4, 0);
        let typ = (count + 1) * scale;
        //TODO check for overflow?
        let max = 2 * (max_scale + 1) * typ;
        (typ, max)
    }

    /// Convert SFPD page program times to typical and maximum Durations.
    ///
    /// Uses scale factors of 8µs and 64µs.
    fn page_program_duration(typ: u32, max_scale: u32) -> (Duration, Duration) {
        let scale = match bits!(typ, 1, 5) {
            0b0 => Duration::from_micros(8),
            0b1 => Duration::from_micros(64),
            _ => unreachable!(),
        };
        let count = bits!(typ, 5, 0);
        let typ = (count + 1) * scale;
        //TODO check for overflow?
        let max = 2 * (max_scale + 1) * typ;
        (typ, max)
    }
}

impl core::fmt::Display for FlashParams {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        writeln!(
            f,
            "SFDP JEDEC Basic Flash Parameter Table v{}.{}",
            self.version_major, self.version_minor
        )?;
        writeln!(f, "  Density: {} bits ({} KiB)", self.density, self.capacity_bytes() / 1024)?;
        writeln!(f, "  Address bytes: {:?}", self.address_bytes)?;
        writeln!(f, "  Legacy information:")?;
        writeln!(f, "    4kB erase supported: {}", self.legacy_4kb_erase_supported)?;
        writeln!(f, "    4kB erase opcode: 0x{:02X}", self.legacy_4kb_erase_inst)?;
        writeln!(f, "    Block Protect always volatile: {}", self.legacy_block_protect_volatile)?;
        writeln!(
            f,
            "    Volatile write enable opcode: 0x{:02X}",
            self.legacy_volatile_write_en_inst
        )?;
        writeln!(f, "    Writes have byte granularity: {}", self.legacy_byte_write_granularity)?;
        writeln!(f, "  Erase instructions:")?;
        for i in 0..4 {
            if let Some(inst) = self.erase_insts[i] {
                writeln!(f, "    {}: {}", i + 1, inst)?;
            } else {
                writeln!(f, "    {}: Not present", i + 1)?;
            }
        }
        if let Some(timing) = self.timing {
            writeln!(f, "  Timing:")?;
            writeln!(
                f,
                "    Chip erase: typ {:?}, max {:?}",
                timing.chip_erase_time_typ_ms, timing.chip_erase_time_max_ms
            )?;
            writeln!(
                f,
                "    First byte program: typ {:?}, max {:?}",
                timing.first_byte_prog_time_typ_ms, timing.first_byte_prog_time_max_ms
            )?;
            writeln!(
                f,
                "    Subsequent byte program: typ {:?}, max {:?}",
                timing.succ_byte_prog_time_typ_ms, timing.succ_byte_prog_time_max_ms
            )?;
            writeln!(
                f,
                "    Page program: typ {:?}, max {:?}",
                timing.page_prog_time_typ_ms, timing.page_prog_time_max_ms
            )?;
        }
        if let Some(page_size) = self.page_size {
            writeln!(f, "  Page size: {} bytes", page_size)?;
        }
        if let Some(busy_poll_flag) = self.busy_poll_flag {
            writeln!(f, "  Poll busy from FSR: {}", busy_poll_flag)?;
        }
        if let Some(busy_poll_status) = self.busy_poll_status {
            writeln!(f, "  Poll busy from SR1: {}", busy_poll_status)?;
        }
        if let Some(reset_inst_f0) = self.reset_inst_f0 {
            writeln!(f, "  Reset using opcode 0xF0: {}", reset_inst_f0)?;
        }
        if let Some(reset_inst_66_99) = self.reset_inst_66_99 {
            writeln!(f, "  Reset using opcodes 0x66, 0x99: {}", reset_inst_66_99)?;
        }
        if let Some(status_1_vol) = self.status_1_vol {
            writeln!(f, "  Status register 1 volatility: {:?}", status_1_vol)?;
        }
        Ok(())
    }
}
