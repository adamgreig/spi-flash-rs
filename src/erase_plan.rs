use alloc::vec::Vec;
use core::time::Duration;

/// Erase plan of (opcode, size, base address, typical duration) to erase a range of memory.
#[derive(Clone, Debug)]
pub(crate) struct ErasePlan(pub Vec<(u8, usize, u32, Option<Duration>)>);

impl ErasePlan {
    pub fn new(insts: &[(usize, u8, Option<Duration>)], start: usize, length: usize) -> Self {
        log::trace!("Creating erase plan, start={} length={}", start, length);
        let mut plan = Vec::new();

        // Sort instructions by smallest area of effect first.
        let mut insts = insts.to_vec();
        insts.sort();

        // We compute the number of useful bytes erased for each operation,
        // then from those with the same maximum number of useful bytes erased,
        // we select the smallest operation, and repeat until all bytes are erased.
        let end = start + length;
        let mut pos = start;
        while pos < end {
            log::trace!("Evaluating candidates, pos={} end={}", pos, end);
            // Current candidate, (bytes, size, opcode, base).
            let mut candidate = (0, usize::MAX, 0, 0, None);
            for (erase_size, opcode, duration) in insts.iter() {
                let erase_base = pos - (pos % erase_size);
                let erase_end = erase_base + erase_size - 1;
                let mut bytes = erase_size - (pos - erase_base);
                if erase_end > end {
                    bytes -= erase_end - end + 1;
                }
                log::trace!(
                    "  Candidate 0x{:02X} ({} bytes): base={} end={} bytes={}",
                    opcode,
                    erase_size,
                    erase_base,
                    erase_end,
                    bytes
                );
                if bytes > candidate.0 || (bytes == candidate.0 && *erase_size < candidate.1) {
                    candidate = (bytes, *erase_size, *opcode, erase_base, *duration);
                }
            }

            log::trace!("Candidate selected: {:?}", candidate);
            pos += candidate.0;
            plan.push((candidate.2, candidate.1, candidate.3 as u32, candidate.4));
        }

        log::debug!("Erase plan: {:?}", plan);

        ErasePlan(plan)
    }

    #[cfg(feature = "std")]
    pub fn total_size(&self) -> usize {
        self.0.iter().map(|x| x.1).sum()
    }
}

#[test]
fn test_erase_plan() {
    let insts = &[(4, 1, None), (32, 2, None), (64, 3, None)];
    // Use a single 4kB erase to erase an aligned 4kB block.
    assert_eq!(ErasePlan::new(insts, 0, 4).0, alloc::vec![(1, 4, 0, None)]);
    // Use a single 64kB erase to erase an aligned 64kB block.
    assert_eq!(
        ErasePlan::new(insts, 0, 64).0,
        alloc::vec![(3, 64, 0, None)]
    );
    // Use three 64kB erases to erase an aligned 192kB block.
    assert_eq!(
        ErasePlan::new(insts, 0, 192).0,
        alloc::vec![(3, 64, 0, None), (3, 64, 64, None), (3, 64, 128, None)]
    );
    // Use 64kB followed by 32kB to erase an aligned 70kB block.
    assert_eq!(
        ErasePlan::new(insts, 0, 70).0,
        alloc::vec![(3, 64, 0, None), (2, 32, 64, None)]
    );
    // Use 64kB followed by 4kB to erase an aligned 66kB block.
    assert_eq!(
        ErasePlan::new(insts, 0, 66).0,
        alloc::vec![(3, 64, 0, None), (1, 4, 64, None)]
    );
    // Use 4kB followed by 64kB to erase a misaligned 64kB block.
    assert_eq!(
        ErasePlan::new(insts, 62, 64).0,
        alloc::vec![(1, 4, 60, None), (3, 64, 64, None)]
    );
    // Use a 4kB, 64kB, 4kB to erase a misaligned 68kB block.
    assert_eq!(
        ErasePlan::new(insts, 62, 68).0,
        alloc::vec![(1, 4, 60, None), (3, 64, 64, None), (1, 4, 128, None)]
    );
}
