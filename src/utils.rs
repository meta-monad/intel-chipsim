use crate::i8008::*;

pub struct MemoryController {
    pub memory: [u8; 16384], // 16K memory
}

impl MemoryController {
    pub fn new() -> Self {
        Self {
            memory: [0 as u8; 16384],
        }
    }

    pub fn load_into(&mut self, start: usize, data: &[u8]) {
        let mut i = start;
        for value in data.into_iter() {
            self.memory[i] = *value;
            i += 1;
        }
    }

    pub fn get_value(&self, address: u16) -> u8 {
        self.memory[u14_to_u16(address) as usize]
    }
}
