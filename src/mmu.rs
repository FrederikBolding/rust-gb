// Memory Layout
// 0000-00FF - BIOS
// 0100-014F - Cartridge header
// 014F-4000 - ROM bank 0
// 4000-7FFF - ROM bank N
// 8000-9FFF - Graphics RAM
// A000-BFFF - External RAM
// C000-DFFF - Working RAM
// E000-FDFF - Shadow working RAM
// FE00-FE9F - Sprite information
// FF00-FF7F - Memory-mapped IO
// FF80-FFFF - Zero-page RAM

use crate::gpu::GPU;

pub const BIOS_START: usize = 0x00;
pub const BIOS_END: usize = 0xFF;
pub const BIOS_SIZE: usize = BIOS_END - BIOS_START + 1;

pub const ROM_BANK_0_START: usize = 0x0000;
pub const ROM_BANK_0_END: usize = 0x3FFF;
pub const ROM_BANK_0_SIZE: usize = ROM_BANK_0_END - ROM_BANK_0_START + 1;

pub const ROM_BANK_N_START: usize = 0x4000;
pub const ROM_BANK_N_END: usize = 0x7FFF;
pub const ROM_BANK_N_SIZE: usize = ROM_BANK_N_END - ROM_BANK_N_START + 1;

pub const VRAM_START: usize = 0x8000;
pub const VRAM_END: usize = 0x9FFF;
pub const VRAM_SIZE: usize = VRAM_END - VRAM_START + 1;

pub const EXTERNAL_RAM_START: usize = 0xA000;
pub const EXTERNAL_RAM_END: usize = 0xBFFF;
pub const EXTERNAL_RAM_SIZE: usize = EXTERNAL_RAM_END - EXTERNAL_RAM_START + 1;

pub const WORKING_RAM_START: usize = 0xC000;
pub const WORKING_RAM_END: usize = 0xDFFF;
pub const WORKING_RAM_SIZE: usize = WORKING_RAM_END - WORKING_RAM_START + 1;

pub const SHADOW_WORKING_RAM_START: usize = 0xE000;
pub const SHADOW_WORKING_RAM_END: usize = 0xFDFF;

pub const SPRITE_INFO_START: usize = 0xFE00;
pub const SPRITE_INFO_END: usize = 0xFE9F;

pub const UNUSED_START: usize = 0xFEA0;
pub const UNUSED_END: usize = 0xFEFF;

pub const IO_START: usize = 0xFF00;
pub const IO_END: usize = 0xFF7F;

pub const ZERO_PAGE_START: usize = 0xFF80;
pub const ZERO_PAGE_END: usize = 0xFFFE;
pub const ZERO_PAGE_SIZE: usize = ZERO_PAGE_END - ZERO_PAGE_START + 1;

pub struct MMU {
    booting: bool,
    pub gpu: GPU,
    bios: [u8; BIOS_SIZE],
    rom_bank_0: [u8; ROM_BANK_0_SIZE],
    rom_bank_n: [u8; ROM_BANK_N_SIZE],
    external_ram: [u8; EXTERNAL_RAM_SIZE],
    working_ram: [u8; WORKING_RAM_SIZE],
    zero_page_ram: [u8; ZERO_PAGE_SIZE],
}

impl MMU {
    pub fn new() -> Self {
        Self {
            booting: true,
            gpu: GPU::new(),
            bios: [0; BIOS_SIZE],
            rom_bank_0: [0; ROM_BANK_0_SIZE],
            rom_bank_n: [0; ROM_BANK_N_SIZE],
            external_ram: [0; EXTERNAL_RAM_SIZE],
            working_ram: [0; WORKING_RAM_SIZE],
            zero_page_ram: [0; ZERO_PAGE_SIZE],
        }
    }

    pub fn step(&mut self, cycles: u8) {
        self.gpu.step(cycles);
    }

    pub fn finish_boot(&mut self) {
        self.booting = false;
        println!("Finished boot");
    }

    pub fn load_bios(&mut self, bios: Vec<u8>) {
        println!("Loading BIOS...");
        for i in 0..BIOS_SIZE {
            self.bios[i] = bios[i];
        }
    }

    pub fn load_rom(&mut self, rom: Vec<u8>) {
        println!("Loading ROM...");
        for i in 0..ROM_BANK_0_SIZE {
            self.rom_bank_0[i] = rom[i];
        }
        for i in 0..ROM_BANK_N_SIZE {
            self.rom_bank_n[i] = rom[ROM_BANK_0_SIZE + i];
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        let address = address as usize;
        //println!("Read 0x{:02x}", address);
        match address {
            BIOS_START..=BIOS_END => {
                if self.booting {
                    self.bios[address]
                } else {
                    self.rom_bank_0[address]
                }
            }
            ROM_BANK_0_START..=ROM_BANK_0_END => self.rom_bank_0[address],
            ROM_BANK_N_START..=ROM_BANK_N_END => self.rom_bank_n[address - ROM_BANK_N_START],
            VRAM_START..=VRAM_END => self.gpu.vram[address - VRAM_START],
            EXTERNAL_RAM_START..=EXTERNAL_RAM_END => {
                self.external_ram[address - EXTERNAL_RAM_START]
            }
            WORKING_RAM_START..=WORKING_RAM_END => self.working_ram[address - WORKING_RAM_START],
            // Mirrors working RAM but does not allow writing
            SHADOW_WORKING_RAM_START..=SHADOW_WORKING_RAM_END => {
                self.working_ram[address - SHADOW_WORKING_RAM_START]
            }
            IO_START..=IO_END => match address {
                0xFF42 => self.gpu.scroll_y,
                0xFF44 => self.gpu.line,
                _ => todo!(
                    "Tried to read from unimplemented IO register 0x{:02x}",
                    address
                ),
            },
            ZERO_PAGE_START..=ZERO_PAGE_END => self.zero_page_ram[address - ZERO_PAGE_START],
            _ => {
                panic!("Failed to read memory at 0x{:02x}", address);
            }
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        let address = address as usize;
        //println!("Write 0x{:02x} 0x{:02x}", address, value);
        match address {
            ROM_BANK_0_START..=ROM_BANK_0_END => {
                self.rom_bank_0[address] = value;
            }
            ROM_BANK_N_START..=ROM_BANK_N_END => {
                self.rom_bank_n[address - ROM_BANK_N_START] = value;
            }
            VRAM_START..=VRAM_END => {
                self.gpu.write_vram(address - VRAM_START, value);
            }
            EXTERNAL_RAM_START..=EXTERNAL_RAM_END => {
                self.external_ram[address - EXTERNAL_RAM_START] = value;
            }
            WORKING_RAM_START..=WORKING_RAM_END => {
                self.working_ram[address - WORKING_RAM_START] = value
            }
            IO_START..=IO_END => {
                // TODO
                match address {
                    0xFF40 => {
                        // TODO: A bunch more flags
                        self.gpu.background_map = value & 0x08 == 0x08;
                    }
                    0xFF42 => self.gpu.scroll_y = value,
                    0xFF43 => self.gpu.scroll_x = value,
                    0xFF47 => self.gpu.background_palette = value,
                    0xFF50 => self.finish_boot(),
                    _ => println!(
                        "Wrote to unimplemented IO register 0x{:02x} 0x{:02x}",
                        address, value
                    ),
                }
            }
            UNUSED_START..=UNUSED_END => {
                // no-op
            }
            ZERO_PAGE_START..=ZERO_PAGE_END => {
                self.zero_page_ram[address - ZERO_PAGE_START] = value
            }
            _ => {
                panic!("Failed to write memory at 0x{:02x}", address);
            }
        }
    }
}
