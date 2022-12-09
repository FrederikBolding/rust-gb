// Memory Layout
// 0000-00FF - BIOS
// 0100-014F - Cartridge header
// 014F-4000 - ROM bank 0
// 4000-7FFF - ROM bank N
// 8000-9FFF - Graphics RAM
// A000-BFFF - External RAM
// C000-DFFF - Working RAM
// E000-FDFF - Shadow working RAM
// FE00-FE9F - Sprite information (OAM)
// FF00-FF7F - Memory-mapped IO
// FF80-FFFF - Zero-page RAM

use crate::gpu::GPU;

pub const BIOS_START: usize = 0x00;
pub const BIOS_END: usize = 0xFF;
pub const BIOS_SIZE: usize = BIOS_END - BIOS_START + 1;

pub const ROM_BANK_0_START: usize = 0x0000;
pub const ROM_BANK_0_END: usize = 0x3FFF;

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

pub const OAM_START: usize = 0xFE00;
pub const OAM_END: usize = 0xFE9F;
pub const OAM_SIZE: usize = OAM_END - OAM_START + 1;

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
    rom: Vec<u8>,
    external_ram: [u8; EXTERNAL_RAM_SIZE],
    working_ram: [u8; WORKING_RAM_SIZE],
    zero_page_ram: [u8; ZERO_PAGE_SIZE],
    rom_bank: u8,
    // Interrupts
    pub vblank_interrupt_enabled: bool,
    pub lcdstat_interrupt_enabled: bool,
    pub timer_interrupt_enabled: bool,  // TODO
    pub serial_interrupt_enabled: bool, // TODO
    pub joypad_interrupt_enabled: bool, // TODO
}

impl MMU {
    pub fn new() -> Self {
        Self {
            booting: true,
            gpu: GPU::new(),
            bios: [0; BIOS_SIZE],
            rom: vec![0; 0],
            external_ram: [0; EXTERNAL_RAM_SIZE],
            working_ram: [0; WORKING_RAM_SIZE],
            zero_page_ram: [0; ZERO_PAGE_SIZE],
            rom_bank: 0,
            vblank_interrupt_enabled: false,
            lcdstat_interrupt_enabled: false,
            timer_interrupt_enabled: false,
            serial_interrupt_enabled: false,
            joypad_interrupt_enabled: false,
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
        self.rom = rom;
    }

    pub fn read(&mut self, address: u16) -> u8 {
        let address = address as usize;
        //println!("Read 0x{:02x}", address);
        match address {
            BIOS_START..=BIOS_END => {
                if self.booting {
                    self.bios[address]
                } else {
                    self.rom[address]
                }
            }
            ROM_BANK_0_START..=ROM_BANK_0_END => self.rom[address],
            ROM_BANK_N_START..=ROM_BANK_N_END => {
                let offset = ROM_BANK_N_SIZE * self.rom_bank as usize;
                self.rom[offset + address - ROM_BANK_N_START]
            }
            VRAM_START..=VRAM_END => self.gpu.vram[address - VRAM_START],
            OAM_START..=OAM_END => self.gpu.oam[address - OAM_START],
            EXTERNAL_RAM_START..=EXTERNAL_RAM_END => {
                self.external_ram[address - EXTERNAL_RAM_START]
            }
            WORKING_RAM_START..=WORKING_RAM_END => self.working_ram[address - WORKING_RAM_START],
            SHADOW_WORKING_RAM_START..=SHADOW_WORKING_RAM_END => {
                self.working_ram[address - SHADOW_WORKING_RAM_START]
            }
            IO_START..=IO_END => match address {
                0xFF25 => 0, // TODO
                0xFF42 => self.gpu.scroll_y,
                0xFF44 => self.gpu.line,
                0xFF41 => {
                    (if self.gpu.hblank_interrupt_enabled { 0x08 } else { 0x00 }
                        | if self.gpu.vblank_interrupt_enabled { 0x10 } else { 0x00 }
                        | if self.gpu.oam_interrupt_enabled { 0x20 } else { 0x00 }
                        | if self.gpu.line_equals_line_interrupt_enabled { 0x40 } else { 0x00 }
                        | if false { 0x04 } else { 0x00 } // TODO
                        | (self.gpu.mode as u8 & 0x03))
                }
                _ => todo!(
                    "Tried to read from unimplemented IO register 0x{:02x}",
                    address
                ),
            },
            ZERO_PAGE_START..=ZERO_PAGE_END => self.zero_page_ram[address - ZERO_PAGE_START],
            0xFFFF => {
                0b11100000
                    | ((if self.joypad_interrupt_enabled { 1 } else { 0 }) << 4)
                    | ((if self.serial_interrupt_enabled { 1 } else { 0 }) << 3)
                    | ((if self.timer_interrupt_enabled { 1 } else { 0 }) << 2)
                    | ((if self.lcdstat_interrupt_enabled { 1 } else { 0 }) << 1)
                    | (if self.vblank_interrupt_enabled { 1 } else { 0 })
            }
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
                self.write_rom(address as u16, value);
            }
            VRAM_START..=VRAM_END => {
                self.gpu.write_vram(address - VRAM_START, value);
            }
            OAM_START..=OAM_END => {
                self.gpu.write_oam(address - OAM_START, value);
            }
            EXTERNAL_RAM_START..=EXTERNAL_RAM_END => {
                self.external_ram[address - EXTERNAL_RAM_START] = value;
            }
            WORKING_RAM_START..=WORKING_RAM_END => {
                self.working_ram[address - WORKING_RAM_START] = value
            }
            SHADOW_WORKING_RAM_START..=SHADOW_WORKING_RAM_END => {
                self.working_ram[address - SHADOW_WORKING_RAM_START] = value
            }
            IO_START..=IO_END => {
                // TODO
                match address {
                    0xFF40 => {
                        // TODO: A bunch more flags
                        self.gpu.background_map = value & 0x08 == 0x08;
                    }
                    0xFF41 => {
                        self.gpu.vblank_interrupt_enabled = value & 0x08 == 0x08;
                        self.gpu.hblank_interrupt_enabled = value & 0x10 == 0x10;
                        self.gpu.oam_interrupt_enabled = value & 0x20 == 0x20;
                        self.gpu.line_equals_line_interrupt_enabled = value & 0x40 == 0x40;
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
            0xFFFF => {
                self.vblank_interrupt_enabled = (value & 0b1) == 0b1;
                self.lcdstat_interrupt_enabled = (value & 0b10) == 0b10;
                self.timer_interrupt_enabled = (value & 0b100) == 0b100;
                self.serial_interrupt_enabled = (value & 0b1000) == 0b1000;
                self.joypad_interrupt_enabled = (value & 0b10000) == 0b10000;
            }
            _ => {
                panic!("Failed to write memory at 0x{:02x}", address);
            }
        }
    }

    fn write_rom(&mut self, address: u16, value: u8) {
        match address & 0xf000 {
            // RAM enabled flag
            0x0000 | 0x1000 => {
                // TODO
                println!("Enable RAM");
            }
            // ROM bank selection
            0x2000 => {
                println!("Setting ROM bank 0x{:2x} to {:?}", address, value);
                self.rom_bank = value;
            }
            _ => panic!("Writing to unimplemented rom location"),
        }
    }
}
