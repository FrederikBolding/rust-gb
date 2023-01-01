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

use crate::{gpu::{GPUMode, GPU}, joypad::Joypad, timer::Timer};

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
    pub timer: Timer,
    pub joypad: Joypad,
    bios: [u8; BIOS_SIZE],
    rom: Vec<u8>,
    external_ram: [u8; EXTERNAL_RAM_SIZE],
    working_ram: [u8; WORKING_RAM_SIZE],
    zero_page_ram: [u8; ZERO_PAGE_SIZE],
    rom_bank: u8,
    // Interrupts
    pub vblank_interrupt_enabled: bool,
    pub lcdstat_interrupt_enabled: bool,
    pub timer_interrupt_enabled: bool,
    pub serial_interrupt_enabled: bool, // TODO
    pub joypad_interrupt_enabled: bool,
}

impl MMU {
    pub fn new() -> Self {
        Self {
            booting: true,
            gpu: GPU::new(),
            timer: Timer::new(),
            joypad: Joypad::new(),
            bios: [0; BIOS_SIZE],
            rom: vec![0; 0],
            external_ram: [0; EXTERNAL_RAM_SIZE],
            working_ram: [0; WORKING_RAM_SIZE],
            zero_page_ram: [0; ZERO_PAGE_SIZE],
            rom_bank: 1,
            vblank_interrupt_enabled: false,
            lcdstat_interrupt_enabled: false,
            timer_interrupt_enabled: false,
            serial_interrupt_enabled: false,
            joypad_interrupt_enabled: false,
        }
    }

    pub fn step(&mut self, cycles: u8) {
        self.timer.step(cycles);
        self.gpu.step(cycles);
    }

    pub fn finish_boot(&mut self) {
        self.booting = false;
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
        println!("ROM type 0x{:2x}", self.rom[0x0147]);
        let title = String::from(std::str::from_utf8(&self.rom[0x0134..0x0143]).unwrap());
        println!("ROM Title: {}", title);
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
                0xFF00 => self.joypad.to_byte(),
                0xFF01 => 0,    // TODO
                0xFF02 => 0,    // TODO
                0xFF04 => self.timer.divider,
                0xFF0F => {
                    (if self.gpu.vblank_interrupt_flag { 0x01 } else { 0x00 }
                            | if self.gpu.lcdstat_interrupt_flag { 0x02 } else { 0x00 }
                            | if self.timer.interrupt_flag { 0x04 } else { 0x00 }
                            | if self.joypad.interrupt_flag { 0x10 } else { 0x00 })
                }
                0xFF25 => 0, // TODO
                0xFF40 => {
                    (if self.gpu.background_enabled {
                        0x01
                    } else {
                        0x00
                    } | if self.gpu.objects_enabled { 0x02 } else { 0x00 }
                        | if self.gpu.objects_size { 0x04 } else { 0x00 }
                        | if self.gpu.background_map { 0x08 } else { 0x00 }
                        | if self.gpu.background_tile { 0x10 } else { 0x00 }
                        | if self.gpu.window_enabled { 0x20 } else { 0x00 }
                        | if self.gpu.window_map { 0x40 } else { 0x00 }
                        | if self.gpu.lcd_enabled { 0x80 } else { 0x00 })
                }
                0xFF41 => {
                    (if self.gpu.hblank_interrupt_enabled { 0x08 } else { 0x00 }
                        | if self.gpu.vblank_interrupt_enabled { 0x10 } else { 0x00 }
                        | if self.gpu.oam_interrupt_enabled { 0x20 } else { 0x00 }
                        | if self.gpu.line_equals_line_interrupt_enabled { 0x40 } else { 0x00 }
                        | if self.gpu.line == self.gpu.line_check { 0x04 } else { 0x00 }
                        | (self.gpu.mode as u8 & 0x03))
                }
                0xFF42 => self.gpu.scroll_y,
                0xFF43 => self.gpu.scroll_x,
                0xFF44 => self.gpu.line,
                0xFF45 => self.gpu.line_check,
                0xFF4A => self.gpu.window_y,
                0xFF4B => self.gpu.window_x,
                0xFF4D => 0xFF, // TODO
                _ => todo!(
                    "Tried to read from unimplemented IO register 0x{:02x}",
                    address
                ),
            },
            UNUSED_START..=UNUSED_END => 0,
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
                    0xFF00 => self.joypad.column = value & 0x20 == 0,
                    0xFF04 => self.timer.divider = 0,
                    0xFF05 => {
                        self.timer.counter = value;
                    }
                    0xFF06 => {
                        self.timer.modulo = value;
                    }
                    0xFF07 => {
                        self.timer.ratio = match value & 0b11 {
                            0b00 => 1024,
                            0b11 => 256,
                            0b10 => 64,
                            _ => 16,
                        };
                        self.timer.enabled = (value & 0x04) == 0x04;
                    }
                    0xFF0F => {
                        self.gpu.vblank_interrupt_flag = value & 0x01 == 0x01;
                        self.gpu.lcdstat_interrupt_flag = value & 0x02 == 0x02;
                        self.timer.interrupt_flag = value & 0x04 == 0x04;
                        self.joypad.interrupt_flag = value & 0x10 == 0x10;
                    }
                    0xFF40 => {
                        self.gpu.background_enabled = value & 0x01 == 0x01;
                        self.gpu.objects_enabled = value & 0x02 == 0x02;
                        self.gpu.objects_size = value & 0x04 == 0x04;
                        self.gpu.background_map = value & 0x08 == 0x08;
                        self.gpu.background_tile = value & 0x10 == 0x10;
                        self.gpu.window_enabled = value & 0x20 == 0x20;
                        self.gpu.window_map = value & 0x40 == 0x40;
                        self.gpu.lcd_enabled = value & 0x80 == 0x80;

                        if !self.gpu.lcd_enabled {
                            self.gpu.mode = GPUMode::HBlank;
                            self.gpu.mode_clock = 0;
                            self.gpu.line = 0;
                            self.gpu.vblank_interrupt_flag = false;
                            self.gpu.lcdstat_interrupt_flag = false;
                            self.gpu.clear_frame_buffer();
                        }
                    }
                    0xFF41 => {
                        self.gpu.vblank_interrupt_enabled = value & 0x08 == 0x08;
                        self.gpu.hblank_interrupt_enabled = value & 0x10 == 0x10;
                        self.gpu.oam_interrupt_enabled = value & 0x20 == 0x20;
                        self.gpu.line_equals_line_interrupt_enabled = value & 0x40 == 0x40;
                    }
                    0xFF42 => self.gpu.scroll_y = value,
                    0xFF43 => self.gpu.scroll_x = value,
                    0xFF45 => self.gpu.line_check = value,
                    0xFF46 => {
                        // Direct Memory Access
                        let source = (value as u16) << 8;
                        let destination = OAM_START as u16;
                        for offset in 0..150 {
                            let byte = self.read(source + offset);
                            self.write(destination + offset, byte)
                        }
                    }
                    0xFF4A => self.gpu.window_y = value,
                    0xFF4B => self.gpu.window_x = value,
                    0xFF47 => self.gpu.background_palette = value,
                    0xFF48 => self.gpu.objects_palette_0 = value,
                    0xFF49 => self.gpu.objects_palette_1 = value,
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
                println!("Enable RAM: {}", (value & 0x0f) == 0x0a);
            }
            // ROM bank selection
            0x2000 => {
                // TODO: Stop assuming MBC1
                let rom_bank_count = match self.rom[0x0148] {
                    0x00 => 2,
                    0x01 => 4,
                    0x02 => 8,
                    0x03 => 16,
                    0x04 => 32,
                    0x05 => 64,
                    0x06 => 128,
                    0x07 => 256,
                    0x08 => 512,
                    _ => 0,
                };
                let mut bank = value & 0x1f;
                bank &= (rom_bank_count * 2 - 1) as u8;
                if bank == 0 {
                    bank = 1;
                }
                //println!("Setting ROM bank 0x{:2x} to {:?} ({})", address, bank, rom_bank_count);
                self.rom_bank = bank;
            }
            0x4000 | 0x5000 => {
                println!("Setting RAM bank 0x{:2x} to {:?}", address, value);
            }
            _ => panic!(
                "Writing to unimplemented rom location 0x{:2x} 0x{:2x}",
                address, value
            ),
        }
    }
}
