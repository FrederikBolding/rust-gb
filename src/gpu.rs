use crate::mmu::{VRAM_SIZE, OAM_SIZE};

pub const WIDTH: usize = 160;
pub const HEIGHT: usize = 144;

pub const FRAME_BUFFER_SIZE: usize = WIDTH * HEIGHT * 3;
pub const TILE_COUNT: usize = 384;

#[derive(Clone, Copy)]
pub struct Tile {
    data: [u8; 64],
}

impl Tile {
    pub fn get(&self, x: usize, y: usize) -> u8 {
        self.data[y * 8 + x]
    }

    pub fn set(&mut self, x: usize, y: usize, value: u8) {
        self.data[y * 8 + x] = value;
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum GPUMode {
    HBlank,
    VBlank,
    OAMAccess,
    VRAMAccess,
}

pub struct GPU {
    pub flush_frame_buffer: bool,
    pub frame_buffer: [u8; FRAME_BUFFER_SIZE],
    pub vram: [u8; VRAM_SIZE],
    pub oam: [u8; OAM_SIZE],
    tileset: [Tile; TILE_COUNT],

    mode_clock: u16,
    mode: GPUMode,

    // Active line
    pub line: u8,

    // Scroll
    pub scroll_x: u8,
    pub scroll_y: u8,

    // Background
    pub background_map: bool,
    pub background_palette: u8,
}

impl GPU {
    pub fn new() -> Self {
        Self {
            flush_frame_buffer: false,
            frame_buffer: [0; FRAME_BUFFER_SIZE],
            vram: [0; VRAM_SIZE],
            oam: [0; OAM_SIZE],
            tileset: [Tile { data: [0; 64] }; TILE_COUNT],
            mode_clock: 0,
            mode: GPUMode::OAMAccess,
            line: 0,
            scroll_x: 0,
            scroll_y: 0,
            background_map: false,
            background_palette: 0,
        }
    }

    pub fn flushed(&mut self) {
        self.flush_frame_buffer = false;
    }

    pub fn step(&mut self, cycles: u8) {
        self.mode_clock += cycles as u16;

        match self.mode {
            GPUMode::HBlank => {
                if self.mode_clock >= 204 {
                    self.mode_clock = 0;

                    self.line += 1;

                    if self.line == 143 {
                        self.mode = GPUMode::VBlank;

                        // Flushes frame buffer to screen elsewhere
                        self.flush_frame_buffer = true;
                    } else {
                        self.mode = GPUMode::OAMAccess;
                    }
                }
            }
            GPUMode::VBlank => {
                if self.mode_clock >= 456 {
                    self.mode_clock = 0;
                    self.line += 1;

                    if self.line > 153 {
                        self.mode = GPUMode::OAMAccess;
                        self.line = 0;
                    }
                }
            }
            GPUMode::OAMAccess => {
                if self.mode_clock >= 80 {
                    self.mode_clock = 0;
                    self.mode = GPUMode::VRAMAccess;
                }
            }
            GPUMode::VRAMAccess => {
                if self.mode_clock >= 172 {
                    self.mode_clock = 0;
                    self.mode = GPUMode::HBlank;
                    // End of this mode is end of scanline
                    self.render_scanline();
                }
            }
        }
    }

    pub fn write_vram(&mut self, address: usize, value: u8) {
        self.vram[address] = value;

        if address < 0x1800 {
            self.update_tile(address, value);
        }
    }

    pub fn write_oam(&mut self, address: usize, value: u8) {
        self.oam[address] = value;
    }

    fn update_tile(&mut self, address: usize, _value: u8) {
        let tile_address = address & 0x1FFE;

        let byte1 = self.vram[tile_address];
        let byte2 = self.vram[tile_address + 1];

        let tile = (tile_address >> 4) & 0x01ff;
        let y = (tile_address >> 1) & 0x0007;

        for x in 0..8 {
            let bitmask = 1 << (7 - x);
            let lsb = byte1 & bitmask;
            let msb = byte2 & bitmask;

            let byte1_result = if lsb != 0 { 1 } else { 0 };
            let byte2_result = if msb != 0 { 2 } else { 0 };

            self.tileset[tile].set(x, y, byte1_result + byte2_result);
        }
    }

    fn get_palette_color(&self, pixel: u8) -> [u8; 3] {
        let a = self.background_palette & 0b11;
        let b = (self.background_palette >> 2) & 0b11;
        let c = (self.background_palette >> 4) & 0b11;
        let d = self.background_palette >> 6;
        let index = match pixel {
            0 => a,
            1 => b,
            2 => c,
            3 => d,
            _ => panic!("Invalid pixel input"),
        };
        match index {
            0 => [255, 255, 255],
            1 => [192, 192, 192],
            2 => [96, 96, 96],
            3 => [0, 0, 0],
            _ => panic!("Invalid pixel input"),
        }
    }

    fn render_scanline(&mut self) {
        let row_offset = ((((self.line as usize) + (self.scroll_y as usize)) & 0xff) >> 3) % 32;
        let map_offset: usize =
            if self.background_map { 0x1C00 } else { 0x1800 } + (row_offset * 32);

        let mut line_offset = (self.scroll_x >> 3) as usize;
        let y = ((self.line as usize + self.scroll_y as usize) & 7) as usize;
        let mut x = (self.scroll_x & 7) as usize;
        let mut frame_offset = (self.line as usize) * WIDTH * 3;

        let mut tile_index = self.vram[map_offset + line_offset] as usize;
        // Some tiles are indexed using signed indices
        if !self.background_map && tile_index < 128 {
            tile_index += 256;
        }

        for _ in 0..WIDTH {
            let pixel = self.tileset[tile_index].get(x, y);
            let color = self.get_palette_color(pixel);
            self.frame_buffer[frame_offset] = color[0];
            self.frame_buffer[frame_offset + 1] = color[1];
            self.frame_buffer[frame_offset + 2] = color[2];

            x += 1;
            if x == 8 {
                x = 0;
                line_offset = (line_offset + 1) % 32;
                tile_index = self.vram[map_offset + line_offset] as usize;
            }

            frame_offset += 3;
        }
    }
}
