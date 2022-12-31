use std::borrow::BorrowMut;

use crate::mmu::{OAM_SIZE, VRAM_SIZE};

pub const WIDTH: usize = 160;
pub const HEIGHT: usize = 144;

pub const FRAME_BUFFER_SIZE: usize = WIDTH * HEIGHT * 3;
pub const COLOR_BUFFER_SIZE: usize = WIDTH * HEIGHT;
pub const TILE_COUNT: usize = 384;
pub const OBJECT_COUNT: usize = 40;

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

    pub fn get_row(&self, y: usize) -> &[u8] {
        &self.data[y * 8..(y + 1) * 8]
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Object {
    x: i16,
    y: i16,
    tile: u8,
    palette: bool,
    flip_x: bool,
    flip_y: bool,
    priority: bool,
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
    color_buffer: [u8; COLOR_BUFFER_SIZE],
    pub vram: [u8; VRAM_SIZE],
    pub oam: [u8; OAM_SIZE],
    tileset: [Tile; TILE_COUNT],
    objects: [Object; OBJECT_COUNT],

    pub mode_clock: u16,
    pub mode: GPUMode,

    pub lcd_enabled: bool,

    // Interrupts
    pub vblank_interrupt_flag: bool,
    // All of these determine when to trigger the LCD status interrupt
    pub vblank_interrupt_enabled: bool,
    pub oam_interrupt_enabled: bool,
    pub hblank_interrupt_enabled: bool,
    pub line_equals_line_interrupt_enabled: bool, // TODO
    pub lcdstat_interrupt_flag: bool,

    // Active line
    pub line: u8,
    pub line_check: u8,

    // Scroll
    pub scroll_x: u8,
    pub scroll_y: u8,

    // Window
    pub window_enabled: bool,
    pub window_map: bool,
    pub window_x: u8,
    pub window_y: u8,
    pub window_counter: u8,

    // Background
    pub background_enabled: bool,
    pub background_map: bool,
    pub background_tile: bool,
    pub background_palette: u8,

    // Objects
    pub objects_enabled: bool,
    pub objects_size: bool,
    pub objects_palette_0: u8,
    pub objects_palette_1: u8,
}

impl GPU {
    pub fn new() -> Self {
        Self {
            flush_frame_buffer: false,
            frame_buffer: [0; FRAME_BUFFER_SIZE],
            color_buffer: [0; COLOR_BUFFER_SIZE],
            vram: [0; VRAM_SIZE],
            oam: [0; OAM_SIZE],
            tileset: [Tile { data: [0; 64] }; TILE_COUNT],
            objects: [Object {
                x: 0,
                y: 0,
                tile: 0,
                flip_x: false,
                flip_y: false,
                palette: false,
                priority: false,
            }; OBJECT_COUNT],
            mode_clock: 0,
            mode: GPUMode::OAMAccess,
            lcd_enabled: false,
            vblank_interrupt_enabled: false,
            vblank_interrupt_flag: false,
            oam_interrupt_enabled: false,
            hblank_interrupt_enabled: false,
            line_equals_line_interrupt_enabled: false,
            lcdstat_interrupt_flag: false,
            line: 0,
            line_check: 0,
            scroll_x: 0,
            scroll_y: 0,
            window_enabled: false,
            window_map: false,
            window_x: 0,
            window_y: 0,
            window_counter: 0,
            background_enabled: false,
            background_map: false,
            background_tile: false,
            background_palette: 0,
            objects_enabled: false,
            objects_size: false,
            objects_palette_0: 0,
            objects_palette_1: 0
        }
    }

    pub fn flushed(&mut self) {
        self.flush_frame_buffer = false;
    }

    pub fn clear_frame_buffer(&mut self) {
        for index in 0..self.frame_buffer.len() {
            self.frame_buffer[index] = 0;
        }
        self.flush_frame_buffer = true;
    }

    pub fn step(&mut self, cycles: u8) {
        if !self.lcd_enabled {
            return;
        }

        self.mode_clock += cycles as u16;

        match self.mode {
            GPUMode::HBlank => {
                if self.mode_clock >= 204 {
                    self.mode_clock -= 204;

                    if self.window_enabled
                        && self.window_x as i16 - 7 < WIDTH as i16
                        && self.window_y < HEIGHT as u8
                        && self.line >= self.window_y
                    {
                        self.window_counter += 1;
                    }

                    self.line += 1;

                    if self.line_equals_line_interrupt_enabled && self.line == self.line_check {
                        self.lcdstat_interrupt_flag = true;
                    }

                    if self.line == 144 {
                        self.mode = GPUMode::VBlank;

                        self.vblank_interrupt_flag = true;
                        if self.vblank_interrupt_enabled {
                            self.lcdstat_interrupt_flag = true;
                        }

                        // Flushes frame buffer to screen elsewhere
                        self.flush_frame_buffer = true;
                    } else {
                        self.mode = GPUMode::OAMAccess;
                        if self.oam_interrupt_enabled {
                            self.lcdstat_interrupt_flag = true;
                        }
                    }
                }
            }
            GPUMode::VBlank => {
                if self.mode_clock >= 456 {
                    self.mode_clock -= 456;
                    self.line += 1;

                    if self.line_equals_line_interrupt_enabled && self.line == self.line_check {
                        self.lcdstat_interrupt_flag = true;
                    }

                    if self.line == 154 {
                        self.mode = GPUMode::OAMAccess;
                        if self.oam_interrupt_enabled {
                            self.lcdstat_interrupt_flag = true;
                        }
                        self.line = 0;
                        self.window_counter = 0;
                    }
                }
            }
            GPUMode::OAMAccess => {
                if self.mode_clock >= 80 {
                    self.mode_clock -= 80;
                    self.mode = GPUMode::VRAMAccess;
                }
            }
            GPUMode::VRAMAccess => {
                if self.mode_clock >= 172 {
                    self.mode_clock -= 172;
                    self.mode = GPUMode::HBlank;
                    if self.hblank_interrupt_enabled {
                        self.lcdstat_interrupt_flag = true;
                    }
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

        self.update_object(address, value);
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

    fn update_object(&mut self, address: usize, value: u8) {
        let index = address / 4;
        if index > OBJECT_COUNT {
            return;
        }

        let byte = address % 4;

        let mut object = self.objects[index].borrow_mut();

        match byte {
            0 => object.y = (value as i16) - 0x10,
            1 => object.x = (value as i16) - 0x8,
            2 => object.tile = value,
            _ => {
                object.palette = (value & 0x10) == 0x10;
                object.flip_x = (value & 0x20) == 0x20;
                object.flip_y = (value & 0x40) == 0x40;
                object.priority = (value & 0x80) == 0x80;
            }
        }
    }

    fn get_palette_color(&self, palette: u8, pixel: u8) -> [u8; 3] {
        let a = palette & 0b11;
        let b = (palette >> 2) & 0b11;
        let c = (palette >> 4) & 0b11;
        let d = palette >> 6;
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
        if self.background_enabled {
            self.render_map(
                self.background_map,
                self.scroll_x,
                self.scroll_y,
                0,
                self.line,
            );
        }
        if self.window_enabled && self.line >= self.window_y {
            self.render_map(self.window_map, 0, 0, self.window_x, self.window_counter);
        }

        if self.objects_enabled {
            self.render_objects();
        }
    }

    // TODO: Clean this up
    fn render_map(&mut self, map: bool, scroll_x: u8, scroll_y: u8, window_x: u8, line: u8) {
        let row_offset = ((((self.line as usize) + (scroll_y as usize)) & 0xff) >> 3) % 32;
        let map_offset: usize = if map { 0x1C00 } else { 0x1800 } + (row_offset * 32);

        let mut line_offset = (scroll_x >> 3) as usize;
        let y = ((line as usize + scroll_y as usize) & 7) as usize;
        let mut x = (scroll_x & 7) as usize;
        let mut color_offset = (line as usize) * WIDTH;
        let mut frame_offset = (line as usize) * WIDTH * 3;

        let mut tile_index = self.vram[map_offset + line_offset] as usize;
        // Some tiles are indexed using signed indices
        if !self.background_tile && tile_index < 128 {
            tile_index += 256;
        }

        for index in 0..WIDTH {
            if index as i16 >= window_x as i16 - 7 {
                let pixel = self.tileset[tile_index].get(x, y);
                let color = self.get_palette_color(self.background_palette, pixel);
                self.color_buffer[color_offset] = pixel;
                self.frame_buffer[frame_offset] = color[0];
                self.frame_buffer[frame_offset + 1] = color[1];
                self.frame_buffer[frame_offset + 2] = color[2];

                x += 1;
                if x == 8 {
                    x = 0;
                    line_offset = (line_offset + 1) % 32;
                    tile_index = self.vram[map_offset + line_offset] as usize;
                    if !self.background_tile && tile_index < 128 {
                        tile_index += 256;
                    }
                }
            }

            color_offset += 1;
            frame_offset += 3;
        }
    }

    // TODO: Clean this up
    fn render_objects(&mut self) {
        let mut draw_count = 0u8;
        let mut index_buffer = [-256i16; WIDTH];

        let object_height = if self.objects_size { 16 } else { 8 };
        let line = self.line as i16;

        for index in 0..OBJECT_COUNT {
            // Only 10 objects can be drawn in one line
            if draw_count == 10 {
                break;
            }

            let object = self.objects[index];

            if !((object.y <= line) && ((object.y + object_height as i16) > line)) {
                continue;
            }

            let palette = if object.palette {
                self.objects_palette_1
            } else {
                self.objects_palette_0
            };

            let mut color_offset = self.line as i32 * WIDTH as i32 + object.x as i32;
            let mut frame_offset = (self.line as i32 * WIDTH as i32 + object.x as i32) * 3 as i32;
            let mut tile_offset = line - object.y;

            if object.flip_y {
                tile_offset = object_height as i16 - tile_offset - 1;
            }

            let tile = if self.objects_size {
                if tile_offset < 8 {
                    self.tileset[object.tile as usize & 0xfe]
                } else {
                    tile_offset -= 8;
                    self.tileset[object.tile as usize | 0x01]
                }
            } else {
                self.tileset[object.tile as usize]
            };

            let tile_row = tile.get_row(tile_offset as usize);

            for tile_x in 0..8 {
                let x = object.x + tile_x as i16;
                let is_contained = (x >= 0) && (x < WIDTH as i16);
                if is_contained {
                    let is_visible =
                        !object.priority || self.color_buffer[color_offset as usize] == 0;

                    let has_priority =
                        index_buffer[x as usize] == -256 || object.x < index_buffer[x as usize];

                    let pixel = tile_row[if object.flip_x { 7 - tile_x } else { tile_x }];
                    if is_visible && has_priority && pixel != 0 {
                        index_buffer[x as usize] = object.x;

                        let color = self.get_palette_color(palette, pixel);

                        self.color_buffer[color_offset as usize] = pixel;
                        self.frame_buffer[frame_offset as usize] = color[0];
                        self.frame_buffer[frame_offset as usize + 1] = color[1];
                        self.frame_buffer[frame_offset as usize + 2] = color[2];
                    }
                }

                color_offset += 1;
                frame_offset += 3;
            }

            draw_count += 1;
        }
    }
}
