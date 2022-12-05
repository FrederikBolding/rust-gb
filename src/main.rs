extern crate minifb;

mod cpu;
mod instruction;
mod mmu;
mod registers;

use std::fs::read;

use cpu::CPU;
use minifb::{Key, Window, WindowOptions};
use mmu::MMU;

const WIDTH: usize = 640;
const HEIGHT: usize = 360;

const BIOS_PATH: &str = "./static/bios/dmg_bootix.bin";
const ROM_PATH: &str = "./static/roms/pocket.gb";

fn main() {
    let bios = read(BIOS_PATH).ok().unwrap();
    let rom = read(ROM_PATH).ok().unwrap();
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.load_bios(bios);
    cpu.mmu.load_rom(rom);

    let mut buffer: Vec<u32> = vec![0; WIDTH * HEIGHT];

    let mut window = Window::new(
        "Test - ESC to exit",
        WIDTH,
        HEIGHT,
        WindowOptions::default(),
    )
    .unwrap_or_else(|e| {
        panic!("{}", e);
    });

    // Limit to max ~60 fps update rate
    window.limit_update_rate(Some(std::time::Duration::from_micros(16600)));

    while window.is_open() && !window.is_key_down(Key::Escape) {
        cpu.step();
        //for i in buffer.iter_mut() {
         //   *i = 0; // write something more funny here!
        //}

        // We unwrap here as we want this code to exit if it fails. Real applications may want to handle this in a different way
        //window.update_with_buffer(&buffer, WIDTH, HEIGHT).unwrap();
    }
}
