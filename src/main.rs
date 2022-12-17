extern crate minifb;

mod cpu;
mod gpu;
mod instruction;
mod mmu;
mod registers;

use std::{
    fs::read,
    thread::sleep,
    time::{Duration, Instant},
};

use cpu::CPU;
use gpu::{HEIGHT, WIDTH};
use minifb::{Key, Window, WindowOptions};
use mmu::MMU;

const BIOS_PATH: &str = "./static/bios/dmg_bootix.bin";
const ROM_PATH: &str = "./static/roms/cpu_instrs.gb";

fn main() {
    let bios = read(BIOS_PATH).ok().unwrap();
    let rom = read(ROM_PATH).ok().unwrap();
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.load_bios(bios);
    cpu.mmu.load_rom(rom);

    let mut buffer: Vec<u32> = vec![0; WIDTH * HEIGHT];

    let mut window = Window::new(
        "rust-gb - ESC to exit",
        WIDTH * 2,
        HEIGHT * 2,
        WindowOptions::default(),
    )
    .unwrap_or_else(|e| {
        panic!("{}", e);
    });

    let mut now = Instant::now();
    while window.is_open() && !window.is_key_down(Key::Escape) {
        // Time elapsed since last run run of while-loop
        let delta_nanos = now.elapsed().subsec_nanos();
        now = Instant::now();

        let delta_cycles = (delta_nanos as f64 / 1000000000f64) * 4190000f64;
        let mut cycles_run = 0;
        while cycles_run <= delta_cycles as usize {
            cycles_run += cpu.step() as usize;
        }

        if cpu.mmu.gpu.flush_frame_buffer {
            for (i, pixel) in cpu.mmu.gpu.frame_buffer.chunks(3).enumerate() {
                buffer[i] = (pixel[2] as u32) << 16 | (pixel[1] as u32) << 8 | (pixel[0] as u32)
            }
            window.update_with_buffer(&buffer, WIDTH, HEIGHT).unwrap();
            cpu.mmu.gpu.flushed();
        } else {
            sleep(Duration::from_nanos(2))
        }
    }
}
