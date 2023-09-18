extern crate minifb;

mod cpu;
mod gpu;
mod instruction;
mod joypad;
mod mmu;
mod registers;
mod timer;

use std::{
    fs::read,
    thread::sleep,
    time::{Duration, Instant},
};

use cpu::CPU;
use gpu::{HEIGHT, WIDTH};
use minifb::{Key, Menu, Window, WindowOptions};
use mmu::MMU;
use rfd::FileDialog;

const BIOS_PATH: &str = "./static/bios/dmg_bootix.bin";

const KEYS: [minifb::Key; 8] = [
    Key::Up,
    Key::Down,
    Key::Left,
    Key::Right,
    Key::Z,
    Key::X,
    Key::Enter,
    Key::Backspace,
];

const LOAD_MENU_ID: usize = 1;

fn main() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);

    let mut window = Window::new(
        "rust-gb - ESC to exit",
        WIDTH * 2,
        HEIGHT * 2,
        WindowOptions::default(),
    )
    .unwrap_or_else(|e| {
        panic!("{}", e);
    });

    let mut menu = Menu::new("File").unwrap();

    menu.add_item("Load ROM", LOAD_MENU_ID).build();

    window.add_menu(&menu);

    let mut now = Instant::now();
    while window.is_open() && !window.is_key_down(Key::Escape) {
        if let Some(menu_id) = window.is_menu_pressed() {
            match menu_id {
                LOAD_MENU_ID => {
                    let file = FileDialog::new()
                        .add_filter("roms", &["gb"])
                        .set_directory("/")
                        .pick_file();
                    let rom_path = file.unwrap();
                    let rom = read(rom_path).ok().unwrap();
                    let bios = read(BIOS_PATH).ok().unwrap();
                    cpu.mmu.load_bios(bios);
                    cpu.mmu.load_rom(rom);
                    now = Instant::now();
                    sleep(Duration::from_nanos(2))
                }
                _ => (),
            }
        }

        if !cpu.mmu.bios_loaded {
            window
                .update_with_buffer(&cpu.mmu.gpu.frame_buffer, WIDTH, HEIGHT)
                .unwrap();
            continue;
        }

        // Time elapsed since last run run of while-loop
        let delta_nanos = now.elapsed().subsec_nanos();
        now = Instant::now();

        let delta_cycles = (delta_nanos as f64 / 1000000000f64) * 4190000f64;
        let mut cycles_run = 0;
        while cycles_run <= delta_cycles as usize {
            cycles_run += cpu.step() as usize;
        }

        if cpu.mmu.gpu.flush_frame_buffer {
            window
                .update_with_buffer(&cpu.mmu.gpu.frame_buffer, WIDTH, HEIGHT)
                .unwrap();
            cpu.mmu.gpu.flushed();
        }

        KEYS.iter().for_each(|key| {
            let was_down = cpu.mmu.joypad.is_key_down(*key);
            let is_down = window.is_key_down(*key);
            if !was_down && is_down {
                cpu.mmu.joypad.on_key_down(*key)
            } else if was_down && !is_down {
                cpu.mmu.joypad.on_key_up(*key);
            }
        });

        if !cpu.mmu.gpu.flush_frame_buffer {
            sleep(Duration::from_nanos(2))
        }
    }
}
