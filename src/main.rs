extern crate num;
#[macro_use]
extern crate num_derive;

mod cpu;
mod mmu;
mod instruction;
mod registers;

use cpu::CPU;
use mmu::MMU;

fn main() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.step();
    println!("Foo bar");
}
