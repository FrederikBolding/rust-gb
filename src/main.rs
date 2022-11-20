mod cpu;
mod mmu;
mod instruction;

use cpu::CPU;
use mmu::MMU;

fn main() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.step();
    println!("Foo bar");
}
