use crate::instruction::{self, Instruction};
use crate::mmu::MMU;

pub struct CPU {
    program_counter: u16,

    // Registers
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,

    // MMU
    pub mmu: MMU,
}

impl CPU {
    pub fn new(mmu: MMU) -> Self {
        Self {
            program_counter: 0x0,
            a: 0x0,
            b: 0x0,
            c: 0x0,
            d: 0x0,
            e: 0x0,
            f: 0x0,
            h: 0x0,
            l: 0x0,
            mmu,
        }
    }

    pub fn step(&mut self) {
        let byte = self.mmu.read(self.program_counter);
        println!("Byte: 0x{:02x}", byte);
        let instruction = Instruction::decode(byte);
        println!("Instruction {:?}", instruction);
        self.execute(instruction);
    }

    fn execute(&mut self, instruction: Instruction) {
        todo!();
    }
}

#[test]
fn test_step() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.write(0, 0x23); //INC(HL)
    cpu.mmu.write(1, 0xB5); //OR(L)
    cpu.mmu.write(2, 0xCB); //PREFIX
    cpu.mmu.write(3, 0xe8); //SET(B, 5)
    for _ in 0..3 {
        cpu.step();
    }

    assert_eq!(cpu.h, 0b0);
    assert_eq!(cpu.l, 0b1);
    assert_eq!(cpu.a, 0b1);
    assert_eq!(cpu.b, 0b0010_0000);
}

#[test]
fn test_step_2() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.write(0, 0x3C); //INC A
    cpu.mmu.write(1, 0x04); // INC B
    cpu.mmu.write(2, 0x80); // ADD A,B
    for _ in 0..2 {
        cpu.step();
    }

    assert_eq!(cpu.a, 2);
    assert_eq!(cpu.b, 1);
}

#[test]
fn test_step_3() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.write(0, 0x04); //INC B
    cpu.mmu.write(1, 0x50); // LD D, B
    for _ in 0..1 {
        cpu.step();
    }

    assert_eq!(cpu.d, 1);
    assert_eq!(cpu.b, 1);
}
