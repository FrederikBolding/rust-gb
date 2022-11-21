use crate::instruction::{Instruction, LoadType};
use crate::mmu::MMU;
use crate::registers::{RegisterTarget, Registers};

pub struct CPU {
    program_counter: u16,
    stack_pointer: u16,

    // Registers
    pub registers: Registers,

    // MMU
    pub mmu: MMU,
}

impl CPU {
    pub fn new(mmu: MMU) -> Self {
        Self {
            program_counter: 0x0,
            stack_pointer: 0x0,
            registers: Registers::new(),
            mmu,
        }
    }

    fn read_current_byte(&mut self) -> u8 {
        self.mmu.read(self.program_counter)
    }

    fn read_next_byte(&mut self) -> u8 {
        self.mmu.read(self.program_counter + 1)
    }

    pub fn step(&mut self) {
        let byte = self.read_current_byte();
        let is_prefixed = byte == 0xCB;
        let instruction_byte = if is_prefixed {
            self.read_next_byte()
        } else {
            byte
        };
        println!("Byte: 0x{:02x}", instruction_byte);
        let instruction = Instruction::from_byte(instruction_byte, false).unwrap();
        println!("Instruction {:?}", instruction);
        self.execute(instruction);
        self.program_counter += 1;
    }

    fn execute(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::NOP => {
                // no-op
            }
            Instruction::ADD(target) => {
                // TODO: Figure out better pattern
                let single_register = num::FromPrimitive::from_u8(target as u8);
                match single_register {
                    Some(single_register) => {
                        let register1 = self.registers.get(single_register);
                        let register2 = self.registers.get(RegisterTarget::A);
                        let value = register1.wrapping_add(register2);
                        self.registers.set(RegisterTarget::A, value);
                    }
                    _ => {
                        panic!("Failed to ADD");
                    }
                }
            }
            Instruction::SUB(target) => {
                let single_register = num::FromPrimitive::from_u8(target as u8);
                match single_register {
                    Some(single_register) => {
                        let register1 = self.registers.get(single_register);
                        let register2 = self.registers.get(RegisterTarget::A);
                        let value = register1.wrapping_sub(register2);
                        self.registers.set(RegisterTarget::A, value);
                    }
                    _ => {
                        panic!("Failed to ADD");
                    }
                }
            }
            Instruction::INC(target) => {
                let single_register = num::FromPrimitive::from_u8(target as u8);
                match single_register {
                    Some(single_register) => {
                        let register = self.registers.get(single_register);
                        let value = register.wrapping_add(1);
                        self.registers.set(single_register, value);
                    }
                    _ => {
                        panic!("Failed to INC");
                    }
                }
            }
            Instruction::DEC(target) => {
                let single_register = num::FromPrimitive::from_u8(target as u8);
                match single_register {
                    Some(single_register) => {
                        let register = self.registers.get(single_register);
                        let value = register.wrapping_sub(1);
                        self.registers.set(single_register, value);
                    }
                    _ => {
                        panic!("Failed to INC");
                    }
                }
            }
            Instruction::LD(load_type) => match load_type {
                _ => {
                    panic!("Failed to load {:?}", load_type)
                }
            },
            _ => {
                panic!("Failed to execute {:?}", instruction);
            }
        }
    }
}

#[test]
fn test_step() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.write(0, 0x23); // INC(HL)
    cpu.mmu.write(1, 0xB5); // OR(L)
    cpu.mmu.write(2, 0xCB); // PREFIX
    cpu.mmu.write(3, 0xe8); // SET(B, 5)
    for _ in 0..=3 {
        cpu.step();
    }

    assert_eq!(cpu.registers.h, 0b0);
    assert_eq!(cpu.registers.l, 0b1);
    assert_eq!(cpu.registers.a, 0b1);
    assert_eq!(cpu.registers.b, 0b0010_0000);
}

#[test]
fn test_step_2() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.write(0, 0x3C); //INC A
    cpu.mmu.write(1, 0x04); // INC B
    cpu.mmu.write(2, 0x80); // ADD A,B
    for _ in 0..=2 {
        cpu.step();
    }

    assert_eq!(cpu.registers.a, 2);
    assert_eq!(cpu.registers.b, 1);
}

#[test]
fn test_step_3() {
    let mmu = MMU::new();
    let mut cpu = CPU::new(mmu);
    cpu.mmu.write(0, 0x04); //INC B
    cpu.mmu.write(1, 0x50); // LD D, B
    for _ in 0..=1 {
        cpu.step();
    }

    assert_eq!(cpu.registers.d, 1);
    assert_eq!(cpu.registers.b, 1);
}
