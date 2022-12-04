// Initially borrowed from: https://github.com/rylev/DMG-01 - modified for this project.

use std;

pub fn is_word_target(target: InstructionTarget) -> bool {
    return match target {
        InstructionTarget::BC
        | InstructionTarget::DE
        | InstructionTarget::HL
        | InstructionTarget::SP => true,
        _ => false,
    };
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum InstructionTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    D8,
    HLI,
    BC,
    DE,
    HL,
    SP,
    AF
}
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ADDHLTarget {
    BC,
    DE,
    HL,
    SP,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum JumpTest {
    NotZero,
    NotCarry,
    Zero,
    Carry,
    Always,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Indirect {
    BCIndirect,
    DEIndirect,
    HLIndirectMinus,
    HLIndirectPlus,
    WordIndirect,
    LastByteIndirect,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadType {
    Byte(InstructionTarget, InstructionTarget),
    Word(InstructionTarget),
    AFromIndirect(Indirect),
    IndirectFromA(Indirect),
    AFromByteAddress,
    ByteAddressFromA,
    SPFromHL,
    HLFromSPN,
    IndirectFromSP,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BitPosition {
    B0,
    B1,
    B2,
    B3,
    B4,
    B5,
    B6,
    B7,
}
impl std::convert::From<BitPosition> for u8 {
    fn from(position: BitPosition) -> u8 {
        match position {
            BitPosition::B0 => 0,
            BitPosition::B1 => 1,
            BitPosition::B2 => 2,
            BitPosition::B3 => 3,
            BitPosition::B4 => 4,
            BitPosition::B5 => 5,
            BitPosition::B6 => 6,
            BitPosition::B7 => 7,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RSTLocation {
    X00,
    X08,
    X10,
    X18,
    X20,
    X28,
    X30,
    X38,
}

impl RSTLocation {
    pub fn to_hex(&self) -> u16 {
        match self {
            RSTLocation::X00 => 0x00,
            RSTLocation::X08 => 0x08,
            RSTLocation::X10 => 0x10,
            RSTLocation::X18 => 0x18,
            RSTLocation::X20 => 0x20,
            RSTLocation::X28 => 0x28,
            RSTLocation::X30 => 0x30,
            RSTLocation::X38 => 0x38,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Instruction {
    // Arithmetic Instructions
    INC(InstructionTarget),
    DEC(InstructionTarget),

    ADD(InstructionTarget),
    ADC(InstructionTarget),
    ADDHL(ADDHLTarget),
    ADDSP,
    SUB(InstructionTarget),
    SBC(InstructionTarget),
    AND(InstructionTarget),
    OR(InstructionTarget),
    XOR(InstructionTarget),
    CP(InstructionTarget),

    CCF,
    SCF,

    RRA,
    RLA,
    RRCA,
    RLCA,
    CPL,
    DAA,

    // Prefix Instructions - all of these use a single byte
    BIT(InstructionTarget, BitPosition),
    RES(InstructionTarget, BitPosition),
    SET(InstructionTarget, BitPosition),
    SRL(InstructionTarget),
    RR(InstructionTarget),
    RL(InstructionTarget),
    RRC(InstructionTarget),
    RLC(InstructionTarget),
    SRA(InstructionTarget),
    SLA(InstructionTarget),
    SWAP(InstructionTarget),

    // Jump Instructions
    JP(JumpTest),
    JR(JumpTest),
    JPI,

    // Load Instructions
    LD(LoadType),

    // Stack Instructions
    PUSH(InstructionTarget),
    POP(InstructionTarget),
    CALL(JumpTest),
    RET(JumpTest),
    RETI,
    RST(RSTLocation),

    // Control Instructions
    HALT,
    NOP,
    DI,
    EI,
}

impl Instruction {
    pub fn from_byte(byte: u8, prefixed: bool) -> Option<Instruction> {
        if prefixed {
            Instruction::from_byte_prefixed(byte)
        } else {
            Instruction::from_byte_not_prefixed(byte)
        }
    }

    fn from_byte_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x00 => Some(Instruction::RLC(InstructionTarget::B)),
            0x01 => Some(Instruction::RLC(InstructionTarget::C)),
            0x02 => Some(Instruction::RLC(InstructionTarget::D)),
            0x03 => Some(Instruction::RLC(InstructionTarget::E)),
            0x04 => Some(Instruction::RLC(InstructionTarget::H)),
            0x05 => Some(Instruction::RLC(InstructionTarget::L)),
            0x06 => Some(Instruction::RLC(InstructionTarget::HLI)),
            0x07 => Some(Instruction::RLC(InstructionTarget::A)),

            0x08 => Some(Instruction::RRC(InstructionTarget::B)),
            0x09 => Some(Instruction::RRC(InstructionTarget::C)),
            0x0a => Some(Instruction::RRC(InstructionTarget::D)),
            0x0b => Some(Instruction::RRC(InstructionTarget::E)),
            0x0c => Some(Instruction::RRC(InstructionTarget::H)),
            0x0d => Some(Instruction::RRC(InstructionTarget::L)),
            0x0e => Some(Instruction::RRC(InstructionTarget::HLI)),
            0x0f => Some(Instruction::RRC(InstructionTarget::A)),

            0x10 => Some(Instruction::RL(InstructionTarget::B)),
            0x11 => Some(Instruction::RL(InstructionTarget::C)),
            0x12 => Some(Instruction::RL(InstructionTarget::D)),
            0x13 => Some(Instruction::RL(InstructionTarget::E)),
            0x14 => Some(Instruction::RL(InstructionTarget::H)),
            0x15 => Some(Instruction::RL(InstructionTarget::L)),
            0x16 => Some(Instruction::RL(InstructionTarget::HLI)),
            0x17 => Some(Instruction::RL(InstructionTarget::A)),

            0x18 => Some(Instruction::RR(InstructionTarget::B)),
            0x19 => Some(Instruction::RR(InstructionTarget::C)),
            0x1a => Some(Instruction::RR(InstructionTarget::D)),
            0x1b => Some(Instruction::RR(InstructionTarget::E)),
            0x1c => Some(Instruction::RR(InstructionTarget::H)),
            0x1d => Some(Instruction::RR(InstructionTarget::L)),
            0x1e => Some(Instruction::RR(InstructionTarget::HLI)),
            0x1f => Some(Instruction::RR(InstructionTarget::A)),

            0x20 => Some(Instruction::SLA(InstructionTarget::B)),
            0x21 => Some(Instruction::SLA(InstructionTarget::C)),
            0x22 => Some(Instruction::SLA(InstructionTarget::D)),
            0x23 => Some(Instruction::SLA(InstructionTarget::E)),
            0x24 => Some(Instruction::SLA(InstructionTarget::H)),
            0x25 => Some(Instruction::SLA(InstructionTarget::L)),
            0x26 => Some(Instruction::SLA(InstructionTarget::HLI)),
            0x27 => Some(Instruction::SLA(InstructionTarget::A)),

            0x28 => Some(Instruction::SRA(InstructionTarget::B)),
            0x29 => Some(Instruction::SRA(InstructionTarget::C)),
            0x2a => Some(Instruction::SRA(InstructionTarget::D)),
            0x2b => Some(Instruction::SRA(InstructionTarget::E)),
            0x2c => Some(Instruction::SRA(InstructionTarget::H)),
            0x2d => Some(Instruction::SRA(InstructionTarget::L)),
            0x2e => Some(Instruction::SRA(InstructionTarget::HLI)),
            0x2f => Some(Instruction::SRA(InstructionTarget::A)),

            0x30 => Some(Instruction::SWAP(InstructionTarget::B)),
            0x31 => Some(Instruction::SWAP(InstructionTarget::C)),
            0x32 => Some(Instruction::SWAP(InstructionTarget::D)),
            0x33 => Some(Instruction::SWAP(InstructionTarget::E)),
            0x34 => Some(Instruction::SWAP(InstructionTarget::H)),
            0x35 => Some(Instruction::SWAP(InstructionTarget::L)),
            0x36 => Some(Instruction::SWAP(InstructionTarget::HLI)),
            0x37 => Some(Instruction::SWAP(InstructionTarget::A)),

            0x38 => Some(Instruction::SRL(InstructionTarget::B)),
            0x39 => Some(Instruction::SRL(InstructionTarget::C)),
            0x3a => Some(Instruction::SRL(InstructionTarget::D)),
            0x3b => Some(Instruction::SRL(InstructionTarget::E)),
            0x3c => Some(Instruction::SRL(InstructionTarget::H)),
            0x3d => Some(Instruction::SRL(InstructionTarget::L)),
            0x3e => Some(Instruction::SRL(InstructionTarget::HLI)),
            0x3f => Some(Instruction::SRL(InstructionTarget::A)),

            0x40 => Some(Instruction::BIT(InstructionTarget::B, BitPosition::B0)),
            0x41 => Some(Instruction::BIT(InstructionTarget::C, BitPosition::B0)),
            0x42 => Some(Instruction::BIT(InstructionTarget::D, BitPosition::B0)),
            0x43 => Some(Instruction::BIT(InstructionTarget::E, BitPosition::B0)),
            0x44 => Some(Instruction::BIT(InstructionTarget::H, BitPosition::B0)),
            0x45 => Some(Instruction::BIT(InstructionTarget::L, BitPosition::B0)),
            0x46 => Some(Instruction::BIT(InstructionTarget::HLI, BitPosition::B0)),
            0x47 => Some(Instruction::BIT(InstructionTarget::A, BitPosition::B0)),

            0x48 => Some(Instruction::BIT(InstructionTarget::B, BitPosition::B1)),
            0x49 => Some(Instruction::BIT(InstructionTarget::C, BitPosition::B1)),
            0x4a => Some(Instruction::BIT(InstructionTarget::D, BitPosition::B1)),
            0x4b => Some(Instruction::BIT(InstructionTarget::E, BitPosition::B1)),
            0x4c => Some(Instruction::BIT(InstructionTarget::H, BitPosition::B1)),
            0x4d => Some(Instruction::BIT(InstructionTarget::L, BitPosition::B1)),
            0x4e => Some(Instruction::BIT(InstructionTarget::HLI, BitPosition::B1)),
            0x4f => Some(Instruction::BIT(InstructionTarget::A, BitPosition::B1)),

            0x50 => Some(Instruction::BIT(InstructionTarget::B, BitPosition::B2)),
            0x51 => Some(Instruction::BIT(InstructionTarget::C, BitPosition::B2)),
            0x52 => Some(Instruction::BIT(InstructionTarget::D, BitPosition::B2)),
            0x53 => Some(Instruction::BIT(InstructionTarget::E, BitPosition::B2)),
            0x54 => Some(Instruction::BIT(InstructionTarget::H, BitPosition::B2)),
            0x55 => Some(Instruction::BIT(InstructionTarget::L, BitPosition::B2)),
            0x56 => Some(Instruction::BIT(InstructionTarget::HLI, BitPosition::B2)),
            0x57 => Some(Instruction::BIT(InstructionTarget::A, BitPosition::B2)),

            0x58 => Some(Instruction::BIT(InstructionTarget::B, BitPosition::B3)),
            0x59 => Some(Instruction::BIT(InstructionTarget::C, BitPosition::B3)),
            0x5a => Some(Instruction::BIT(InstructionTarget::D, BitPosition::B3)),
            0x5b => Some(Instruction::BIT(InstructionTarget::E, BitPosition::B3)),
            0x5c => Some(Instruction::BIT(InstructionTarget::H, BitPosition::B3)),
            0x5d => Some(Instruction::BIT(InstructionTarget::L, BitPosition::B3)),
            0x5e => Some(Instruction::BIT(InstructionTarget::HLI, BitPosition::B3)),
            0x5f => Some(Instruction::BIT(InstructionTarget::A, BitPosition::B3)),

            0x60 => Some(Instruction::BIT(InstructionTarget::B, BitPosition::B4)),
            0x61 => Some(Instruction::BIT(InstructionTarget::C, BitPosition::B4)),
            0x62 => Some(Instruction::BIT(InstructionTarget::D, BitPosition::B4)),
            0x63 => Some(Instruction::BIT(InstructionTarget::E, BitPosition::B4)),
            0x64 => Some(Instruction::BIT(InstructionTarget::H, BitPosition::B4)),
            0x65 => Some(Instruction::BIT(InstructionTarget::L, BitPosition::B4)),
            0x66 => Some(Instruction::BIT(InstructionTarget::HLI, BitPosition::B4)),
            0x67 => Some(Instruction::BIT(InstructionTarget::A, BitPosition::B4)),

            0x68 => Some(Instruction::BIT(InstructionTarget::B, BitPosition::B5)),
            0x69 => Some(Instruction::BIT(InstructionTarget::C, BitPosition::B5)),
            0x6a => Some(Instruction::BIT(InstructionTarget::D, BitPosition::B5)),
            0x6b => Some(Instruction::BIT(InstructionTarget::E, BitPosition::B5)),
            0x6c => Some(Instruction::BIT(InstructionTarget::H, BitPosition::B5)),
            0x6d => Some(Instruction::BIT(InstructionTarget::L, BitPosition::B5)),
            0x6e => Some(Instruction::BIT(InstructionTarget::HLI, BitPosition::B5)),
            0x6f => Some(Instruction::BIT(InstructionTarget::A, BitPosition::B5)),

            0x70 => Some(Instruction::BIT(InstructionTarget::B, BitPosition::B6)),
            0x71 => Some(Instruction::BIT(InstructionTarget::C, BitPosition::B6)),
            0x72 => Some(Instruction::BIT(InstructionTarget::D, BitPosition::B6)),
            0x73 => Some(Instruction::BIT(InstructionTarget::E, BitPosition::B6)),
            0x74 => Some(Instruction::BIT(InstructionTarget::H, BitPosition::B6)),
            0x75 => Some(Instruction::BIT(InstructionTarget::L, BitPosition::B6)),
            0x76 => Some(Instruction::BIT(InstructionTarget::HLI, BitPosition::B6)),
            0x77 => Some(Instruction::BIT(InstructionTarget::A, BitPosition::B6)),

            0x78 => Some(Instruction::BIT(InstructionTarget::B, BitPosition::B7)),
            0x79 => Some(Instruction::BIT(InstructionTarget::C, BitPosition::B7)),
            0x7a => Some(Instruction::BIT(InstructionTarget::D, BitPosition::B7)),
            0x7b => Some(Instruction::BIT(InstructionTarget::E, BitPosition::B7)),
            0x7c => Some(Instruction::BIT(InstructionTarget::H, BitPosition::B7)),
            0x7d => Some(Instruction::BIT(InstructionTarget::L, BitPosition::B7)),
            0x7e => Some(Instruction::BIT(InstructionTarget::HLI, BitPosition::B7)),
            0x7f => Some(Instruction::BIT(InstructionTarget::A, BitPosition::B7)),

            0x80 => Some(Instruction::RES(InstructionTarget::B, BitPosition::B0)),
            0x81 => Some(Instruction::RES(InstructionTarget::C, BitPosition::B0)),
            0x82 => Some(Instruction::RES(InstructionTarget::D, BitPosition::B0)),
            0x83 => Some(Instruction::RES(InstructionTarget::E, BitPosition::B0)),
            0x84 => Some(Instruction::RES(InstructionTarget::H, BitPosition::B0)),
            0x85 => Some(Instruction::RES(InstructionTarget::L, BitPosition::B0)),
            0x86 => Some(Instruction::RES(InstructionTarget::HLI, BitPosition::B0)),
            0x87 => Some(Instruction::RES(InstructionTarget::A, BitPosition::B0)),

            0x88 => Some(Instruction::RES(InstructionTarget::B, BitPosition::B1)),
            0x89 => Some(Instruction::RES(InstructionTarget::C, BitPosition::B1)),
            0x8a => Some(Instruction::RES(InstructionTarget::D, BitPosition::B1)),
            0x8b => Some(Instruction::RES(InstructionTarget::E, BitPosition::B1)),
            0x8c => Some(Instruction::RES(InstructionTarget::H, BitPosition::B1)),
            0x8d => Some(Instruction::RES(InstructionTarget::L, BitPosition::B1)),
            0x8e => Some(Instruction::RES(InstructionTarget::HLI, BitPosition::B1)),
            0x8f => Some(Instruction::RES(InstructionTarget::A, BitPosition::B1)),

            0x90 => Some(Instruction::RES(InstructionTarget::B, BitPosition::B2)),
            0x91 => Some(Instruction::RES(InstructionTarget::C, BitPosition::B2)),
            0x92 => Some(Instruction::RES(InstructionTarget::D, BitPosition::B2)),
            0x93 => Some(Instruction::RES(InstructionTarget::E, BitPosition::B2)),
            0x94 => Some(Instruction::RES(InstructionTarget::H, BitPosition::B2)),
            0x95 => Some(Instruction::RES(InstructionTarget::L, BitPosition::B2)),
            0x96 => Some(Instruction::RES(InstructionTarget::HLI, BitPosition::B2)),
            0x97 => Some(Instruction::RES(InstructionTarget::A, BitPosition::B2)),

            0x98 => Some(Instruction::RES(InstructionTarget::B, BitPosition::B3)),
            0x99 => Some(Instruction::RES(InstructionTarget::C, BitPosition::B3)),
            0x9a => Some(Instruction::RES(InstructionTarget::D, BitPosition::B3)),
            0x9b => Some(Instruction::RES(InstructionTarget::E, BitPosition::B3)),
            0x9c => Some(Instruction::RES(InstructionTarget::H, BitPosition::B3)),
            0x9d => Some(Instruction::RES(InstructionTarget::L, BitPosition::B3)),
            0x9e => Some(Instruction::RES(InstructionTarget::HLI, BitPosition::B3)),
            0x9f => Some(Instruction::RES(InstructionTarget::A, BitPosition::B3)),

            0xa0 => Some(Instruction::RES(InstructionTarget::B, BitPosition::B4)),
            0xa1 => Some(Instruction::RES(InstructionTarget::C, BitPosition::B4)),
            0xa2 => Some(Instruction::RES(InstructionTarget::D, BitPosition::B4)),
            0xa3 => Some(Instruction::RES(InstructionTarget::E, BitPosition::B4)),
            0xa4 => Some(Instruction::RES(InstructionTarget::H, BitPosition::B4)),
            0xa5 => Some(Instruction::RES(InstructionTarget::L, BitPosition::B4)),
            0xa6 => Some(Instruction::RES(InstructionTarget::HLI, BitPosition::B4)),
            0xa7 => Some(Instruction::RES(InstructionTarget::A, BitPosition::B4)),

            0xa8 => Some(Instruction::RES(InstructionTarget::B, BitPosition::B5)),
            0xa9 => Some(Instruction::RES(InstructionTarget::C, BitPosition::B5)),
            0xaa => Some(Instruction::RES(InstructionTarget::D, BitPosition::B5)),
            0xab => Some(Instruction::RES(InstructionTarget::E, BitPosition::B5)),
            0xac => Some(Instruction::RES(InstructionTarget::H, BitPosition::B5)),
            0xad => Some(Instruction::RES(InstructionTarget::L, BitPosition::B5)),
            0xae => Some(Instruction::RES(InstructionTarget::HLI, BitPosition::B5)),
            0xaf => Some(Instruction::RES(InstructionTarget::A, BitPosition::B5)),

            0xb0 => Some(Instruction::RES(InstructionTarget::B, BitPosition::B6)),
            0xb1 => Some(Instruction::RES(InstructionTarget::C, BitPosition::B6)),
            0xb2 => Some(Instruction::RES(InstructionTarget::D, BitPosition::B6)),
            0xb3 => Some(Instruction::RES(InstructionTarget::E, BitPosition::B6)),
            0xb4 => Some(Instruction::RES(InstructionTarget::H, BitPosition::B6)),
            0xb5 => Some(Instruction::RES(InstructionTarget::L, BitPosition::B6)),
            0xb6 => Some(Instruction::RES(InstructionTarget::HLI, BitPosition::B6)),
            0xb7 => Some(Instruction::RES(InstructionTarget::A, BitPosition::B6)),

            0xb8 => Some(Instruction::RES(InstructionTarget::B, BitPosition::B7)),
            0xb9 => Some(Instruction::RES(InstructionTarget::C, BitPosition::B7)),
            0xba => Some(Instruction::RES(InstructionTarget::D, BitPosition::B7)),
            0xbb => Some(Instruction::RES(InstructionTarget::E, BitPosition::B7)),
            0xbc => Some(Instruction::RES(InstructionTarget::H, BitPosition::B7)),
            0xbd => Some(Instruction::RES(InstructionTarget::L, BitPosition::B7)),
            0xbe => Some(Instruction::RES(InstructionTarget::HLI, BitPosition::B7)),
            0xbf => Some(Instruction::RES(InstructionTarget::A, BitPosition::B7)),

            0xc0 => Some(Instruction::SET(InstructionTarget::B, BitPosition::B0)),
            0xc1 => Some(Instruction::SET(InstructionTarget::C, BitPosition::B0)),
            0xc2 => Some(Instruction::SET(InstructionTarget::D, BitPosition::B0)),
            0xc3 => Some(Instruction::SET(InstructionTarget::E, BitPosition::B0)),
            0xc4 => Some(Instruction::SET(InstructionTarget::H, BitPosition::B0)),
            0xc5 => Some(Instruction::SET(InstructionTarget::L, BitPosition::B0)),
            0xc6 => Some(Instruction::SET(InstructionTarget::HLI, BitPosition::B0)),
            0xc7 => Some(Instruction::SET(InstructionTarget::A, BitPosition::B0)),

            0xc8 => Some(Instruction::SET(InstructionTarget::B, BitPosition::B1)),
            0xc9 => Some(Instruction::SET(InstructionTarget::C, BitPosition::B1)),
            0xca => Some(Instruction::SET(InstructionTarget::D, BitPosition::B1)),
            0xcb => Some(Instruction::SET(InstructionTarget::E, BitPosition::B1)),
            0xcc => Some(Instruction::SET(InstructionTarget::H, BitPosition::B1)),
            0xcd => Some(Instruction::SET(InstructionTarget::L, BitPosition::B1)),
            0xce => Some(Instruction::SET(InstructionTarget::HLI, BitPosition::B1)),
            0xcf => Some(Instruction::SET(InstructionTarget::A, BitPosition::B1)),

            0xd0 => Some(Instruction::SET(InstructionTarget::B, BitPosition::B2)),
            0xd1 => Some(Instruction::SET(InstructionTarget::C, BitPosition::B2)),
            0xd2 => Some(Instruction::SET(InstructionTarget::D, BitPosition::B2)),
            0xd3 => Some(Instruction::SET(InstructionTarget::E, BitPosition::B2)),
            0xd4 => Some(Instruction::SET(InstructionTarget::H, BitPosition::B2)),
            0xd5 => Some(Instruction::SET(InstructionTarget::L, BitPosition::B2)),
            0xd6 => Some(Instruction::SET(InstructionTarget::HLI, BitPosition::B2)),
            0xd7 => Some(Instruction::SET(InstructionTarget::A, BitPosition::B2)),

            0xd8 => Some(Instruction::SET(InstructionTarget::B, BitPosition::B3)),
            0xd9 => Some(Instruction::SET(InstructionTarget::C, BitPosition::B3)),
            0xda => Some(Instruction::SET(InstructionTarget::D, BitPosition::B3)),
            0xdb => Some(Instruction::SET(InstructionTarget::E, BitPosition::B3)),
            0xdc => Some(Instruction::SET(InstructionTarget::H, BitPosition::B3)),
            0xdd => Some(Instruction::SET(InstructionTarget::L, BitPosition::B3)),
            0xde => Some(Instruction::SET(InstructionTarget::HLI, BitPosition::B3)),
            0xdf => Some(Instruction::SET(InstructionTarget::A, BitPosition::B3)),

            0xe0 => Some(Instruction::SET(InstructionTarget::B, BitPosition::B4)),
            0xe1 => Some(Instruction::SET(InstructionTarget::C, BitPosition::B4)),
            0xe2 => Some(Instruction::SET(InstructionTarget::D, BitPosition::B4)),
            0xe3 => Some(Instruction::SET(InstructionTarget::E, BitPosition::B4)),
            0xe4 => Some(Instruction::SET(InstructionTarget::H, BitPosition::B4)),
            0xe5 => Some(Instruction::SET(InstructionTarget::L, BitPosition::B4)),
            0xe6 => Some(Instruction::SET(InstructionTarget::HLI, BitPosition::B4)),
            0xe7 => Some(Instruction::SET(InstructionTarget::A, BitPosition::B4)),

            0xe8 => Some(Instruction::SET(InstructionTarget::B, BitPosition::B5)),
            0xe9 => Some(Instruction::SET(InstructionTarget::C, BitPosition::B5)),
            0xea => Some(Instruction::SET(InstructionTarget::D, BitPosition::B5)),
            0xeb => Some(Instruction::SET(InstructionTarget::E, BitPosition::B5)),
            0xec => Some(Instruction::SET(InstructionTarget::H, BitPosition::B5)),
            0xed => Some(Instruction::SET(InstructionTarget::L, BitPosition::B5)),
            0xee => Some(Instruction::SET(InstructionTarget::HLI, BitPosition::B5)),
            0xef => Some(Instruction::SET(InstructionTarget::A, BitPosition::B5)),

            0xf0 => Some(Instruction::SET(InstructionTarget::B, BitPosition::B6)),
            0xf1 => Some(Instruction::SET(InstructionTarget::C, BitPosition::B6)),
            0xf2 => Some(Instruction::SET(InstructionTarget::D, BitPosition::B6)),
            0xf3 => Some(Instruction::SET(InstructionTarget::E, BitPosition::B6)),
            0xf4 => Some(Instruction::SET(InstructionTarget::H, BitPosition::B6)),
            0xf5 => Some(Instruction::SET(InstructionTarget::L, BitPosition::B6)),
            0xf6 => Some(Instruction::SET(InstructionTarget::HLI, BitPosition::B6)),
            0xf7 => Some(Instruction::SET(InstructionTarget::A, BitPosition::B6)),

            0xf8 => Some(Instruction::SET(InstructionTarget::B, BitPosition::B7)),
            0xf9 => Some(Instruction::SET(InstructionTarget::C, BitPosition::B7)),
            0xfa => Some(Instruction::SET(InstructionTarget::D, BitPosition::B7)),
            0xfb => Some(Instruction::SET(InstructionTarget::E, BitPosition::B7)),
            0xfc => Some(Instruction::SET(InstructionTarget::H, BitPosition::B7)),
            0xfd => Some(Instruction::SET(InstructionTarget::L, BitPosition::B7)),
            0xfe => Some(Instruction::SET(InstructionTarget::HLI, BitPosition::B7)),
            0xff => Some(Instruction::SET(InstructionTarget::A, BitPosition::B7)),
        }
    }

    fn from_byte_not_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x3c => Some(Instruction::INC(InstructionTarget::A)),
            0x04 => Some(Instruction::INC(InstructionTarget::B)),
            0x14 => Some(Instruction::INC(InstructionTarget::D)),
            0x24 => Some(Instruction::INC(InstructionTarget::H)),
            0x0c => Some(Instruction::INC(InstructionTarget::C)),
            0x1c => Some(Instruction::INC(InstructionTarget::E)),
            0x2c => Some(Instruction::INC(InstructionTarget::L)),
            0x34 => Some(Instruction::INC(InstructionTarget::HLI)),
            0x03 => Some(Instruction::INC(InstructionTarget::BC)),
            0x13 => Some(Instruction::INC(InstructionTarget::DE)),
            0x23 => Some(Instruction::INC(InstructionTarget::HL)),
            0x33 => Some(Instruction::INC(InstructionTarget::SP)),

            0x3d => Some(Instruction::DEC(InstructionTarget::A)),
            0x05 => Some(Instruction::DEC(InstructionTarget::B)),
            0x0d => Some(Instruction::DEC(InstructionTarget::C)),
            0x15 => Some(Instruction::DEC(InstructionTarget::D)),
            0x1d => Some(Instruction::DEC(InstructionTarget::E)),
            0x25 => Some(Instruction::DEC(InstructionTarget::H)),
            0x2d => Some(Instruction::DEC(InstructionTarget::L)),
            0x35 => Some(Instruction::DEC(InstructionTarget::HLI)),
            0x0b => Some(Instruction::DEC(InstructionTarget::BC)),
            0x1b => Some(Instruction::DEC(InstructionTarget::DE)),
            0x2b => Some(Instruction::DEC(InstructionTarget::HL)),
            0x3b => Some(Instruction::DEC(InstructionTarget::SP)),

            0x87 => Some(Instruction::ADD(InstructionTarget::A)),
            0x80 => Some(Instruction::ADD(InstructionTarget::B)),
            0x81 => Some(Instruction::ADD(InstructionTarget::C)),
            0x82 => Some(Instruction::ADD(InstructionTarget::D)),
            0x83 => Some(Instruction::ADD(InstructionTarget::E)),
            0x84 => Some(Instruction::ADD(InstructionTarget::H)),
            0x85 => Some(Instruction::ADD(InstructionTarget::L)),
            0x86 => Some(Instruction::ADD(InstructionTarget::HLI)),
            0xc6 => Some(Instruction::ADD(InstructionTarget::D8)),

            0x09 => Some(Instruction::ADDHL(ADDHLTarget::BC)),
            0x19 => Some(Instruction::ADDHL(ADDHLTarget::DE)),
            0x29 => Some(Instruction::ADDHL(ADDHLTarget::HL)),
            0x39 => Some(Instruction::ADDHL(ADDHLTarget::SP)),

            0x8f => Some(Instruction::ADC(InstructionTarget::A)),
            0x88 => Some(Instruction::ADC(InstructionTarget::B)),
            0x89 => Some(Instruction::ADC(InstructionTarget::C)),
            0x8a => Some(Instruction::ADC(InstructionTarget::D)),
            0x8b => Some(Instruction::ADC(InstructionTarget::E)),
            0x8c => Some(Instruction::ADC(InstructionTarget::H)),
            0x8d => Some(Instruction::ADC(InstructionTarget::L)),
            0x8e => Some(Instruction::ADC(InstructionTarget::HLI)),
            0xce => Some(Instruction::ADC(InstructionTarget::D8)),

            0x97 => Some(Instruction::SUB(InstructionTarget::A)),
            0x90 => Some(Instruction::SUB(InstructionTarget::B)),
            0x91 => Some(Instruction::SUB(InstructionTarget::C)),
            0x92 => Some(Instruction::SUB(InstructionTarget::D)),
            0x93 => Some(Instruction::SUB(InstructionTarget::E)),
            0x94 => Some(Instruction::SUB(InstructionTarget::H)),
            0x95 => Some(Instruction::SUB(InstructionTarget::L)),
            0x96 => Some(Instruction::SUB(InstructionTarget::HLI)),
            0xd6 => Some(Instruction::SUB(InstructionTarget::D8)),

            0x9f => Some(Instruction::SBC(InstructionTarget::A)),
            0x98 => Some(Instruction::SBC(InstructionTarget::B)),
            0x99 => Some(Instruction::SBC(InstructionTarget::C)),
            0x9a => Some(Instruction::SBC(InstructionTarget::D)),
            0x9b => Some(Instruction::SBC(InstructionTarget::E)),
            0x9c => Some(Instruction::SBC(InstructionTarget::H)),
            0x9d => Some(Instruction::SBC(InstructionTarget::L)),
            0x9e => Some(Instruction::SBC(InstructionTarget::HLI)),
            0xde => Some(Instruction::SBC(InstructionTarget::D8)),

            0xa7 => Some(Instruction::AND(InstructionTarget::A)),
            0xa0 => Some(Instruction::AND(InstructionTarget::B)),
            0xa1 => Some(Instruction::AND(InstructionTarget::C)),
            0xa2 => Some(Instruction::AND(InstructionTarget::D)),
            0xa3 => Some(Instruction::AND(InstructionTarget::E)),
            0xa4 => Some(Instruction::AND(InstructionTarget::H)),
            0xa5 => Some(Instruction::AND(InstructionTarget::L)),
            0xa6 => Some(Instruction::AND(InstructionTarget::HLI)),
            0xe6 => Some(Instruction::AND(InstructionTarget::D8)),

            0xb7 => Some(Instruction::OR(InstructionTarget::A)),
            0xb0 => Some(Instruction::OR(InstructionTarget::B)),
            0xb1 => Some(Instruction::OR(InstructionTarget::C)),
            0xb2 => Some(Instruction::OR(InstructionTarget::D)),
            0xb3 => Some(Instruction::OR(InstructionTarget::E)),
            0xb4 => Some(Instruction::OR(InstructionTarget::H)),
            0xb5 => Some(Instruction::OR(InstructionTarget::L)),
            0xb6 => Some(Instruction::OR(InstructionTarget::HLI)),
            0xf6 => Some(Instruction::OR(InstructionTarget::D8)),

            0xaf => Some(Instruction::XOR(InstructionTarget::A)),
            0xa8 => Some(Instruction::XOR(InstructionTarget::B)),
            0xa9 => Some(Instruction::XOR(InstructionTarget::C)),
            0xaa => Some(Instruction::XOR(InstructionTarget::D)),
            0xab => Some(Instruction::XOR(InstructionTarget::E)),
            0xac => Some(Instruction::XOR(InstructionTarget::H)),
            0xad => Some(Instruction::XOR(InstructionTarget::L)),
            0xae => Some(Instruction::XOR(InstructionTarget::HLI)),
            0xee => Some(Instruction::XOR(InstructionTarget::D8)),

            0xbf => Some(Instruction::CP(InstructionTarget::A)),
            0xb8 => Some(Instruction::CP(InstructionTarget::B)),
            0xb9 => Some(Instruction::CP(InstructionTarget::C)),
            0xba => Some(Instruction::CP(InstructionTarget::D)),
            0xbb => Some(Instruction::CP(InstructionTarget::E)),
            0xbc => Some(Instruction::CP(InstructionTarget::H)),
            0xbd => Some(Instruction::CP(InstructionTarget::L)),
            0xbe => Some(Instruction::CP(InstructionTarget::HLI)),
            0xfe => Some(Instruction::CP(InstructionTarget::D8)),

            0xe8 => Some(Instruction::ADDSP),

            0x3f => Some(Instruction::CCF),
            0x37 => Some(Instruction::SCF),
            0x1f => Some(Instruction::RRA),
            0x17 => Some(Instruction::RLA),
            0x0f => Some(Instruction::RRCA),
            0x07 => Some(Instruction::RLCA),
            0x2f => Some(Instruction::CPL),

            0x27 => Some(Instruction::DAA),

            0xc3 => Some(Instruction::JP(JumpTest::Always)),
            0xc2 => Some(Instruction::JP(JumpTest::NotZero)),
            0xd2 => Some(Instruction::JP(JumpTest::NotCarry)),
            0xca => Some(Instruction::JP(JumpTest::Zero)),
            0xda => Some(Instruction::JP(JumpTest::Carry)),

            0x18 => Some(Instruction::JR(JumpTest::Always)),
            0x28 => Some(Instruction::JR(JumpTest::Zero)),
            0x38 => Some(Instruction::JR(JumpTest::Carry)),
            0x20 => Some(Instruction::JR(JumpTest::NotZero)),
            0x30 => Some(Instruction::JR(JumpTest::NotCarry)),

            0xe9 => Some(Instruction::JPI),

            0xf2 => Some(Instruction::LD(LoadType::AFromIndirect(
                Indirect::LastByteIndirect,
            ))),
            0x0a => Some(Instruction::LD(LoadType::AFromIndirect(
                Indirect::BCIndirect,
            ))),
            0x1a => Some(Instruction::LD(LoadType::AFromIndirect(
                Indirect::DEIndirect,
            ))),
            0x2a => Some(Instruction::LD(LoadType::AFromIndirect(
                Indirect::HLIndirectPlus,
            ))),
            0x3a => Some(Instruction::LD(LoadType::AFromIndirect(
                Indirect::HLIndirectMinus,
            ))),
            0xfa => Some(Instruction::LD(LoadType::AFromIndirect(
                Indirect::WordIndirect,
            ))),

            0xe2 => Some(Instruction::LD(LoadType::IndirectFromA(
                Indirect::LastByteIndirect,
            ))),
            0x02 => Some(Instruction::LD(LoadType::IndirectFromA(
                Indirect::BCIndirect,
            ))),
            0x12 => Some(Instruction::LD(LoadType::IndirectFromA(
                Indirect::DEIndirect,
            ))),
            0x22 => Some(Instruction::LD(LoadType::IndirectFromA(
                Indirect::HLIndirectPlus,
            ))),
            0x32 => Some(Instruction::LD(LoadType::IndirectFromA(
                Indirect::HLIndirectMinus,
            ))),
            0xea => Some(Instruction::LD(LoadType::IndirectFromA(
                Indirect::WordIndirect,
            ))),

            0x01 => Some(Instruction::LD(LoadType::Word(InstructionTarget::BC))),
            0x11 => Some(Instruction::LD(LoadType::Word(InstructionTarget::DE))),
            0x21 => Some(Instruction::LD(LoadType::Word(InstructionTarget::HL))),
            0x31 => Some(Instruction::LD(LoadType::Word(InstructionTarget::SP))),

            0x40 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::B,
                InstructionTarget::B,
            ))),
            0x41 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::B,
                InstructionTarget::C,
            ))),
            0x42 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::B,
                InstructionTarget::D,
            ))),
            0x43 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::B,
                InstructionTarget::E,
            ))),
            0x44 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::B,
                InstructionTarget::H,
            ))),
            0x45 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::B,
                InstructionTarget::L,
            ))),
            0x46 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::B,
                InstructionTarget::HLI,
            ))),
            0x47 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::B,
                InstructionTarget::A,
            ))),

            0x48 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::C,
                InstructionTarget::B,
            ))),
            0x49 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::C,
                InstructionTarget::C,
            ))),
            0x4a => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::C,
                InstructionTarget::D,
            ))),
            0x4b => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::C,
                InstructionTarget::E,
            ))),
            0x4c => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::C,
                InstructionTarget::H,
            ))),
            0x4d => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::C,
                InstructionTarget::L,
            ))),
            0x4e => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::C,
                InstructionTarget::HLI,
            ))),
            0x4f => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::C,
                InstructionTarget::A,
            ))),

            0x50 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::D,
                InstructionTarget::B,
            ))),
            0x51 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::D,
                InstructionTarget::C,
            ))),
            0x52 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::D,
                InstructionTarget::D,
            ))),
            0x53 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::D,
                InstructionTarget::E,
            ))),
            0x54 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::D,
                InstructionTarget::H,
            ))),
            0x55 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::D,
                InstructionTarget::L,
            ))),
            0x56 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::D,
                InstructionTarget::HLI,
            ))),
            0x57 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::D,
                InstructionTarget::A,
            ))),

            0x58 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::E,
                InstructionTarget::B,
            ))),
            0x59 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::E,
                InstructionTarget::C,
            ))),
            0x5a => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::E,
                InstructionTarget::D,
            ))),
            0x5b => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::E,
                InstructionTarget::E,
            ))),
            0x5c => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::E,
                InstructionTarget::H,
            ))),
            0x5d => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::E,
                InstructionTarget::L,
            ))),
            0x5e => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::E,
                InstructionTarget::HLI,
            ))),
            0x5f => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::E,
                InstructionTarget::A,
            ))),

            0x60 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::H,
                InstructionTarget::B,
            ))),
            0x61 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::H,
                InstructionTarget::C,
            ))),
            0x62 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::H,
                InstructionTarget::D,
            ))),
            0x63 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::H,
                InstructionTarget::E,
            ))),
            0x64 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::H,
                InstructionTarget::H,
            ))),
            0x65 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::H,
                InstructionTarget::L,
            ))),
            0x66 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::H,
                InstructionTarget::HLI,
            ))),
            0x67 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::H,
                InstructionTarget::A,
            ))),

            0x68 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::L,
                InstructionTarget::B,
            ))),
            0x69 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::L,
                InstructionTarget::C,
            ))),
            0x6a => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::L,
                InstructionTarget::D,
            ))),
            0x6b => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::L,
                InstructionTarget::E,
            ))),
            0x6c => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::L,
                InstructionTarget::H,
            ))),
            0x6d => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::L,
                InstructionTarget::L,
            ))),
            0x6e => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::L,
                InstructionTarget::HLI,
            ))),
            0x6f => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::L,
                InstructionTarget::A,
            ))),

            0x70 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::HLI,
                InstructionTarget::B,
            ))),
            0x71 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::HLI,
                InstructionTarget::C,
            ))),
            0x72 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::HLI,
                InstructionTarget::D,
            ))),
            0x73 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::HLI,
                InstructionTarget::E,
            ))),
            0x74 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::HLI,
                InstructionTarget::H,
            ))),
            0x75 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::HLI,
                InstructionTarget::L,
            ))),
            0x77 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::HLI,
                InstructionTarget::A,
            ))),

            0x78 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::A,
                InstructionTarget::B,
            ))),
            0x79 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::A,
                InstructionTarget::C,
            ))),
            0x7a => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::A,
                InstructionTarget::D,
            ))),
            0x7b => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::A,
                InstructionTarget::E,
            ))),
            0x7c => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::A,
                InstructionTarget::H,
            ))),
            0x7d => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::A,
                InstructionTarget::L,
            ))),
            0x7e => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::A,
                InstructionTarget::HLI,
            ))),
            0x7f => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::A,
                InstructionTarget::A,
            ))),

            0x3e => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::A,
                InstructionTarget::D8,
            ))),
            0x06 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::B,
                InstructionTarget::D8,
            ))),
            0x0e => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::C,
                InstructionTarget::D8,
            ))),
            0x16 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::D,
                InstructionTarget::D8,
            ))),
            0x1e => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::E,
                InstructionTarget::D8,
            ))),
            0x26 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::H,
                InstructionTarget::D8,
            ))),
            0x2e => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::L,
                InstructionTarget::D8,
            ))),
            0x36 => Some(Instruction::LD(LoadType::Byte(
                InstructionTarget::HLI,
                InstructionTarget::D8,
            ))),

            0xe0 => Some(Instruction::LD(LoadType::ByteAddressFromA)),
            0xf0 => Some(Instruction::LD(LoadType::AFromByteAddress)),

            0x08 => Some(Instruction::LD(LoadType::IndirectFromSP)),
            0xf9 => Some(Instruction::LD(LoadType::SPFromHL)),
            0xf8 => Some(Instruction::LD(LoadType::HLFromSPN)),

            0xc5 => Some(Instruction::PUSH(InstructionTarget::BC)),
            0xd5 => Some(Instruction::PUSH(InstructionTarget::DE)),
            0xe5 => Some(Instruction::PUSH(InstructionTarget::HL)),
            0xf5 => Some(Instruction::PUSH(InstructionTarget::AF)),

            0xc1 => Some(Instruction::POP(InstructionTarget::BC)),
            0xd1 => Some(Instruction::POP(InstructionTarget::DE)),
            0xe1 => Some(Instruction::POP(InstructionTarget::HL)),
            0xf1 => Some(Instruction::POP(InstructionTarget::AF)),

            0xc4 => Some(Instruction::CALL(JumpTest::NotZero)),
            0xd4 => Some(Instruction::CALL(JumpTest::NotCarry)),
            0xcc => Some(Instruction::CALL(JumpTest::Zero)),
            0xdc => Some(Instruction::CALL(JumpTest::Carry)),
            0xcd => Some(Instruction::CALL(JumpTest::Always)),

            0xc0 => Some(Instruction::RET(JumpTest::NotZero)),
            0xd0 => Some(Instruction::RET(JumpTest::NotCarry)),
            0xc8 => Some(Instruction::RET(JumpTest::Zero)),
            0xd8 => Some(Instruction::RET(JumpTest::Carry)),
            0xc9 => Some(Instruction::RET(JumpTest::Always)),
            0xd9 => Some(Instruction::RETI),

            0xc7 => Some(Instruction::RST(RSTLocation::X00)),
            0xd7 => Some(Instruction::RST(RSTLocation::X10)),
            0xe7 => Some(Instruction::RST(RSTLocation::X20)),
            0xf7 => Some(Instruction::RST(RSTLocation::X30)),
            0xcf => Some(Instruction::RST(RSTLocation::X08)),
            0xdf => Some(Instruction::RST(RSTLocation::X18)),
            0xef => Some(Instruction::RST(RSTLocation::X28)),
            0xff => Some(Instruction::RST(RSTLocation::X38)),

            0x00 => Some(Instruction::NOP),
            0x76 => Some(Instruction::HALT),
            0xf3 => Some(Instruction::DI),
            0xfb => Some(Instruction::EI),

            _ => None,
        }
    }
}
