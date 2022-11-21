// Borrowed from: https://github.com/rylev/DMG-01

use std;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IncDecTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLI,
    BC,
    DE,
    HL,
    SP,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ArithmeticTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    D8,
    HLI,
}
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ADDHLTarget {
    BC,
    DE,
    HL,
    SP,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PrefixTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLI,
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
pub enum LoadByteTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLI,
}
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadByteSource {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    D8,
    HLI,
}
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadWordTarget {
    BC,
    DE,
    HL,
    SP,
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
    Byte(LoadByteTarget, LoadByteSource),
    Word(LoadWordTarget),
    AFromIndirect(Indirect),
    IndirectFromA(Indirect),
    AFromByteAddress,
    ByteAddressFromA,
    SPFromHL,
    HLFromSPN,
    IndirectFromSP,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum StackTarget {
    AF,
    BC,
    DE,
    HL,
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
    INC(IncDecTarget),
    DEC(IncDecTarget),

    ADD(ArithmeticTarget),
    ADC(ArithmeticTarget),
    ADDHL(ADDHLTarget),
    ADDSP,
    SUB(ArithmeticTarget),
    SBC(ArithmeticTarget),
    AND(ArithmeticTarget),
    OR(ArithmeticTarget),
    XOR(ArithmeticTarget),
    CP(ArithmeticTarget),

    CCF,
    SCF,

    RRA,
    RLA,
    RRCA,
    RLCA,
    CPL,
    DAA,

    // Prefix Instructions
    BIT(PrefixTarget, BitPosition),
    RES(PrefixTarget, BitPosition),
    SET(PrefixTarget, BitPosition),
    SRL(PrefixTarget),
    RR(PrefixTarget),
    RL(PrefixTarget),
    RRC(PrefixTarget),
    RLC(PrefixTarget),
    SRA(PrefixTarget),
    SLA(PrefixTarget),
    SWAP(PrefixTarget),

    // Jump Instructions
    JP(JumpTest),
    JR(JumpTest),
    JPI,

    // Load Instructions
    LD(LoadType),

    // Stack Instructions
    PUSH(StackTarget),
    POP(StackTarget),
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
            0x00 => Some(Instruction::RLC(PrefixTarget::B)),
            0x01 => Some(Instruction::RLC(PrefixTarget::C)),
            0x02 => Some(Instruction::RLC(PrefixTarget::D)),
            0x03 => Some(Instruction::RLC(PrefixTarget::E)),
            0x04 => Some(Instruction::RLC(PrefixTarget::H)),
            0x05 => Some(Instruction::RLC(PrefixTarget::L)),
            0x06 => Some(Instruction::RLC(PrefixTarget::HLI)),
            0x07 => Some(Instruction::RLC(PrefixTarget::A)),

            0x08 => Some(Instruction::RRC(PrefixTarget::B)),
            0x09 => Some(Instruction::RRC(PrefixTarget::C)),
            0x0a => Some(Instruction::RRC(PrefixTarget::D)),
            0x0b => Some(Instruction::RRC(PrefixTarget::E)),
            0x0c => Some(Instruction::RRC(PrefixTarget::H)),
            0x0d => Some(Instruction::RRC(PrefixTarget::L)),
            0x0e => Some(Instruction::RRC(PrefixTarget::HLI)),
            0x0f => Some(Instruction::RRC(PrefixTarget::A)),

            0x10 => Some(Instruction::RL(PrefixTarget::B)),
            0x11 => Some(Instruction::RL(PrefixTarget::C)),
            0x12 => Some(Instruction::RL(PrefixTarget::D)),
            0x13 => Some(Instruction::RL(PrefixTarget::E)),
            0x14 => Some(Instruction::RL(PrefixTarget::H)),
            0x15 => Some(Instruction::RL(PrefixTarget::L)),
            0x16 => Some(Instruction::RL(PrefixTarget::HLI)),
            0x17 => Some(Instruction::RL(PrefixTarget::A)),

            0x18 => Some(Instruction::RR(PrefixTarget::B)),
            0x19 => Some(Instruction::RR(PrefixTarget::C)),
            0x1a => Some(Instruction::RR(PrefixTarget::D)),
            0x1b => Some(Instruction::RR(PrefixTarget::E)),
            0x1c => Some(Instruction::RR(PrefixTarget::H)),
            0x1d => Some(Instruction::RR(PrefixTarget::L)),
            0x1e => Some(Instruction::RR(PrefixTarget::HLI)),
            0x1f => Some(Instruction::RR(PrefixTarget::A)),

            0x20 => Some(Instruction::SLA(PrefixTarget::B)),
            0x21 => Some(Instruction::SLA(PrefixTarget::C)),
            0x22 => Some(Instruction::SLA(PrefixTarget::D)),
            0x23 => Some(Instruction::SLA(PrefixTarget::E)),
            0x24 => Some(Instruction::SLA(PrefixTarget::H)),
            0x25 => Some(Instruction::SLA(PrefixTarget::L)),
            0x26 => Some(Instruction::SLA(PrefixTarget::HLI)),
            0x27 => Some(Instruction::SLA(PrefixTarget::A)),

            0x28 => Some(Instruction::SRA(PrefixTarget::B)),
            0x29 => Some(Instruction::SRA(PrefixTarget::C)),
            0x2a => Some(Instruction::SRA(PrefixTarget::D)),
            0x2b => Some(Instruction::SRA(PrefixTarget::E)),
            0x2c => Some(Instruction::SRA(PrefixTarget::H)),
            0x2d => Some(Instruction::SRA(PrefixTarget::L)),
            0x2e => Some(Instruction::SRA(PrefixTarget::HLI)),
            0x2f => Some(Instruction::SRA(PrefixTarget::A)),

            0x30 => Some(Instruction::SWAP(PrefixTarget::B)),
            0x31 => Some(Instruction::SWAP(PrefixTarget::C)),
            0x32 => Some(Instruction::SWAP(PrefixTarget::D)),
            0x33 => Some(Instruction::SWAP(PrefixTarget::E)),
            0x34 => Some(Instruction::SWAP(PrefixTarget::H)),
            0x35 => Some(Instruction::SWAP(PrefixTarget::L)),
            0x36 => Some(Instruction::SWAP(PrefixTarget::HLI)),
            0x37 => Some(Instruction::SWAP(PrefixTarget::A)),

            0x38 => Some(Instruction::SRL(PrefixTarget::B)),
            0x39 => Some(Instruction::SRL(PrefixTarget::C)),
            0x3a => Some(Instruction::SRL(PrefixTarget::D)),
            0x3b => Some(Instruction::SRL(PrefixTarget::E)),
            0x3c => Some(Instruction::SRL(PrefixTarget::H)),
            0x3d => Some(Instruction::SRL(PrefixTarget::L)),
            0x3e => Some(Instruction::SRL(PrefixTarget::HLI)),
            0x3f => Some(Instruction::SRL(PrefixTarget::A)),

            0x40 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B0)),
            0x41 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B0)),
            0x42 => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B0)),
            0x43 => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B0)),
            0x44 => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B0)),
            0x45 => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B0)),
            0x46 => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B0)),
            0x47 => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B0)),

            0x48 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B1)),
            0x49 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B1)),
            0x4a => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B1)),
            0x4b => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B1)),
            0x4c => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B1)),
            0x4d => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B1)),
            0x4e => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B1)),
            0x4f => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B1)),

            0x50 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B2)),
            0x51 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B2)),
            0x52 => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B2)),
            0x53 => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B2)),
            0x54 => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B2)),
            0x55 => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B2)),
            0x56 => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B2)),
            0x57 => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B2)),

            0x58 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B3)),
            0x59 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B3)),
            0x5a => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B3)),
            0x5b => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B3)),
            0x5c => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B3)),
            0x5d => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B3)),
            0x5e => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B3)),
            0x5f => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B3)),

            0x60 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B4)),
            0x61 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B4)),
            0x62 => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B4)),
            0x63 => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B4)),
            0x64 => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B4)),
            0x65 => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B4)),
            0x66 => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B4)),
            0x67 => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B4)),

            0x68 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B5)),
            0x69 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B5)),
            0x6a => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B5)),
            0x6b => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B5)),
            0x6c => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B5)),
            0x6d => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B5)),
            0x6e => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B5)),
            0x6f => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B5)),

            0x70 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B6)),
            0x71 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B6)),
            0x72 => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B6)),
            0x73 => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B6)),
            0x74 => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B6)),
            0x75 => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B6)),
            0x76 => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B6)),
            0x77 => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B6)),

            0x78 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B7)),
            0x79 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B7)),
            0x7a => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B7)),
            0x7b => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B7)),
            0x7c => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B7)),
            0x7d => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B7)),
            0x7e => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B7)),
            0x7f => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B7)),

            0x80 => Some(Instruction::RES(PrefixTarget::B, BitPosition::B0)),
            0x81 => Some(Instruction::RES(PrefixTarget::C, BitPosition::B0)),
            0x82 => Some(Instruction::RES(PrefixTarget::D, BitPosition::B0)),
            0x83 => Some(Instruction::RES(PrefixTarget::E, BitPosition::B0)),
            0x84 => Some(Instruction::RES(PrefixTarget::H, BitPosition::B0)),
            0x85 => Some(Instruction::RES(PrefixTarget::L, BitPosition::B0)),
            0x86 => Some(Instruction::RES(PrefixTarget::HLI, BitPosition::B0)),
            0x87 => Some(Instruction::RES(PrefixTarget::A, BitPosition::B0)),

            0x88 => Some(Instruction::RES(PrefixTarget::B, BitPosition::B1)),
            0x89 => Some(Instruction::RES(PrefixTarget::C, BitPosition::B1)),
            0x8a => Some(Instruction::RES(PrefixTarget::D, BitPosition::B1)),
            0x8b => Some(Instruction::RES(PrefixTarget::E, BitPosition::B1)),
            0x8c => Some(Instruction::RES(PrefixTarget::H, BitPosition::B1)),
            0x8d => Some(Instruction::RES(PrefixTarget::L, BitPosition::B1)),
            0x8e => Some(Instruction::RES(PrefixTarget::HLI, BitPosition::B1)),
            0x8f => Some(Instruction::RES(PrefixTarget::A, BitPosition::B1)),

            0x90 => Some(Instruction::RES(PrefixTarget::B, BitPosition::B2)),
            0x91 => Some(Instruction::RES(PrefixTarget::C, BitPosition::B2)),
            0x92 => Some(Instruction::RES(PrefixTarget::D, BitPosition::B2)),
            0x93 => Some(Instruction::RES(PrefixTarget::E, BitPosition::B2)),
            0x94 => Some(Instruction::RES(PrefixTarget::H, BitPosition::B2)),
            0x95 => Some(Instruction::RES(PrefixTarget::L, BitPosition::B2)),
            0x96 => Some(Instruction::RES(PrefixTarget::HLI, BitPosition::B2)),
            0x97 => Some(Instruction::RES(PrefixTarget::A, BitPosition::B2)),

            0x98 => Some(Instruction::RES(PrefixTarget::B, BitPosition::B3)),
            0x99 => Some(Instruction::RES(PrefixTarget::C, BitPosition::B3)),
            0x9a => Some(Instruction::RES(PrefixTarget::D, BitPosition::B3)),
            0x9b => Some(Instruction::RES(PrefixTarget::E, BitPosition::B3)),
            0x9c => Some(Instruction::RES(PrefixTarget::H, BitPosition::B3)),
            0x9d => Some(Instruction::RES(PrefixTarget::L, BitPosition::B3)),
            0x9e => Some(Instruction::RES(PrefixTarget::HLI, BitPosition::B3)),
            0x9f => Some(Instruction::RES(PrefixTarget::A, BitPosition::B3)),

            0xa0 => Some(Instruction::RES(PrefixTarget::B, BitPosition::B4)),
            0xa1 => Some(Instruction::RES(PrefixTarget::C, BitPosition::B4)),
            0xa2 => Some(Instruction::RES(PrefixTarget::D, BitPosition::B4)),
            0xa3 => Some(Instruction::RES(PrefixTarget::E, BitPosition::B4)),
            0xa4 => Some(Instruction::RES(PrefixTarget::H, BitPosition::B4)),
            0xa5 => Some(Instruction::RES(PrefixTarget::L, BitPosition::B4)),
            0xa6 => Some(Instruction::RES(PrefixTarget::HLI, BitPosition::B4)),
            0xa7 => Some(Instruction::RES(PrefixTarget::A, BitPosition::B4)),

            0xa8 => Some(Instruction::RES(PrefixTarget::B, BitPosition::B5)),
            0xa9 => Some(Instruction::RES(PrefixTarget::C, BitPosition::B5)),
            0xaa => Some(Instruction::RES(PrefixTarget::D, BitPosition::B5)),
            0xab => Some(Instruction::RES(PrefixTarget::E, BitPosition::B5)),
            0xac => Some(Instruction::RES(PrefixTarget::H, BitPosition::B5)),
            0xad => Some(Instruction::RES(PrefixTarget::L, BitPosition::B5)),
            0xae => Some(Instruction::RES(PrefixTarget::HLI, BitPosition::B5)),
            0xaf => Some(Instruction::RES(PrefixTarget::A, BitPosition::B5)),

            0xb0 => Some(Instruction::RES(PrefixTarget::B, BitPosition::B6)),
            0xb1 => Some(Instruction::RES(PrefixTarget::C, BitPosition::B6)),
            0xb2 => Some(Instruction::RES(PrefixTarget::D, BitPosition::B6)),
            0xb3 => Some(Instruction::RES(PrefixTarget::E, BitPosition::B6)),
            0xb4 => Some(Instruction::RES(PrefixTarget::H, BitPosition::B6)),
            0xb5 => Some(Instruction::RES(PrefixTarget::L, BitPosition::B6)),
            0xb6 => Some(Instruction::RES(PrefixTarget::HLI, BitPosition::B6)),
            0xb7 => Some(Instruction::RES(PrefixTarget::A, BitPosition::B6)),

            0xb8 => Some(Instruction::RES(PrefixTarget::B, BitPosition::B7)),
            0xb9 => Some(Instruction::RES(PrefixTarget::C, BitPosition::B7)),
            0xba => Some(Instruction::RES(PrefixTarget::D, BitPosition::B7)),
            0xbb => Some(Instruction::RES(PrefixTarget::E, BitPosition::B7)),
            0xbc => Some(Instruction::RES(PrefixTarget::H, BitPosition::B7)),
            0xbd => Some(Instruction::RES(PrefixTarget::L, BitPosition::B7)),
            0xbe => Some(Instruction::RES(PrefixTarget::HLI, BitPosition::B7)),
            0xbf => Some(Instruction::RES(PrefixTarget::A, BitPosition::B7)),

            0xc0 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B0)),
            0xc1 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B0)),
            0xc2 => Some(Instruction::SET(PrefixTarget::D, BitPosition::B0)),
            0xc3 => Some(Instruction::SET(PrefixTarget::E, BitPosition::B0)),
            0xc4 => Some(Instruction::SET(PrefixTarget::H, BitPosition::B0)),
            0xc5 => Some(Instruction::SET(PrefixTarget::L, BitPosition::B0)),
            0xc6 => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B0)),
            0xc7 => Some(Instruction::SET(PrefixTarget::A, BitPosition::B0)),

            0xc8 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B1)),
            0xc9 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B1)),
            0xca => Some(Instruction::SET(PrefixTarget::D, BitPosition::B1)),
            0xcb => Some(Instruction::SET(PrefixTarget::E, BitPosition::B1)),
            0xcc => Some(Instruction::SET(PrefixTarget::H, BitPosition::B1)),
            0xcd => Some(Instruction::SET(PrefixTarget::L, BitPosition::B1)),
            0xce => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B1)),
            0xcf => Some(Instruction::SET(PrefixTarget::A, BitPosition::B1)),

            0xd0 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B2)),
            0xd1 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B2)),
            0xd2 => Some(Instruction::SET(PrefixTarget::D, BitPosition::B2)),
            0xd3 => Some(Instruction::SET(PrefixTarget::E, BitPosition::B2)),
            0xd4 => Some(Instruction::SET(PrefixTarget::H, BitPosition::B2)),
            0xd5 => Some(Instruction::SET(PrefixTarget::L, BitPosition::B2)),
            0xd6 => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B2)),
            0xd7 => Some(Instruction::SET(PrefixTarget::A, BitPosition::B2)),

            0xd8 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B3)),
            0xd9 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B3)),
            0xda => Some(Instruction::SET(PrefixTarget::D, BitPosition::B3)),
            0xdb => Some(Instruction::SET(PrefixTarget::E, BitPosition::B3)),
            0xdc => Some(Instruction::SET(PrefixTarget::H, BitPosition::B3)),
            0xdd => Some(Instruction::SET(PrefixTarget::L, BitPosition::B3)),
            0xde => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B3)),
            0xdf => Some(Instruction::SET(PrefixTarget::A, BitPosition::B3)),

            0xe0 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B4)),
            0xe1 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B4)),
            0xe2 => Some(Instruction::SET(PrefixTarget::D, BitPosition::B4)),
            0xe3 => Some(Instruction::SET(PrefixTarget::E, BitPosition::B4)),
            0xe4 => Some(Instruction::SET(PrefixTarget::H, BitPosition::B4)),
            0xe5 => Some(Instruction::SET(PrefixTarget::L, BitPosition::B4)),
            0xe6 => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B4)),
            0xe7 => Some(Instruction::SET(PrefixTarget::A, BitPosition::B4)),

            0xe8 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B5)),
            0xe9 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B5)),
            0xea => Some(Instruction::SET(PrefixTarget::D, BitPosition::B5)),
            0xeb => Some(Instruction::SET(PrefixTarget::E, BitPosition::B5)),
            0xec => Some(Instruction::SET(PrefixTarget::H, BitPosition::B5)),
            0xed => Some(Instruction::SET(PrefixTarget::L, BitPosition::B5)),
            0xee => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B5)),
            0xef => Some(Instruction::SET(PrefixTarget::A, BitPosition::B5)),

            0xf0 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B6)),
            0xf1 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B6)),
            0xf2 => Some(Instruction::SET(PrefixTarget::D, BitPosition::B6)),
            0xf3 => Some(Instruction::SET(PrefixTarget::E, BitPosition::B6)),
            0xf4 => Some(Instruction::SET(PrefixTarget::H, BitPosition::B6)),
            0xf5 => Some(Instruction::SET(PrefixTarget::L, BitPosition::B6)),
            0xf6 => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B6)),
            0xf7 => Some(Instruction::SET(PrefixTarget::A, BitPosition::B6)),

            0xf8 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B7)),
            0xf9 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B7)),
            0xfa => Some(Instruction::SET(PrefixTarget::D, BitPosition::B7)),
            0xfb => Some(Instruction::SET(PrefixTarget::E, BitPosition::B7)),
            0xfc => Some(Instruction::SET(PrefixTarget::H, BitPosition::B7)),
            0xfd => Some(Instruction::SET(PrefixTarget::L, BitPosition::B7)),
            0xfe => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B7)),
            0xff => Some(Instruction::SET(PrefixTarget::A, BitPosition::B7)),
        }
    }

    fn from_byte_not_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x3c => Some(Instruction::INC(IncDecTarget::A)),
            0x04 => Some(Instruction::INC(IncDecTarget::B)),
            0x14 => Some(Instruction::INC(IncDecTarget::D)),
            0x24 => Some(Instruction::INC(IncDecTarget::H)),
            0x0c => Some(Instruction::INC(IncDecTarget::C)),
            0x1c => Some(Instruction::INC(IncDecTarget::E)),
            0x2c => Some(Instruction::INC(IncDecTarget::L)),
            0x34 => Some(Instruction::INC(IncDecTarget::HLI)),
            0x03 => Some(Instruction::INC(IncDecTarget::BC)),
            0x13 => Some(Instruction::INC(IncDecTarget::DE)),
            0x23 => Some(Instruction::INC(IncDecTarget::HL)),
            0x33 => Some(Instruction::INC(IncDecTarget::SP)),

            0x3d => Some(Instruction::DEC(IncDecTarget::A)),
            0x05 => Some(Instruction::DEC(IncDecTarget::B)),
            0x0d => Some(Instruction::DEC(IncDecTarget::C)),
            0x15 => Some(Instruction::DEC(IncDecTarget::D)),
            0x1d => Some(Instruction::DEC(IncDecTarget::E)),
            0x25 => Some(Instruction::DEC(IncDecTarget::H)),
            0x2d => Some(Instruction::DEC(IncDecTarget::L)),
            0x35 => Some(Instruction::DEC(IncDecTarget::HLI)),
            0x0b => Some(Instruction::DEC(IncDecTarget::BC)),
            0x1b => Some(Instruction::DEC(IncDecTarget::DE)),
            0x2b => Some(Instruction::DEC(IncDecTarget::HL)),
            0x3b => Some(Instruction::DEC(IncDecTarget::SP)),

            0x87 => Some(Instruction::ADD(ArithmeticTarget::A)),
            0x80 => Some(Instruction::ADD(ArithmeticTarget::B)),
            0x81 => Some(Instruction::ADD(ArithmeticTarget::C)),
            0x82 => Some(Instruction::ADD(ArithmeticTarget::D)),
            0x83 => Some(Instruction::ADD(ArithmeticTarget::E)),
            0x84 => Some(Instruction::ADD(ArithmeticTarget::H)),
            0x85 => Some(Instruction::ADD(ArithmeticTarget::L)),
            0x86 => Some(Instruction::ADD(ArithmeticTarget::HLI)),
            0xc6 => Some(Instruction::ADD(ArithmeticTarget::D8)),

            0x09 => Some(Instruction::ADDHL(ADDHLTarget::BC)),
            0x19 => Some(Instruction::ADDHL(ADDHLTarget::DE)),
            0x29 => Some(Instruction::ADDHL(ADDHLTarget::HL)),
            0x39 => Some(Instruction::ADDHL(ADDHLTarget::SP)),

            0x8f => Some(Instruction::ADC(ArithmeticTarget::A)),
            0x88 => Some(Instruction::ADC(ArithmeticTarget::B)),
            0x89 => Some(Instruction::ADC(ArithmeticTarget::C)),
            0x8a => Some(Instruction::ADC(ArithmeticTarget::D)),
            0x8b => Some(Instruction::ADC(ArithmeticTarget::E)),
            0x8c => Some(Instruction::ADC(ArithmeticTarget::H)),
            0x8d => Some(Instruction::ADC(ArithmeticTarget::L)),
            0x8e => Some(Instruction::ADC(ArithmeticTarget::HLI)),
            0xce => Some(Instruction::ADC(ArithmeticTarget::D8)),

            0x97 => Some(Instruction::SUB(ArithmeticTarget::A)),
            0x90 => Some(Instruction::SUB(ArithmeticTarget::B)),
            0x91 => Some(Instruction::SUB(ArithmeticTarget::C)),
            0x92 => Some(Instruction::SUB(ArithmeticTarget::D)),
            0x93 => Some(Instruction::SUB(ArithmeticTarget::E)),
            0x94 => Some(Instruction::SUB(ArithmeticTarget::H)),
            0x95 => Some(Instruction::SUB(ArithmeticTarget::L)),
            0x96 => Some(Instruction::SUB(ArithmeticTarget::HLI)),
            0xd6 => Some(Instruction::SUB(ArithmeticTarget::D8)),

            0x9f => Some(Instruction::SBC(ArithmeticTarget::A)),
            0x98 => Some(Instruction::SBC(ArithmeticTarget::B)),
            0x99 => Some(Instruction::SBC(ArithmeticTarget::C)),
            0x9a => Some(Instruction::SBC(ArithmeticTarget::D)),
            0x9b => Some(Instruction::SBC(ArithmeticTarget::E)),
            0x9c => Some(Instruction::SBC(ArithmeticTarget::H)),
            0x9d => Some(Instruction::SBC(ArithmeticTarget::L)),
            0x9e => Some(Instruction::SBC(ArithmeticTarget::HLI)),
            0xde => Some(Instruction::SBC(ArithmeticTarget::D8)),

            0xa7 => Some(Instruction::AND(ArithmeticTarget::A)),
            0xa0 => Some(Instruction::AND(ArithmeticTarget::B)),
            0xa1 => Some(Instruction::AND(ArithmeticTarget::C)),
            0xa2 => Some(Instruction::AND(ArithmeticTarget::D)),
            0xa3 => Some(Instruction::AND(ArithmeticTarget::E)),
            0xa4 => Some(Instruction::AND(ArithmeticTarget::H)),
            0xa5 => Some(Instruction::AND(ArithmeticTarget::L)),
            0xa6 => Some(Instruction::AND(ArithmeticTarget::HLI)),
            0xe6 => Some(Instruction::AND(ArithmeticTarget::D8)),

            0xb7 => Some(Instruction::OR(ArithmeticTarget::A)),
            0xb0 => Some(Instruction::OR(ArithmeticTarget::B)),
            0xb1 => Some(Instruction::OR(ArithmeticTarget::C)),
            0xb2 => Some(Instruction::OR(ArithmeticTarget::D)),
            0xb3 => Some(Instruction::OR(ArithmeticTarget::E)),
            0xb4 => Some(Instruction::OR(ArithmeticTarget::H)),
            0xb5 => Some(Instruction::OR(ArithmeticTarget::L)),
            0xb6 => Some(Instruction::OR(ArithmeticTarget::HLI)),
            0xf6 => Some(Instruction::OR(ArithmeticTarget::D8)),

            0xaf => Some(Instruction::XOR(ArithmeticTarget::A)),
            0xa8 => Some(Instruction::XOR(ArithmeticTarget::B)),
            0xa9 => Some(Instruction::XOR(ArithmeticTarget::C)),
            0xaa => Some(Instruction::XOR(ArithmeticTarget::D)),
            0xab => Some(Instruction::XOR(ArithmeticTarget::E)),
            0xac => Some(Instruction::XOR(ArithmeticTarget::H)),
            0xad => Some(Instruction::XOR(ArithmeticTarget::L)),
            0xae => Some(Instruction::XOR(ArithmeticTarget::HLI)),
            0xee => Some(Instruction::XOR(ArithmeticTarget::D8)),

            0xbf => Some(Instruction::CP(ArithmeticTarget::A)),
            0xb8 => Some(Instruction::CP(ArithmeticTarget::B)),
            0xb9 => Some(Instruction::CP(ArithmeticTarget::C)),
            0xba => Some(Instruction::CP(ArithmeticTarget::D)),
            0xbb => Some(Instruction::CP(ArithmeticTarget::E)),
            0xbc => Some(Instruction::CP(ArithmeticTarget::H)),
            0xbd => Some(Instruction::CP(ArithmeticTarget::L)),
            0xbe => Some(Instruction::CP(ArithmeticTarget::HLI)),
            0xfe => Some(Instruction::CP(ArithmeticTarget::D8)),

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

            0x01 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::BC))),
            0x11 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::DE))),
            0x21 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::HL))),
            0x31 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::SP))),

            0x40 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::B,
            ))),
            0x41 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::C,
            ))),
            0x42 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::D,
            ))),
            0x43 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::E,
            ))),
            0x44 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::H,
            ))),
            0x45 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::L,
            ))),
            0x46 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::HLI,
            ))),
            0x47 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::A,
            ))),

            0x48 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::B,
            ))),
            0x49 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::C,
            ))),
            0x4a => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::D,
            ))),
            0x4b => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::E,
            ))),
            0x4c => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::H,
            ))),
            0x4d => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::L,
            ))),
            0x4e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::HLI,
            ))),
            0x4f => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::A,
            ))),

            0x50 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::B,
            ))),
            0x51 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::C,
            ))),
            0x52 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::D,
            ))),
            0x53 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::E,
            ))),
            0x54 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::H,
            ))),
            0x55 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::L,
            ))),
            0x56 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::HLI,
            ))),
            0x57 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::A,
            ))),

            0x58 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::B,
            ))),
            0x59 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::C,
            ))),
            0x5a => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::D,
            ))),
            0x5b => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::E,
            ))),
            0x5c => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::H,
            ))),
            0x5d => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::L,
            ))),
            0x5e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::HLI,
            ))),
            0x5f => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::A,
            ))),

            0x60 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::B,
            ))),
            0x61 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::C,
            ))),
            0x62 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::D,
            ))),
            0x63 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::E,
            ))),
            0x64 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::H,
            ))),
            0x65 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::L,
            ))),
            0x66 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::HLI,
            ))),
            0x67 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::A,
            ))),

            0x68 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::B,
            ))),
            0x69 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::C,
            ))),
            0x6a => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::D,
            ))),
            0x6b => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::E,
            ))),
            0x6c => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::H,
            ))),
            0x6d => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::L,
            ))),
            0x6e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::HLI,
            ))),
            0x6f => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::A,
            ))),

            0x70 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::B,
            ))),
            0x71 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::C,
            ))),
            0x72 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::D,
            ))),
            0x73 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::E,
            ))),
            0x74 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::H,
            ))),
            0x75 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::L,
            ))),
            0x77 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::A,
            ))),

            0x78 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::B,
            ))),
            0x79 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::C,
            ))),
            0x7a => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::D,
            ))),
            0x7b => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::E,
            ))),
            0x7c => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::H,
            ))),
            0x7d => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::L,
            ))),
            0x7e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::HLI,
            ))),
            0x7f => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::A,
            ))),

            0x3e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::A,
                LoadByteSource::D8,
            ))),
            0x06 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::B,
                LoadByteSource::D8,
            ))),
            0x0e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::C,
                LoadByteSource::D8,
            ))),
            0x16 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::D,
                LoadByteSource::D8,
            ))),
            0x1e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::E,
                LoadByteSource::D8,
            ))),
            0x26 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::H,
                LoadByteSource::D8,
            ))),
            0x2e => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::L,
                LoadByteSource::D8,
            ))),
            0x36 => Some(Instruction::LD(LoadType::Byte(
                LoadByteTarget::HLI,
                LoadByteSource::D8,
            ))),

            0xe0 => Some(Instruction::LD(LoadType::ByteAddressFromA)),
            0xf0 => Some(Instruction::LD(LoadType::AFromByteAddress)),

            0x08 => Some(Instruction::LD(LoadType::IndirectFromSP)),
            0xf9 => Some(Instruction::LD(LoadType::SPFromHL)),
            0xf8 => Some(Instruction::LD(LoadType::HLFromSPN)),

            0xc5 => Some(Instruction::PUSH(StackTarget::BC)),
            0xd5 => Some(Instruction::PUSH(StackTarget::DE)),
            0xe5 => Some(Instruction::PUSH(StackTarget::HL)),
            0xf5 => Some(Instruction::PUSH(StackTarget::AF)),

            0xc1 => Some(Instruction::POP(StackTarget::BC)),
            0xd1 => Some(Instruction::POP(StackTarget::DE)),
            0xe1 => Some(Instruction::POP(StackTarget::HL)),
            0xf1 => Some(Instruction::POP(StackTarget::AF)),

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