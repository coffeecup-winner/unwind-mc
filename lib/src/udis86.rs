use std::ffi::CStr;
use std::mem;

use libudis86_sys::*;

pub struct Disassembler {
    ud: ud,
}

#[derive(Debug, Clone)]
pub struct Insn {
    pub address: u64,
    pub code: ud_mnemonic_code,
    pub length: u8,
    pub hex: String,
    pub assembly: String,
    pub operands: Vec<Operand>,
    pub prefix_rex: u8,
    pub prefix_segment: ud_type,
    pub prefix_operand_size: bool,
    pub prefix_address_size: bool,
    pub prefix_lock: u8,
    pub prefix_str: u8,
    pub prefix_rep: u8,
    pub prefix_repe: u8,
    pub prefix_repne: u8,
}

impl ToString for Insn {
    fn to_string(&self) -> String {
        self.assembly.clone()
    }
}

impl Insn {
    pub fn get_target_address(&self) -> u64 {
        let target_address = self.address + u64::from(self.length);
        (target_address as i64 + self.operands[0].get_i64()) as u64
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Operand {
    pub type_: ud_type,
    pub size: u16,
    pub base: ud_type,
    pub index: ud_type,
    pub scale: u8,
    pub offset: u8,
    pub lvalue: ud_lval,
    pub oprcode: u8,
}

impl Operand {
    pub fn get_i64(&self) -> i64 {
        unsafe {
            match self.size {
                8 => i64::from(self.lvalue.sbyte),
                16 => i64::from(self.lvalue.sword),
                32 => i64::from(self.lvalue.sdword),
                64 => self.lvalue.sqword as i64,
                _ => panic!("Impossible"),
            }
        }
    }

    pub fn get_memory_offset(&self) -> i64 {
        unsafe {
            match self.offset {
                0 => 0,
                8 => i64::from(self.lvalue.sbyte),
                16 => i64::from(self.lvalue.sword),
                32 => i64::from(self.lvalue.sdword),
                64 => self.lvalue.sqword as i64,
                _ => panic!("Impossible"),
            }
        }
    }
}

impl Disassembler {
    pub fn new(pc: u64) -> Self {
        unsafe {
            let mut ud: ud = mem::uninitialized();
            ud_init(&mut ud);
            ud_set_mode(&mut ud, 32);
            // Syntax generation in udis86 is bugged (incorrect vsnprintf calls)
            // Intel syntax translation is done manually below
            ud_set_syntax(&mut ud, None);
            ud_set_pc(&mut ud, pc);
            Disassembler { ud }
        }
    }

    pub fn set_pc(&mut self, pc: u64) -> () {
        unsafe {
            ud_set_pc(&mut self.ud, pc);
        }
    }

    pub fn disassemble(&mut self, bytes: &[u8], offset: usize, length: usize) -> Vec<Insn> {
        let mut result = vec![];
        unsafe {
            ud_set_input_buffer(&mut self.ud, bytes.as_ptr().add(offset), length);
            while ud_disassemble(&mut self.ud) != 0 {
                let hex_cstr = CStr::from_ptr(ud_insn_hex(&mut self.ud));
                let operands = Self::get_operands(&mut self.ud);

                result.push(Insn {
                    address: ud_insn_off(&self.ud),
                    code: ud_insn_mnemonic(&self.ud),
                    length: ud_insn_len(&self.ud) as u8,
                    hex: hex_cstr.to_str().unwrap().to_owned(),
                    assembly: Self::print_intel(&self.ud, &operands),
                    operands: operands,
                    prefix_rex: self.ud.pfx_rex,
                    prefix_segment: mem::transmute(u32::from(self.ud.pfx_seg)),
                    prefix_operand_size: self.ud.pfx_opr == 0x66,
                    prefix_address_size: self.ud.pfx_adr == 0x67,
                    prefix_lock: self.ud.pfx_lock,
                    prefix_str: self.ud.pfx_str,
                    prefix_rep: self.ud.pfx_rep,
                    prefix_repe: self.ud.pfx_repe,
                    prefix_repne: self.ud.pfx_repne,
                });
            }
        }
        result
    }

    fn get_operands(ud: &mut ud) -> Vec<Operand> {
        let mut result = vec![];
        unsafe {
            let mut index = 0;
            let mut op = ud_insn_opr(ud, index);

            while !op.is_null() {
                result.push(Operand {
                    type_: (*op).otype,
                    size: (*op).size,
                    base: (*op).base,
                    index: (*op).index,
                    scale: (*op).scale,
                    offset: (*op).offset,
                    lvalue: (*op).lval,
                    oprcode: (*op)._oprcode,
                });
                index += 1;
                op = ud_insn_opr(ud, index);
            }
        }
        result
    }

    // Here and below code ported from udis86's syn-intel.c
    // The ported version is abridged, rare or non-x86 bits are removed
    fn print_intel(ud: &ud, operands: &Vec<Operand>) -> String {
        let mut result = String::new();
        
        if ud.pfx_seg != 0 &&
            operands.len() > 1 &&
            operands[0].type_ != ud_type::UD_OP_MEM &&
            operands[1].type_ != ud_type::UD_OP_MEM {
            result += &Self::print_reg(
                unsafe { std::mem::transmute(ud.pfx_seg as u32) }
            );
        }

        if ud.pfx_lock != 0 {
            result += "lock ";
        }
        
        if ud.pfx_rep != 0 {
            result += "rep ";
        } else if ud.pfx_repe != 0 {
            result += "repe ";
        } else if ud.pfx_repne != 0 {
            result += "repne ";
        }

        result += &Self::print_mnemonic(ud.mnemonic);

        if operands.len() > 0 && operands[0].type_ != ud_type::UD_NONE {
            let mut cast = false;
            result += " ";
            if operands[0].type_ == ud_type::UD_OP_MEM {
                if operands.len() <= 1 ||
                    operands[1].type_ == ud_type::UD_NONE ||
                    operands[1].type_ == ud_type::UD_OP_IMM ||
                    operands[1].type_ == ud_type::UD_OP_CONST ||                    
                    operands[0].size != operands[1].size {
                    cast = true;
                } else if operands.len() > 1 &&
                    operands[1].type_ == ud_type::UD_OP_REG &&
                    operands[1].base == ud_type::UD_R_CL {
                    match ud.mnemonic {
                        ud_mnemonic_code::UD_Ircl |
                        ud_mnemonic_code::UD_Irol |
                        ud_mnemonic_code::UD_Iror |
                        ud_mnemonic_code::UD_Ircr |
                        ud_mnemonic_code::UD_Ishl |
                        ud_mnemonic_code::UD_Ishr |
                        ud_mnemonic_code::UD_Isar => {
                            cast = true;
                        },
                        _ => {}
                    }
                }
            }
            result += &Self::print_operand(ud, &operands[0], cast);
        }

        if operands.len() > 1 && operands[1].type_ != ud_type::UD_NONE {
            let mut cast = false;
            result += ", ";
            if operands[1].type_ == ud_type::UD_OP_MEM &&
                operands[1].size != operands[0].size {
                cast = true;
            }
            result += &Self::print_operand(ud, &operands[1], cast);
        }

        if operands.len() > 2 && operands[2].type_ != ud_type::UD_NONE {
            let mut cast = false;
            result += ", ";
            if operands[2].type_ == ud_type::UD_OP_MEM &&
                operands[2].size != operands[1].size {
                cast = true;
            }
            result += &Self::print_operand(ud, &operands[2], cast);
        }

        if operands.len() > 3 && operands[3].type_ != ud_type::UD_NONE {
            result += ", ";
            result += &Self::print_operand(ud, &operands[3], false);
        }

        result
    }

    fn print_operand(ud: &ud, op: &Operand, cast: bool) -> String {
        let mut s = String::new();

        match op.type_ {
            ud_type::UD_OP_REG => {
                s += &Self::print_reg(op.base);
            },
            ud_type::UD_OP_MEM => {
                if cast {
                    s += &Self::print_cast(ud, op);
                }
                s += "[";
                if ud.pfx_seg != 0 {
                    s += &Self::print_reg(
                        unsafe { std::mem::transmute(ud.pfx_seg as u32) }
                    );
                    s += ":";
                }
                if op.base != ud_type::UD_NONE {
                    s += &Self::print_reg(op.base);
                }
                if op.index != ud_type::UD_NONE {
                    if op.base != ud_type::UD_NONE {
                        s += "+";
                    }
                    s += &Self::print_reg(op.index);
                    if op.scale != 0 {
                        s += &format!("*{}", op.scale);
                    }
                }
                if op.offset != 0 {
                    s += &Self::print_mem(op);
                }
                s += "]";
            },
            ud_type::UD_OP_IMM => {
                s += &Self::print_immediate(ud, op);
            },
            ud_type::UD_OP_JIMM => {
                s += &Self::print_relative_address(ud, op);
            },
            ud_type::UD_OP_PTR => {
                match op.size {
                    32 => {
                        s += unsafe {
                            &format!("word 0x{:x}0x{:x}", op.lvalue.ptr.seg, op.lvalue.ptr.off & 0xffff)
                        }
                    },
                    48 => {
                        s += unsafe {
                            &format!("dword 0x{:x}0x{:x}", op.lvalue.ptr.seg, op.lvalue.ptr.off)
                        }
                    },
                    _ => {}
                }
            },
            ud_type::UD_OP_CONST => {
                if cast {
                    s += &Self::print_cast(ud, op);
                }
                s += unsafe { &format!("{}", op.lvalue.udword) }
            },
            _ => {}
        }

        return s;
    }

    fn print_cast(ud: &ud, op: &Operand) -> String {
        let mut s = String::new();
        
        if ud.br_far != 0 {
            s += "far ";
        }
        match op.size {
            8 => { s += "byte "; },
            16 => { s += "word "; },
            32 => { s += "dword "; },
            64 => { s += "qword "; },
            80 => { s += "tword "; },
            128 => { s += "oword "; },
            256 => { s += "yword "; },
            _ => {}
        }
        
        return s;
    }

    fn print_mem(op: &Operand) -> String {
        assert!(op.offset != 0);
        if op.base == ud_type::UD_NONE && op.index == ud_type::UD_NONE {
            assert!(op.scale == 0 && op.offset != 8);
            format!("0x{:x}", unsafe { match op.offset {
                16 => op.lvalue.uword as u64,
                32 => op.lvalue.udword as u64,
                64 => op.lvalue.uqword,
                _ => panic!("Invalid offset")
            }})
        } else {
            assert!(op.offset != 64);
            let v = unsafe { match op.offset {
                8 => op.lvalue.sbyte as i32,
                16 => op.lvalue.sword as i32,
                32 => op.lvalue.sdword,
                _ => panic!("Invalid offset")
            }};
            if v < 0 {
                format!("-0x{0:x}", -v)
            } else {
                format!("+0x{0:x}", v)
            }
        }
    }

    fn print_relative_address(ud: &ud, op: &Operand) -> String {
        let trunc_mask = 0xffffffffffffffff >> (64 - ud.opr_mode);
        let offset = unsafe { match op.size {
            8 => op.lvalue.sbyte as u64,
            16 => op.lvalue.sword as u64,
            32 => op.lvalue.sdword as u64,
            _ => panic!("Invalid relative offset size")
        }};
        let address = ud.pc.wrapping_add(offset) & trunc_mask;
        format!("0x{:x}", address)
    }

    fn print_immediate(ud: &ud, op: &Operand) -> String {
        let mut v: u64;
        if op.oprcode == 46 /* OP_sI */ && op.size != ud.opr_mode as u16 {
            if op.size == 8 {
                v = unsafe { op.lvalue.sbyte } as u64;
            } else {
                assert!(op.size == 32);
                v = unsafe { op.lvalue.sdword } as u64;
            }
            if ud.opr_mode < 64 {
                v = v & ((1 << ud.opr_mode) - 1);
            }
        } else {
            v = unsafe { match op.size {
                8 => op.lvalue.ubyte as u64,
                16 => op.lvalue.uword as u64,
                32 => op.lvalue.udword as u64,
                64 => op.lvalue.uqword,
                _ => panic!("Invalid offset")
            }};
        }
        format!("0x{:x}", v)
    }

    fn print_reg(reg: ud_type) -> String {
        String::from(match reg {
            ud_type::UD_R_AL => "al",
            ud_type::UD_R_CL => "cl",
            ud_type::UD_R_DL => "dl",
            ud_type::UD_R_BL => "bl",
            ud_type::UD_R_AH => "ah",
            ud_type::UD_R_CH => "ch",
            ud_type::UD_R_DH => "dh",
            ud_type::UD_R_BH => "bh",
            ud_type::UD_R_SPL => "spl",
            ud_type::UD_R_BPL => "bpl",
            ud_type::UD_R_SIL => "sil",                
            ud_type::UD_R_DIL => "dil",                
            ud_type::UD_R_R8B => "r8b",                
            ud_type::UD_R_R9B => "r9b",                
            ud_type::UD_R_R10B => "r10b",                
            ud_type::UD_R_R11B => "r11b",                
            ud_type::UD_R_R12B => "r12b",                
            ud_type::UD_R_R13B => "r13b",                
            ud_type::UD_R_R14B => "r14b",                
            ud_type::UD_R_R15B => "r15b",                
            ud_type::UD_R_AX => "ax",                
            ud_type::UD_R_CX => "cx",                
            ud_type::UD_R_DX => "dx",                
            ud_type::UD_R_BX => "bx",                
            ud_type::UD_R_SP => "sp",                
            ud_type::UD_R_BP => "bp",                
            ud_type::UD_R_SI => "si",                
            ud_type::UD_R_DI => "di",                
            ud_type::UD_R_R8W => "r8w",                
            ud_type::UD_R_R9W => "r9w",                
            ud_type::UD_R_R10W => "r10w",                
            ud_type::UD_R_R11W => "r11w",                
            ud_type::UD_R_R12W => "r12w",                
            ud_type::UD_R_R13W => "r13w",                
            ud_type::UD_R_R14W => "r14w",                
            ud_type::UD_R_R15W => "r15w",                
            ud_type::UD_R_EAX => "eax",                
            ud_type::UD_R_ECX => "ecx",                
            ud_type::UD_R_EDX => "edx",                
            ud_type::UD_R_EBX => "ebx",                
            ud_type::UD_R_ESP => "esp",                
            ud_type::UD_R_EBP => "ebp",                
            ud_type::UD_R_ESI => "esi",                
            ud_type::UD_R_EDI => "edi",                
            ud_type::UD_R_R8D => "r8d",                
            ud_type::UD_R_R9D => "r9d",                
            ud_type::UD_R_R10D => "r10d",                
            ud_type::UD_R_R11D => "r11d",                
            ud_type::UD_R_R12D => "r12d",                
            ud_type::UD_R_R13D => "r13d",                
            ud_type::UD_R_R14D => "r14d",                
            ud_type::UD_R_R15D => "r15d",                
            ud_type::UD_R_RAX => "rax",                
            ud_type::UD_R_RCX => "rcx",                
            ud_type::UD_R_RDX => "rdx",                
            ud_type::UD_R_RBX => "rbx",                
            ud_type::UD_R_RSP => "rsp",                
            ud_type::UD_R_RBP => "rbp",                
            ud_type::UD_R_RSI => "rsi",                
            ud_type::UD_R_RDI => "rdi",                
            ud_type::UD_R_R8 => "r8",                
            ud_type::UD_R_R9 => "r9",                
            ud_type::UD_R_R10 => "r10",                
            ud_type::UD_R_R11 => "r11",                
            ud_type::UD_R_R12 => "r12",                
            ud_type::UD_R_R13 => "r13",                
            ud_type::UD_R_R14 => "r14",                
            ud_type::UD_R_R15 => "r15",                
            ud_type::UD_R_ES => "es",                
            ud_type::UD_R_CS => "cs",                
            ud_type::UD_R_SS => "ss",                
            ud_type::UD_R_DS => "ds",                
            ud_type::UD_R_FS => "fs",                
            ud_type::UD_R_GS => "gs",                
            ud_type::UD_R_CR0 => "cr0",                
            ud_type::UD_R_CR1 => "cr1",                
            ud_type::UD_R_CR2 => "cr2",                
            ud_type::UD_R_CR3 => "cr3",                
            ud_type::UD_R_CR4 => "cr4",                
            ud_type::UD_R_CR5 => "cr5",                
            ud_type::UD_R_CR6 => "cr6",                
            ud_type::UD_R_CR7 => "cr7",                
            ud_type::UD_R_CR8 => "cr8",                
            ud_type::UD_R_CR9 => "cr9",                
            ud_type::UD_R_CR10 => "cr10",                
            ud_type::UD_R_CR11 => "cr11",                
            ud_type::UD_R_CR12 => "cr12",                
            ud_type::UD_R_CR13 => "cr13",                
            ud_type::UD_R_CR14 => "cr14",                
            ud_type::UD_R_CR15 => "cr15",                
            ud_type::UD_R_DR0 => "dr0",                
            ud_type::UD_R_DR1 => "dr1",                
            ud_type::UD_R_DR2 => "dr2",                
            ud_type::UD_R_DR3 => "dr3",                
            ud_type::UD_R_DR4 => "dr4",                
            ud_type::UD_R_DR5 => "dr5",                
            ud_type::UD_R_DR6 => "dr6",                
            ud_type::UD_R_DR7 => "dr7",                
            ud_type::UD_R_DR8 => "dr8",                
            ud_type::UD_R_DR9 => "dr9",
            ud_type::UD_R_DR10 => "dr10",
            ud_type::UD_R_DR11 => "dr11",
            ud_type::UD_R_DR12 => "dr12",
            ud_type::UD_R_DR13 => "dr13",
            ud_type::UD_R_DR14 => "dr14",
            ud_type::UD_R_DR15 => "dr15",
            ud_type::UD_R_MM0 => "mm0",
            ud_type::UD_R_MM1 => "mm1",
            ud_type::UD_R_MM2 => "mm2",
            ud_type::UD_R_MM3 => "mm3",
            ud_type::UD_R_MM4 => "mm4",
            ud_type::UD_R_MM5 => "mm5",
            ud_type::UD_R_MM6 => "mm6",
            ud_type::UD_R_MM7 => "mm7",
            ud_type::UD_R_ST0 => "st0",
            ud_type::UD_R_ST1 => "st1",
            ud_type::UD_R_ST2 => "st2",
            ud_type::UD_R_ST3 => "st3",
            ud_type::UD_R_ST4 => "st4",
            ud_type::UD_R_ST5 => "st5",
            ud_type::UD_R_ST6 => "st6",
            ud_type::UD_R_ST7 => "st7",
            ud_type::UD_R_XMM0 => "xmm0",
            ud_type::UD_R_XMM1 => "xmm1",
            ud_type::UD_R_XMM2 => "xmm2",
            ud_type::UD_R_XMM3 => "xmm3",
            ud_type::UD_R_XMM4 => "xmm4",
            ud_type::UD_R_XMM5 => "xmm5",
            ud_type::UD_R_XMM6 => "xmm6",
            ud_type::UD_R_XMM7 => "xmm7",
            ud_type::UD_R_XMM8 => "xmm8",
            ud_type::UD_R_XMM9 => "xmm9",
            ud_type::UD_R_XMM10 => "xmm10",
            ud_type::UD_R_XMM11 => "xmm11",
            ud_type::UD_R_XMM12 => "xmm12",
            ud_type::UD_R_XMM13 => "xmm13",
            ud_type::UD_R_XMM14 => "xmm14",
            ud_type::UD_R_XMM15 => "xmm15",
            ud_type::UD_R_YMM0 => "ymm0",
            ud_type::UD_R_YMM1 => "ymm1",
            ud_type::UD_R_YMM2 => "ymm2",
            ud_type::UD_R_YMM3 => "ymm3",
            ud_type::UD_R_YMM4 => "ymm4",
            ud_type::UD_R_YMM5 => "ymm5",
            ud_type::UD_R_YMM6 => "ymm6",
            ud_type::UD_R_YMM7 => "ymm7",
            ud_type::UD_R_YMM8 => "ymm8",
            ud_type::UD_R_YMM9 => "ymm9",
            ud_type::UD_R_YMM10 => "ymm10",
            ud_type::UD_R_YMM11 => "ymm11",
            ud_type::UD_R_YMM12 => "ymm12",
            ud_type::UD_R_YMM13 => "ymm13",
            ud_type::UD_R_YMM14 => "ymm14",
            ud_type::UD_R_YMM15 => "ymm15",
            ud_type::UD_R_RIP => "rip",
            _ => panic!("Invalid register")
        })
    }

    fn print_mnemonic(mnemonic: ud_mnemonic_code) -> String {
        String::from(match mnemonic {
            ud_mnemonic_code::UD_Iaaa => "aaa",
            ud_mnemonic_code::UD_Iaad => "aad",
            ud_mnemonic_code::UD_Iaam => "aam",
            ud_mnemonic_code::UD_Iaas => "aas",
            ud_mnemonic_code::UD_Iadc => "adc",
            ud_mnemonic_code::UD_Iadd => "add",
            ud_mnemonic_code::UD_Iaddpd => "addpd",
            ud_mnemonic_code::UD_Iaddps => "addps",
            ud_mnemonic_code::UD_Iaddsd => "addsd",
            ud_mnemonic_code::UD_Iaddss => "addss",
            ud_mnemonic_code::UD_Iaddsubpd => "addsubpd",
            ud_mnemonic_code::UD_Iaddsubps => "addsubps",
            ud_mnemonic_code::UD_Iaesdec => "aesdec",
            ud_mnemonic_code::UD_Iaesdeclast => "aesdeclast",
            ud_mnemonic_code::UD_Iaesenc => "aesenc",
            ud_mnemonic_code::UD_Iaesenclast => "aesenclast",
            ud_mnemonic_code::UD_Iaesimc => "aesimc",
            ud_mnemonic_code::UD_Iaeskeygenassist => "aeskeygenassist",
            ud_mnemonic_code::UD_Iand => "and",
            ud_mnemonic_code::UD_Iandnpd => "andnpd",
            ud_mnemonic_code::UD_Iandnps => "andnps",
            ud_mnemonic_code::UD_Iandpd => "andpd",
            ud_mnemonic_code::UD_Iandps => "andps",
            ud_mnemonic_code::UD_Iarpl => "arpl",
            ud_mnemonic_code::UD_Iblendpd => "blendpd",
            ud_mnemonic_code::UD_Iblendps => "blendps",
            ud_mnemonic_code::UD_Iblendvpd => "blendvpd",
            ud_mnemonic_code::UD_Iblendvps => "blendvps",
            ud_mnemonic_code::UD_Ibound => "bound",
            ud_mnemonic_code::UD_Ibsf => "bsf",
            ud_mnemonic_code::UD_Ibsr => "bsr",
            ud_mnemonic_code::UD_Ibswap => "bswap",
            ud_mnemonic_code::UD_Ibt => "bt",
            ud_mnemonic_code::UD_Ibtc => "btc",
            ud_mnemonic_code::UD_Ibtr => "btr",
            ud_mnemonic_code::UD_Ibts => "bts",
            ud_mnemonic_code::UD_Icall => "call",
            ud_mnemonic_code::UD_Icbw => "cbw",
            ud_mnemonic_code::UD_Icdq => "cdq",
            ud_mnemonic_code::UD_Icdqe => "cdqe",
            ud_mnemonic_code::UD_Iclc => "clc",
            ud_mnemonic_code::UD_Icld => "cld",
            ud_mnemonic_code::UD_Iclflush => "clflush",
            ud_mnemonic_code::UD_Iclgi => "clgi",
            ud_mnemonic_code::UD_Icli => "cli",
            ud_mnemonic_code::UD_Iclts => "clts",
            ud_mnemonic_code::UD_Icmc => "cmc",
            ud_mnemonic_code::UD_Icmova => "cmova",
            ud_mnemonic_code::UD_Icmovae => "cmovae",
            ud_mnemonic_code::UD_Icmovb => "cmovb",
            ud_mnemonic_code::UD_Icmovbe => "cmovbe",
            ud_mnemonic_code::UD_Icmovg => "cmovg",
            ud_mnemonic_code::UD_Icmovge => "cmovge",
            ud_mnemonic_code::UD_Icmovl => "cmovl",
            ud_mnemonic_code::UD_Icmovle => "cmovle",
            ud_mnemonic_code::UD_Icmovno => "cmovno",
            ud_mnemonic_code::UD_Icmovnp => "cmovnp",
            ud_mnemonic_code::UD_Icmovns => "cmovns",
            ud_mnemonic_code::UD_Icmovnz => "cmovnz",
            ud_mnemonic_code::UD_Icmovo => "cmovo",
            ud_mnemonic_code::UD_Icmovp => "cmovp",
            ud_mnemonic_code::UD_Icmovs => "cmovs",
            ud_mnemonic_code::UD_Icmovz => "cmovz",
            ud_mnemonic_code::UD_Icmp => "cmp",
            ud_mnemonic_code::UD_Icmppd => "cmppd",
            ud_mnemonic_code::UD_Icmpps => "cmpps",
            ud_mnemonic_code::UD_Icmpsb => "cmpsb",
            ud_mnemonic_code::UD_Icmpsd => "cmpsd",
            ud_mnemonic_code::UD_Icmpsq => "cmpsq",
            ud_mnemonic_code::UD_Icmpss => "cmpss",
            ud_mnemonic_code::UD_Icmpsw => "cmpsw",
            ud_mnemonic_code::UD_Icmpxchg => "cmpxchg",
            ud_mnemonic_code::UD_Icmpxchg16b => "cmpxchg16b",
            ud_mnemonic_code::UD_Icmpxchg8b => "cmpxchg8b",
            ud_mnemonic_code::UD_Icomisd => "comisd",
            ud_mnemonic_code::UD_Icomiss => "comiss",
            ud_mnemonic_code::UD_Icpuid => "cpuid",
            ud_mnemonic_code::UD_Icqo => "cqo",
            ud_mnemonic_code::UD_Icrc32 => "crc32",
            ud_mnemonic_code::UD_Icvtdq2pd => "cvtdq2pd",
            ud_mnemonic_code::UD_Icvtdq2ps => "cvtdq2ps",
            ud_mnemonic_code::UD_Icvtpd2dq => "cvtpd2dq",
            ud_mnemonic_code::UD_Icvtpd2pi => "cvtpd2pi",
            ud_mnemonic_code::UD_Icvtpd2ps => "cvtpd2ps",
            ud_mnemonic_code::UD_Icvtpi2pd => "cvtpi2pd",
            ud_mnemonic_code::UD_Icvtpi2ps => "cvtpi2ps",
            ud_mnemonic_code::UD_Icvtps2dq => "cvtps2dq",
            ud_mnemonic_code::UD_Icvtps2pd => "cvtps2pd",
            ud_mnemonic_code::UD_Icvtps2pi => "cvtps2pi",
            ud_mnemonic_code::UD_Icvtsd2si => "cvtsd2si",
            ud_mnemonic_code::UD_Icvtsd2ss => "cvtsd2ss",
            ud_mnemonic_code::UD_Icvtsi2sd => "cvtsi2sd",
            ud_mnemonic_code::UD_Icvtsi2ss => "cvtsi2ss",
            ud_mnemonic_code::UD_Icvtss2sd => "cvtss2sd",
            ud_mnemonic_code::UD_Icvtss2si => "cvtss2si",
            ud_mnemonic_code::UD_Icvttpd2dq => "cvttpd2dq",
            ud_mnemonic_code::UD_Icvttpd2pi => "cvttpd2pi",
            ud_mnemonic_code::UD_Icvttps2dq => "cvttps2dq",
            ud_mnemonic_code::UD_Icvttps2pi => "cvttps2pi",
            ud_mnemonic_code::UD_Icvttsd2si => "cvttsd2si",
            ud_mnemonic_code::UD_Icvttss2si => "cvttss2si",
            ud_mnemonic_code::UD_Icwd => "cwd",
            ud_mnemonic_code::UD_Icwde => "cwde",
            ud_mnemonic_code::UD_Idaa => "daa",
            ud_mnemonic_code::UD_Idas => "das",
            ud_mnemonic_code::UD_Idec => "dec",
            ud_mnemonic_code::UD_Idiv => "div",
            ud_mnemonic_code::UD_Idivpd => "divpd",
            ud_mnemonic_code::UD_Idivps => "divps",
            ud_mnemonic_code::UD_Idivsd => "divsd",
            ud_mnemonic_code::UD_Idivss => "divss",
            ud_mnemonic_code::UD_Idppd => "dppd",
            ud_mnemonic_code::UD_Idpps => "dpps",
            ud_mnemonic_code::UD_Iemms => "emms",
            ud_mnemonic_code::UD_Ienter => "enter",
            ud_mnemonic_code::UD_Iextractps => "extractps",
            ud_mnemonic_code::UD_If2xm1 => "f2xm1",
            ud_mnemonic_code::UD_Ifabs => "fabs",
            ud_mnemonic_code::UD_Ifadd => "fadd",
            ud_mnemonic_code::UD_Ifaddp => "faddp",
            ud_mnemonic_code::UD_Ifbld => "fbld",
            ud_mnemonic_code::UD_Ifbstp => "fbstp",
            ud_mnemonic_code::UD_Ifchs => "fchs",
            ud_mnemonic_code::UD_Ifclex => "fclex",
            ud_mnemonic_code::UD_Ifcmovb => "fcmovb",
            ud_mnemonic_code::UD_Ifcmovbe => "fcmovbe",
            ud_mnemonic_code::UD_Ifcmove => "fcmove",
            ud_mnemonic_code::UD_Ifcmovnb => "fcmovnb",
            ud_mnemonic_code::UD_Ifcmovnbe => "fcmovnbe",
            ud_mnemonic_code::UD_Ifcmovne => "fcmovne",
            ud_mnemonic_code::UD_Ifcmovnu => "fcmovnu",
            ud_mnemonic_code::UD_Ifcmovu => "fcmovu",
            ud_mnemonic_code::UD_Ifcom => "fcom",
            ud_mnemonic_code::UD_Ifcom2 => "fcom2",
            ud_mnemonic_code::UD_Ifcomi => "fcomi",
            ud_mnemonic_code::UD_Ifcomip => "fcomip",
            ud_mnemonic_code::UD_Ifcomp => "fcomp",
            ud_mnemonic_code::UD_Ifcomp3 => "fcomp3",
            ud_mnemonic_code::UD_Ifcomp5 => "fcomp5",
            ud_mnemonic_code::UD_Ifcompp => "fcompp",
            ud_mnemonic_code::UD_Ifcos => "fcos",
            ud_mnemonic_code::UD_Ifdecstp => "fdecstp",
            ud_mnemonic_code::UD_Ifdiv => "fdiv",
            ud_mnemonic_code::UD_Ifdivp => "fdivp",
            ud_mnemonic_code::UD_Ifdivr => "fdivr",
            ud_mnemonic_code::UD_Ifdivrp => "fdivrp",
            ud_mnemonic_code::UD_Ifemms => "femms",
            ud_mnemonic_code::UD_Iffree => "ffree",
            ud_mnemonic_code::UD_Iffreep => "ffreep",
            ud_mnemonic_code::UD_Ifiadd => "fiadd",
            ud_mnemonic_code::UD_Ificom => "ficom",
            ud_mnemonic_code::UD_Ificomp => "ficomp",
            ud_mnemonic_code::UD_Ifidiv => "fidiv",
            ud_mnemonic_code::UD_Ifidivr => "fidivr",
            ud_mnemonic_code::UD_Ifild => "fild",
            ud_mnemonic_code::UD_Ifimul => "fimul",
            ud_mnemonic_code::UD_Ifincstp => "fincstp",
            ud_mnemonic_code::UD_Ifist => "fist",
            ud_mnemonic_code::UD_Ifistp => "fistp",
            ud_mnemonic_code::UD_Ifisttp => "fisttp",
            ud_mnemonic_code::UD_Ifisub => "fisub",
            ud_mnemonic_code::UD_Ifisubr => "fisubr",
            ud_mnemonic_code::UD_Ifld => "fld",
            ud_mnemonic_code::UD_Ifld1 => "fld1",
            ud_mnemonic_code::UD_Ifldcw => "fldcw",
            ud_mnemonic_code::UD_Ifldenv => "fldenv",
            ud_mnemonic_code::UD_Ifldl2e => "fldl2e",
            ud_mnemonic_code::UD_Ifldl2t => "fldl2t",
            ud_mnemonic_code::UD_Ifldlg2 => "fldlg2",
            ud_mnemonic_code::UD_Ifldln2 => "fldln2",
            ud_mnemonic_code::UD_Ifldpi => "fldpi",
            ud_mnemonic_code::UD_Ifldz => "fldz",
            ud_mnemonic_code::UD_Ifmul => "fmul",
            ud_mnemonic_code::UD_Ifmulp => "fmulp",
            ud_mnemonic_code::UD_Ifndisi => "fndisi",
            ud_mnemonic_code::UD_Ifneni => "fneni",
            ud_mnemonic_code::UD_Ifninit => "fninit",
            ud_mnemonic_code::UD_Ifnop => "fnop",
            ud_mnemonic_code::UD_Ifnsave => "fnsave",
            ud_mnemonic_code::UD_Ifnsetpm => "fnsetpm",
            ud_mnemonic_code::UD_Ifnstcw => "fnstcw",
            ud_mnemonic_code::UD_Ifnstenv => "fnstenv",
            ud_mnemonic_code::UD_Ifnstsw => "fnstsw",
            ud_mnemonic_code::UD_Ifpatan => "fpatan",
            ud_mnemonic_code::UD_Ifprem => "fprem",
            ud_mnemonic_code::UD_Ifprem1 => "fprem1",
            ud_mnemonic_code::UD_Ifptan => "fptan",
            ud_mnemonic_code::UD_Ifrndint => "frndint",
            ud_mnemonic_code::UD_Ifrstor => "frstor",
            ud_mnemonic_code::UD_Ifrstpm => "frstpm",
            ud_mnemonic_code::UD_Ifscale => "fscale",
            ud_mnemonic_code::UD_Ifsin => "fsin",
            ud_mnemonic_code::UD_Ifsincos => "fsincos",
            ud_mnemonic_code::UD_Ifsqrt => "fsqrt",
            ud_mnemonic_code::UD_Ifst => "fst",
            ud_mnemonic_code::UD_Ifstp => "fstp",
            ud_mnemonic_code::UD_Ifstp1 => "fstp1",
            ud_mnemonic_code::UD_Ifstp8 => "fstp8",
            ud_mnemonic_code::UD_Ifstp9 => "fstp9",
            ud_mnemonic_code::UD_Ifsub => "fsub",
            ud_mnemonic_code::UD_Ifsubp => "fsubp",
            ud_mnemonic_code::UD_Ifsubr => "fsubr",
            ud_mnemonic_code::UD_Ifsubrp => "fsubrp",
            ud_mnemonic_code::UD_Iftst => "ftst",
            ud_mnemonic_code::UD_Ifucom => "fucom",
            ud_mnemonic_code::UD_Ifucomi => "fucomi",
            ud_mnemonic_code::UD_Ifucomip => "fucomip",
            ud_mnemonic_code::UD_Ifucomp => "fucomp",
            ud_mnemonic_code::UD_Ifucompp => "fucompp",
            ud_mnemonic_code::UD_Ifxam => "fxam",
            ud_mnemonic_code::UD_Ifxch => "fxch",
            ud_mnemonic_code::UD_Ifxch4 => "fxch4",
            ud_mnemonic_code::UD_Ifxch7 => "fxch7",
            ud_mnemonic_code::UD_Ifxrstor => "fxrstor",
            ud_mnemonic_code::UD_Ifxsave => "fxsave",
            ud_mnemonic_code::UD_Ifxtract => "fxtract",
            ud_mnemonic_code::UD_Ifyl2x => "fyl2x",
            ud_mnemonic_code::UD_Ifyl2xp1 => "fyl2xp1",
            ud_mnemonic_code::UD_Igetsec => "getsec",
            ud_mnemonic_code::UD_Ihaddpd => "haddpd",
            ud_mnemonic_code::UD_Ihaddps => "haddps",
            ud_mnemonic_code::UD_Ihlt => "hlt",
            ud_mnemonic_code::UD_Ihsubpd => "hsubpd",
            ud_mnemonic_code::UD_Ihsubps => "hsubps",
            ud_mnemonic_code::UD_Iidiv => "idiv",
            ud_mnemonic_code::UD_Iimul => "imul",
            ud_mnemonic_code::UD_Iin => "in",
            ud_mnemonic_code::UD_Iinc => "inc",
            ud_mnemonic_code::UD_Iinsb => "insb",
            ud_mnemonic_code::UD_Iinsd => "insd",
            ud_mnemonic_code::UD_Iinsertps => "insertps",
            ud_mnemonic_code::UD_Iinsw => "insw",
            ud_mnemonic_code::UD_Iint => "int",
            ud_mnemonic_code::UD_Iint1 => "int1",
            ud_mnemonic_code::UD_Iint3 => "int3",
            ud_mnemonic_code::UD_Iinto => "into",
            ud_mnemonic_code::UD_Iinvd => "invd",
            ud_mnemonic_code::UD_Iinvept => "invept",
            ud_mnemonic_code::UD_Iinvlpg => "invlpg",
            ud_mnemonic_code::UD_Iinvlpga => "invlpga",
            ud_mnemonic_code::UD_Iinvvpid => "invvpid",
            ud_mnemonic_code::UD_Iiretd => "iretd",
            ud_mnemonic_code::UD_Iiretq => "iretq",
            ud_mnemonic_code::UD_Iiretw => "iretw",
            ud_mnemonic_code::UD_Ija => "ja",
            ud_mnemonic_code::UD_Ijae => "jae",
            ud_mnemonic_code::UD_Ijb => "jb",
            ud_mnemonic_code::UD_Ijbe => "jbe",
            ud_mnemonic_code::UD_Ijcxz => "jcxz",
            ud_mnemonic_code::UD_Ijecxz => "jecxz",
            ud_mnemonic_code::UD_Ijg => "jg",
            ud_mnemonic_code::UD_Ijge => "jge",
            ud_mnemonic_code::UD_Ijl => "jl",
            ud_mnemonic_code::UD_Ijle => "jle",
            ud_mnemonic_code::UD_Ijmp => "jmp",
            ud_mnemonic_code::UD_Ijno => "jno",
            ud_mnemonic_code::UD_Ijnp => "jnp",
            ud_mnemonic_code::UD_Ijns => "jns",
            ud_mnemonic_code::UD_Ijnz => "jnz",
            ud_mnemonic_code::UD_Ijo => "jo",
            ud_mnemonic_code::UD_Ijp => "jp",
            ud_mnemonic_code::UD_Ijrcxz => "jrcxz",
            ud_mnemonic_code::UD_Ijs => "js",
            ud_mnemonic_code::UD_Ijz => "jz",
            ud_mnemonic_code::UD_Ilahf => "lahf",
            ud_mnemonic_code::UD_Ilar => "lar",
            ud_mnemonic_code::UD_Ilddqu => "lddqu",
            ud_mnemonic_code::UD_Ildmxcsr => "ldmxcsr",
            ud_mnemonic_code::UD_Ilds => "lds",
            ud_mnemonic_code::UD_Ilea => "lea",
            ud_mnemonic_code::UD_Ileave => "leave",
            ud_mnemonic_code::UD_Iles => "les",
            ud_mnemonic_code::UD_Ilfence => "lfence",
            ud_mnemonic_code::UD_Ilfs => "lfs",
            ud_mnemonic_code::UD_Ilgdt => "lgdt",
            ud_mnemonic_code::UD_Ilgs => "lgs",
            ud_mnemonic_code::UD_Ilidt => "lidt",
            ud_mnemonic_code::UD_Illdt => "lldt",
            ud_mnemonic_code::UD_Ilmsw => "lmsw",
            ud_mnemonic_code::UD_Ilock => "lock",
            ud_mnemonic_code::UD_Ilodsb => "lodsb",
            ud_mnemonic_code::UD_Ilodsd => "lodsd",
            ud_mnemonic_code::UD_Ilodsq => "lodsq",
            ud_mnemonic_code::UD_Ilodsw => "lodsw",
            ud_mnemonic_code::UD_Iloop => "loop",
            ud_mnemonic_code::UD_Iloope => "loope",
            ud_mnemonic_code::UD_Iloopne => "loopne",
            ud_mnemonic_code::UD_Ilsl => "lsl",
            ud_mnemonic_code::UD_Ilss => "lss",
            ud_mnemonic_code::UD_Iltr => "ltr",
            ud_mnemonic_code::UD_Imaskmovdqu => "maskmovdqu",
            ud_mnemonic_code::UD_Imaskmovq => "maskmovq",
            ud_mnemonic_code::UD_Imaxpd => "maxpd",
            ud_mnemonic_code::UD_Imaxps => "maxps",
            ud_mnemonic_code::UD_Imaxsd => "maxsd",
            ud_mnemonic_code::UD_Imaxss => "maxss",
            ud_mnemonic_code::UD_Imfence => "mfence",
            ud_mnemonic_code::UD_Iminpd => "minpd",
            ud_mnemonic_code::UD_Iminps => "minps",
            ud_mnemonic_code::UD_Iminsd => "minsd",
            ud_mnemonic_code::UD_Iminss => "minss",
            ud_mnemonic_code::UD_Imonitor => "monitor",
            ud_mnemonic_code::UD_Imontmul => "montmul",
            ud_mnemonic_code::UD_Imov => "mov",
            ud_mnemonic_code::UD_Imovapd => "movapd",
            ud_mnemonic_code::UD_Imovaps => "movaps",
            ud_mnemonic_code::UD_Imovbe => "movbe",
            ud_mnemonic_code::UD_Imovd => "movd",
            ud_mnemonic_code::UD_Imovddup => "movddup",
            ud_mnemonic_code::UD_Imovdq2q => "movdq2q",
            ud_mnemonic_code::UD_Imovdqa => "movdqa",
            ud_mnemonic_code::UD_Imovdqu => "movdqu",
            ud_mnemonic_code::UD_Imovhlps => "movhlps",
            ud_mnemonic_code::UD_Imovhpd => "movhpd",
            ud_mnemonic_code::UD_Imovhps => "movhps",
            ud_mnemonic_code::UD_Imovlhps => "movlhps",
            ud_mnemonic_code::UD_Imovlpd => "movlpd",
            ud_mnemonic_code::UD_Imovlps => "movlps",
            ud_mnemonic_code::UD_Imovmskpd => "movmskpd",
            ud_mnemonic_code::UD_Imovmskps => "movmskps",
            ud_mnemonic_code::UD_Imovntdq => "movntdq",
            ud_mnemonic_code::UD_Imovntdqa => "movntdqa",
            ud_mnemonic_code::UD_Imovnti => "movnti",
            ud_mnemonic_code::UD_Imovntpd => "movntpd",
            ud_mnemonic_code::UD_Imovntps => "movntps",
            ud_mnemonic_code::UD_Imovntq => "movntq",
            ud_mnemonic_code::UD_Imovq => "movq",
            ud_mnemonic_code::UD_Imovq2dq => "movq2dq",
            ud_mnemonic_code::UD_Imovsb => "movsb",
            ud_mnemonic_code::UD_Imovsd => "movsd",
            ud_mnemonic_code::UD_Imovshdup => "movshdup",
            ud_mnemonic_code::UD_Imovsldup => "movsldup",
            ud_mnemonic_code::UD_Imovsq => "movsq",
            ud_mnemonic_code::UD_Imovss => "movss",
            ud_mnemonic_code::UD_Imovsw => "movsw",
            ud_mnemonic_code::UD_Imovsx => "movsx",
            ud_mnemonic_code::UD_Imovsxd => "movsxd",
            ud_mnemonic_code::UD_Imovupd => "movupd",
            ud_mnemonic_code::UD_Imovups => "movups",
            ud_mnemonic_code::UD_Imovzx => "movzx",
            ud_mnemonic_code::UD_Impsadbw => "mpsadbw",
            ud_mnemonic_code::UD_Imul => "mul",
            ud_mnemonic_code::UD_Imulpd => "mulpd",
            ud_mnemonic_code::UD_Imulps => "mulps",
            ud_mnemonic_code::UD_Imulsd => "mulsd",
            ud_mnemonic_code::UD_Imulss => "mulss",
            ud_mnemonic_code::UD_Imwait => "mwait",
            ud_mnemonic_code::UD_Ineg => "neg",
            ud_mnemonic_code::UD_Inop => "nop",
            ud_mnemonic_code::UD_Inot => "not",
            ud_mnemonic_code::UD_Ior => "or",
            ud_mnemonic_code::UD_Iorpd => "orpd",
            ud_mnemonic_code::UD_Iorps => "orps",
            ud_mnemonic_code::UD_Iout => "out",
            ud_mnemonic_code::UD_Ioutsb => "outsb",
            ud_mnemonic_code::UD_Ioutsd => "outsd",
            ud_mnemonic_code::UD_Ioutsw => "outsw",
            ud_mnemonic_code::UD_Ipabsb => "pabsb",
            ud_mnemonic_code::UD_Ipabsd => "pabsd",
            ud_mnemonic_code::UD_Ipabsw => "pabsw",
            ud_mnemonic_code::UD_Ipackssdw => "packssdw",
            ud_mnemonic_code::UD_Ipacksswb => "packsswb",
            ud_mnemonic_code::UD_Ipackusdw => "packusdw",
            ud_mnemonic_code::UD_Ipackuswb => "packuswb",
            ud_mnemonic_code::UD_Ipaddb => "paddb",
            ud_mnemonic_code::UD_Ipaddd => "paddd",
            ud_mnemonic_code::UD_Ipaddq => "paddq",
            ud_mnemonic_code::UD_Ipaddsb => "paddsb",
            ud_mnemonic_code::UD_Ipaddsw => "paddsw",
            ud_mnemonic_code::UD_Ipaddusb => "paddusb",
            ud_mnemonic_code::UD_Ipaddusw => "paddusw",
            ud_mnemonic_code::UD_Ipaddw => "paddw",
            ud_mnemonic_code::UD_Ipalignr => "palignr",
            ud_mnemonic_code::UD_Ipand => "pand",
            ud_mnemonic_code::UD_Ipandn => "pandn",
            ud_mnemonic_code::UD_Ipavgb => "pavgb",
            ud_mnemonic_code::UD_Ipavgusb => "pavgusb",
            ud_mnemonic_code::UD_Ipavgw => "pavgw",
            ud_mnemonic_code::UD_Ipblendvb => "pblendvb",
            ud_mnemonic_code::UD_Ipblendw => "pblendw",
            ud_mnemonic_code::UD_Ipclmulqdq => "pclmulqdq",
            ud_mnemonic_code::UD_Ipcmpeqb => "pcmpeqb",
            ud_mnemonic_code::UD_Ipcmpeqd => "pcmpeqd",
            ud_mnemonic_code::UD_Ipcmpeqq => "pcmpeqq",
            ud_mnemonic_code::UD_Ipcmpeqw => "pcmpeqw",
            ud_mnemonic_code::UD_Ipcmpestri => "pcmpestri",
            ud_mnemonic_code::UD_Ipcmpestrm => "pcmpestrm",
            ud_mnemonic_code::UD_Ipcmpgtb => "pcmpgtb",
            ud_mnemonic_code::UD_Ipcmpgtd => "pcmpgtd",
            ud_mnemonic_code::UD_Ipcmpgtq => "pcmpgtq",
            ud_mnemonic_code::UD_Ipcmpgtw => "pcmpgtw",
            ud_mnemonic_code::UD_Ipcmpistri => "pcmpistri",
            ud_mnemonic_code::UD_Ipcmpistrm => "pcmpistrm",
            ud_mnemonic_code::UD_Ipextrb => "pextrb",
            ud_mnemonic_code::UD_Ipextrd => "pextrd",
            ud_mnemonic_code::UD_Ipextrq => "pextrq",
            ud_mnemonic_code::UD_Ipextrw => "pextrw",
            ud_mnemonic_code::UD_Ipf2id => "pf2id",
            ud_mnemonic_code::UD_Ipf2iw => "pf2iw",
            ud_mnemonic_code::UD_Ipfacc => "pfacc",
            ud_mnemonic_code::UD_Ipfadd => "pfadd",
            ud_mnemonic_code::UD_Ipfcmpeq => "pfcmpeq",
            ud_mnemonic_code::UD_Ipfcmpge => "pfcmpge",
            ud_mnemonic_code::UD_Ipfcmpgt => "pfcmpgt",
            ud_mnemonic_code::UD_Ipfmax => "pfmax",
            ud_mnemonic_code::UD_Ipfmin => "pfmin",
            ud_mnemonic_code::UD_Ipfmul => "pfmul",
            ud_mnemonic_code::UD_Ipfnacc => "pfnacc",
            ud_mnemonic_code::UD_Ipfpnacc => "pfpnacc",
            ud_mnemonic_code::UD_Ipfrcp => "pfrcp",
            ud_mnemonic_code::UD_Ipfrcpit1 => "pfrcpit1",
            ud_mnemonic_code::UD_Ipfrcpit2 => "pfrcpit2",
            ud_mnemonic_code::UD_Ipfrsqit1 => "pfrsqit1",
            ud_mnemonic_code::UD_Ipfrsqrt => "pfrsqrt",
            ud_mnemonic_code::UD_Ipfsub => "pfsub",
            ud_mnemonic_code::UD_Ipfsubr => "pfsubr",
            ud_mnemonic_code::UD_Iphaddd => "phaddd",
            ud_mnemonic_code::UD_Iphaddsw => "phaddsw",
            ud_mnemonic_code::UD_Iphaddw => "phaddw",
            ud_mnemonic_code::UD_Iphminposuw => "phminposuw",
            ud_mnemonic_code::UD_Iphsubd => "phsubd",
            ud_mnemonic_code::UD_Iphsubsw => "phsubsw",
            ud_mnemonic_code::UD_Iphsubw => "phsubw",
            ud_mnemonic_code::UD_Ipi2fd => "pi2fd",
            ud_mnemonic_code::UD_Ipi2fw => "pi2fw",
            ud_mnemonic_code::UD_Ipinsrb => "pinsrb",
            ud_mnemonic_code::UD_Ipinsrd => "pinsrd",
            ud_mnemonic_code::UD_Ipinsrq => "pinsrq",
            ud_mnemonic_code::UD_Ipinsrw => "pinsrw",
            ud_mnemonic_code::UD_Ipmaddubsw => "pmaddubsw",
            ud_mnemonic_code::UD_Ipmaddwd => "pmaddwd",
            ud_mnemonic_code::UD_Ipmaxsb => "pmaxsb",
            ud_mnemonic_code::UD_Ipmaxsd => "pmaxsd",
            ud_mnemonic_code::UD_Ipmaxsw => "pmaxsw",
            ud_mnemonic_code::UD_Ipmaxub => "pmaxub",
            ud_mnemonic_code::UD_Ipmaxud => "pmaxud",
            ud_mnemonic_code::UD_Ipmaxuw => "pmaxuw",
            ud_mnemonic_code::UD_Ipminsb => "pminsb",
            ud_mnemonic_code::UD_Ipminsd => "pminsd",
            ud_mnemonic_code::UD_Ipminsw => "pminsw",
            ud_mnemonic_code::UD_Ipminub => "pminub",
            ud_mnemonic_code::UD_Ipminud => "pminud",
            ud_mnemonic_code::UD_Ipminuw => "pminuw",
            ud_mnemonic_code::UD_Ipmovmskb => "pmovmskb",
            ud_mnemonic_code::UD_Ipmovsxbd => "pmovsxbd",
            ud_mnemonic_code::UD_Ipmovsxbq => "pmovsxbq",
            ud_mnemonic_code::UD_Ipmovsxbw => "pmovsxbw",
            ud_mnemonic_code::UD_Ipmovsxdq => "pmovsxdq",
            ud_mnemonic_code::UD_Ipmovsxwd => "pmovsxwd",
            ud_mnemonic_code::UD_Ipmovsxwq => "pmovsxwq",
            ud_mnemonic_code::UD_Ipmovzxbd => "pmovzxbd",
            ud_mnemonic_code::UD_Ipmovzxbq => "pmovzxbq",
            ud_mnemonic_code::UD_Ipmovzxbw => "pmovzxbw",
            ud_mnemonic_code::UD_Ipmovzxdq => "pmovzxdq",
            ud_mnemonic_code::UD_Ipmovzxwd => "pmovzxwd",
            ud_mnemonic_code::UD_Ipmovzxwq => "pmovzxwq",
            ud_mnemonic_code::UD_Ipmuldq => "pmuldq",
            ud_mnemonic_code::UD_Ipmulhrsw => "pmulhrsw",
            ud_mnemonic_code::UD_Ipmulhrw => "pmulhrw",
            ud_mnemonic_code::UD_Ipmulhuw => "pmulhuw",
            ud_mnemonic_code::UD_Ipmulhw => "pmulhw",
            ud_mnemonic_code::UD_Ipmulld => "pmulld",
            ud_mnemonic_code::UD_Ipmullw => "pmullw",
            ud_mnemonic_code::UD_Ipmuludq => "pmuludq",
            ud_mnemonic_code::UD_Ipop => "pop",
            ud_mnemonic_code::UD_Ipopa => "popa",
            ud_mnemonic_code::UD_Ipopad => "popad",
            ud_mnemonic_code::UD_Ipopcnt => "popcnt",
            ud_mnemonic_code::UD_Ipopfd => "popfd",
            ud_mnemonic_code::UD_Ipopfq => "popfq",
            ud_mnemonic_code::UD_Ipopfw => "popfw",
            ud_mnemonic_code::UD_Ipor => "por",
            ud_mnemonic_code::UD_Iprefetch => "prefetch",
            ud_mnemonic_code::UD_Iprefetchnta => "prefetchnta",
            ud_mnemonic_code::UD_Iprefetcht0 => "prefetcht0",
            ud_mnemonic_code::UD_Iprefetcht1 => "prefetcht1",
            ud_mnemonic_code::UD_Iprefetcht2 => "prefetcht2",
            ud_mnemonic_code::UD_Ipsadbw => "psadbw",
            ud_mnemonic_code::UD_Ipshufb => "pshufb",
            ud_mnemonic_code::UD_Ipshufd => "pshufd",
            ud_mnemonic_code::UD_Ipshufhw => "pshufhw",
            ud_mnemonic_code::UD_Ipshuflw => "pshuflw",
            ud_mnemonic_code::UD_Ipshufw => "pshufw",
            ud_mnemonic_code::UD_Ipsignb => "psignb",
            ud_mnemonic_code::UD_Ipsignd => "psignd",
            ud_mnemonic_code::UD_Ipsignw => "psignw",
            ud_mnemonic_code::UD_Ipslld => "pslld",
            ud_mnemonic_code::UD_Ipslldq => "pslldq",
            ud_mnemonic_code::UD_Ipsllq => "psllq",
            ud_mnemonic_code::UD_Ipsllw => "psllw",
            ud_mnemonic_code::UD_Ipsrad => "psrad",
            ud_mnemonic_code::UD_Ipsraw => "psraw",
            ud_mnemonic_code::UD_Ipsrld => "psrld",
            ud_mnemonic_code::UD_Ipsrldq => "psrldq",
            ud_mnemonic_code::UD_Ipsrlq => "psrlq",
            ud_mnemonic_code::UD_Ipsrlw => "psrlw",
            ud_mnemonic_code::UD_Ipsubb => "psubb",
            ud_mnemonic_code::UD_Ipsubd => "psubd",
            ud_mnemonic_code::UD_Ipsubq => "psubq",
            ud_mnemonic_code::UD_Ipsubsb => "psubsb",
            ud_mnemonic_code::UD_Ipsubsw => "psubsw",
            ud_mnemonic_code::UD_Ipsubusb => "psubusb",
            ud_mnemonic_code::UD_Ipsubusw => "psubusw",
            ud_mnemonic_code::UD_Ipsubw => "psubw",
            ud_mnemonic_code::UD_Ipswapd => "pswapd",
            ud_mnemonic_code::UD_Iptest => "ptest",
            ud_mnemonic_code::UD_Ipunpckhbw => "punpckhbw",
            ud_mnemonic_code::UD_Ipunpckhdq => "punpckhdq",
            ud_mnemonic_code::UD_Ipunpckhqdq => "punpckhqdq",
            ud_mnemonic_code::UD_Ipunpckhwd => "punpckhwd",
            ud_mnemonic_code::UD_Ipunpcklbw => "punpcklbw",
            ud_mnemonic_code::UD_Ipunpckldq => "punpckldq",
            ud_mnemonic_code::UD_Ipunpcklqdq => "punpcklqdq",
            ud_mnemonic_code::UD_Ipunpcklwd => "punpcklwd",
            ud_mnemonic_code::UD_Ipush => "push",
            ud_mnemonic_code::UD_Ipusha => "pusha",
            ud_mnemonic_code::UD_Ipushad => "pushad",
            ud_mnemonic_code::UD_Ipushfd => "pushfd",
            ud_mnemonic_code::UD_Ipushfq => "pushfq",
            ud_mnemonic_code::UD_Ipushfw => "pushfw",
            ud_mnemonic_code::UD_Ipxor => "pxor",
            ud_mnemonic_code::UD_Ircl => "rcl",
            ud_mnemonic_code::UD_Ircpps => "rcpps",
            ud_mnemonic_code::UD_Ircpss => "rcpss",
            ud_mnemonic_code::UD_Ircr => "rcr",
            ud_mnemonic_code::UD_Irdmsr => "rdmsr",
            ud_mnemonic_code::UD_Irdpmc => "rdpmc",
            ud_mnemonic_code::UD_Irdrand => "rdrand",
            ud_mnemonic_code::UD_Irdtsc => "rdtsc",
            ud_mnemonic_code::UD_Irdtscp => "rdtscp",
            ud_mnemonic_code::UD_Irep => "rep",
            ud_mnemonic_code::UD_Irepne => "repne",
            ud_mnemonic_code::UD_Iret => "ret",
            ud_mnemonic_code::UD_Iretf => "retf",
            ud_mnemonic_code::UD_Irol => "rol",
            ud_mnemonic_code::UD_Iror => "ror",
            ud_mnemonic_code::UD_Iroundpd => "roundpd",
            ud_mnemonic_code::UD_Iroundps => "roundps",
            ud_mnemonic_code::UD_Iroundsd => "roundsd",
            ud_mnemonic_code::UD_Iroundss => "roundss",
            ud_mnemonic_code::UD_Irsm => "rsm",
            ud_mnemonic_code::UD_Irsqrtps => "rsqrtps",
            ud_mnemonic_code::UD_Irsqrtss => "rsqrtss",
            ud_mnemonic_code::UD_Isahf => "sahf",
            ud_mnemonic_code::UD_Isalc => "salc",
            ud_mnemonic_code::UD_Isar => "sar",
            ud_mnemonic_code::UD_Isbb => "sbb",
            ud_mnemonic_code::UD_Iscasb => "scasb",
            ud_mnemonic_code::UD_Iscasd => "scasd",
            ud_mnemonic_code::UD_Iscasq => "scasq",
            ud_mnemonic_code::UD_Iscasw => "scasw",
            ud_mnemonic_code::UD_Iseta => "seta",
            ud_mnemonic_code::UD_Isetae => "setae",
            ud_mnemonic_code::UD_Isetb => "setb",
            ud_mnemonic_code::UD_Isetbe => "setbe",
            ud_mnemonic_code::UD_Isetg => "setg",
            ud_mnemonic_code::UD_Isetge => "setge",
            ud_mnemonic_code::UD_Isetl => "setl",
            ud_mnemonic_code::UD_Isetle => "setle",
            ud_mnemonic_code::UD_Isetno => "setno",
            ud_mnemonic_code::UD_Isetnp => "setnp",
            ud_mnemonic_code::UD_Isetns => "setns",
            ud_mnemonic_code::UD_Isetnz => "setnz",
            ud_mnemonic_code::UD_Iseto => "seto",
            ud_mnemonic_code::UD_Isetp => "setp",
            ud_mnemonic_code::UD_Isets => "sets",
            ud_mnemonic_code::UD_Isetz => "setz",
            ud_mnemonic_code::UD_Isfence => "sfence",
            ud_mnemonic_code::UD_Isgdt => "sgdt",
            ud_mnemonic_code::UD_Ishl => "shl",
            ud_mnemonic_code::UD_Ishld => "shld",
            ud_mnemonic_code::UD_Ishr => "shr",
            ud_mnemonic_code::UD_Ishrd => "shrd",
            ud_mnemonic_code::UD_Ishufpd => "shufpd",
            ud_mnemonic_code::UD_Ishufps => "shufps",
            ud_mnemonic_code::UD_Isidt => "sidt",
            ud_mnemonic_code::UD_Iskinit => "skinit",
            ud_mnemonic_code::UD_Isldt => "sldt",
            ud_mnemonic_code::UD_Ismsw => "smsw",
            ud_mnemonic_code::UD_Isqrtpd => "sqrtpd",
            ud_mnemonic_code::UD_Isqrtps => "sqrtps",
            ud_mnemonic_code::UD_Isqrtsd => "sqrtsd",
            ud_mnemonic_code::UD_Isqrtss => "sqrtss",
            ud_mnemonic_code::UD_Istc => "stc",
            ud_mnemonic_code::UD_Istd => "std",
            ud_mnemonic_code::UD_Istgi => "stgi",
            ud_mnemonic_code::UD_Isti => "sti",
            ud_mnemonic_code::UD_Istmxcsr => "stmxcsr",
            ud_mnemonic_code::UD_Istosb => "stosb",
            ud_mnemonic_code::UD_Istosd => "stosd",
            ud_mnemonic_code::UD_Istosq => "stosq",
            ud_mnemonic_code::UD_Istosw => "stosw",
            ud_mnemonic_code::UD_Istr => "str",
            ud_mnemonic_code::UD_Isub => "sub",
            ud_mnemonic_code::UD_Isubpd => "subpd",
            ud_mnemonic_code::UD_Isubps => "subps",
            ud_mnemonic_code::UD_Isubsd => "subsd",
            ud_mnemonic_code::UD_Isubss => "subss",
            ud_mnemonic_code::UD_Iswapgs => "swapgs",
            ud_mnemonic_code::UD_Isyscall => "syscall",
            ud_mnemonic_code::UD_Isysenter => "sysenter",
            ud_mnemonic_code::UD_Isysexit => "sysexit",
            ud_mnemonic_code::UD_Isysret => "sysret",
            ud_mnemonic_code::UD_Itest => "test",
            ud_mnemonic_code::UD_Iucomisd => "ucomisd",
            ud_mnemonic_code::UD_Iucomiss => "ucomiss",
            ud_mnemonic_code::UD_Iud2 => "ud2",
            ud_mnemonic_code::UD_Iunpckhpd => "unpckhpd",
            ud_mnemonic_code::UD_Iunpckhps => "unpckhps",
            ud_mnemonic_code::UD_Iunpcklpd => "unpcklpd",
            ud_mnemonic_code::UD_Iunpcklps => "unpcklps",
            ud_mnemonic_code::UD_Ivaddpd => "vaddpd",
            ud_mnemonic_code::UD_Ivaddps => "vaddps",
            ud_mnemonic_code::UD_Ivaddsd => "vaddsd",
            ud_mnemonic_code::UD_Ivaddss => "vaddss",
            ud_mnemonic_code::UD_Ivaddsubpd => "vaddsubpd",
            ud_mnemonic_code::UD_Ivaddsubps => "vaddsubps",
            ud_mnemonic_code::UD_Ivaesdec => "vaesdec",
            ud_mnemonic_code::UD_Ivaesdeclast => "vaesdeclast",
            ud_mnemonic_code::UD_Ivaesenc => "vaesenc",
            ud_mnemonic_code::UD_Ivaesenclast => "vaesenclast",
            ud_mnemonic_code::UD_Ivaesimc => "vaesimc",
            ud_mnemonic_code::UD_Ivaeskeygenassist => "vaeskeygenassist",
            ud_mnemonic_code::UD_Ivandnpd => "vandnpd",
            ud_mnemonic_code::UD_Ivandnps => "vandnps",
            ud_mnemonic_code::UD_Ivandpd => "vandpd",
            ud_mnemonic_code::UD_Ivandps => "vandps",
            ud_mnemonic_code::UD_Ivblendpd => "vblendpd",
            ud_mnemonic_code::UD_Ivblendps => "vblendps",
            ud_mnemonic_code::UD_Ivblendvpd => "vblendvpd",
            ud_mnemonic_code::UD_Ivblendvps => "vblendvps",
            ud_mnemonic_code::UD_Ivbroadcastsd => "vbroadcastsd",
            ud_mnemonic_code::UD_Ivbroadcastss => "vbroadcastss",
            ud_mnemonic_code::UD_Ivcmppd => "vcmppd",
            ud_mnemonic_code::UD_Ivcmpps => "vcmpps",
            ud_mnemonic_code::UD_Ivcmpsd => "vcmpsd",
            ud_mnemonic_code::UD_Ivcmpss => "vcmpss",
            ud_mnemonic_code::UD_Ivcomisd => "vcomisd",
            ud_mnemonic_code::UD_Ivcomiss => "vcomiss",
            ud_mnemonic_code::UD_Ivcvtdq2pd => "vcvtdq2pd",
            ud_mnemonic_code::UD_Ivcvtdq2ps => "vcvtdq2ps",
            ud_mnemonic_code::UD_Ivcvtpd2dq => "vcvtpd2dq",
            ud_mnemonic_code::UD_Ivcvtpd2ps => "vcvtpd2ps",
            ud_mnemonic_code::UD_Ivcvtps2dq => "vcvtps2dq",
            ud_mnemonic_code::UD_Ivcvtps2pd => "vcvtps2pd",
            ud_mnemonic_code::UD_Ivcvtsd2si => "vcvtsd2si",
            ud_mnemonic_code::UD_Ivcvtsd2ss => "vcvtsd2ss",
            ud_mnemonic_code::UD_Ivcvtsi2sd => "vcvtsi2sd",
            ud_mnemonic_code::UD_Ivcvtsi2ss => "vcvtsi2ss",
            ud_mnemonic_code::UD_Ivcvtss2sd => "vcvtss2sd",
            ud_mnemonic_code::UD_Ivcvtss2si => "vcvtss2si",
            ud_mnemonic_code::UD_Ivcvttpd2dq => "vcvttpd2dq",
            ud_mnemonic_code::UD_Ivcvttps2dq => "vcvttps2dq",
            ud_mnemonic_code::UD_Ivcvttsd2si => "vcvttsd2si",
            ud_mnemonic_code::UD_Ivcvttss2si => "vcvttss2si",
            ud_mnemonic_code::UD_Ivdivpd => "vdivpd",
            ud_mnemonic_code::UD_Ivdivps => "vdivps",
            ud_mnemonic_code::UD_Ivdivsd => "vdivsd",
            ud_mnemonic_code::UD_Ivdivss => "vdivss",
            ud_mnemonic_code::UD_Ivdppd => "vdppd",
            ud_mnemonic_code::UD_Ivdpps => "vdpps",
            ud_mnemonic_code::UD_Iverr => "verr",
            ud_mnemonic_code::UD_Iverw => "verw",
            ud_mnemonic_code::UD_Ivextractf128 => "vextractf128",
            ud_mnemonic_code::UD_Ivextractps => "vextractps",
            ud_mnemonic_code::UD_Ivhaddpd => "vhaddpd",
            ud_mnemonic_code::UD_Ivhaddps => "vhaddps",
            ud_mnemonic_code::UD_Ivhsubpd => "vhsubpd",
            ud_mnemonic_code::UD_Ivhsubps => "vhsubps",
            ud_mnemonic_code::UD_Ivinsertf128 => "vinsertf128",
            ud_mnemonic_code::UD_Ivinsertps => "vinsertps",
            ud_mnemonic_code::UD_Ivlddqu => "vlddqu",
            ud_mnemonic_code::UD_Ivmaskmovdqu => "vmaskmovdqu",
            ud_mnemonic_code::UD_Ivmaskmovpd => "vmaskmovpd",
            ud_mnemonic_code::UD_Ivmaskmovps => "vmaskmovps",
            ud_mnemonic_code::UD_Ivmaxpd => "vmaxpd",
            ud_mnemonic_code::UD_Ivmaxps => "vmaxps",
            ud_mnemonic_code::UD_Ivmaxsd => "vmaxsd",
            ud_mnemonic_code::UD_Ivmaxss => "vmaxss",
            ud_mnemonic_code::UD_Ivmcall => "vmcall",
            ud_mnemonic_code::UD_Ivmclear => "vmclear",
            ud_mnemonic_code::UD_Ivminpd => "vminpd",
            ud_mnemonic_code::UD_Ivminps => "vminps",
            ud_mnemonic_code::UD_Ivminsd => "vminsd",
            ud_mnemonic_code::UD_Ivminss => "vminss",
            ud_mnemonic_code::UD_Ivmlaunch => "vmlaunch",
            ud_mnemonic_code::UD_Ivmload => "vmload",
            ud_mnemonic_code::UD_Ivmmcall => "vmmcall",
            ud_mnemonic_code::UD_Ivmovapd => "vmovapd",
            ud_mnemonic_code::UD_Ivmovaps => "vmovaps",
            ud_mnemonic_code::UD_Ivmovd => "vmovd",
            ud_mnemonic_code::UD_Ivmovddup => "vmovddup",
            ud_mnemonic_code::UD_Ivmovdqa => "vmovdqa",
            ud_mnemonic_code::UD_Ivmovdqu => "vmovdqu",
            ud_mnemonic_code::UD_Ivmovhlps => "vmovhlps",
            ud_mnemonic_code::UD_Ivmovhpd => "vmovhpd",
            ud_mnemonic_code::UD_Ivmovhps => "vmovhps",
            ud_mnemonic_code::UD_Ivmovlhps => "vmovlhps",
            ud_mnemonic_code::UD_Ivmovlpd => "vmovlpd",
            ud_mnemonic_code::UD_Ivmovlps => "vmovlps",
            ud_mnemonic_code::UD_Ivmovmskpd => "vmovmskpd",
            ud_mnemonic_code::UD_Ivmovmskps => "vmovmskps",
            ud_mnemonic_code::UD_Ivmovntdq => "vmovntdq",
            ud_mnemonic_code::UD_Ivmovntdqa => "vmovntdqa",
            ud_mnemonic_code::UD_Ivmovntpd => "vmovntpd",
            ud_mnemonic_code::UD_Ivmovntps => "vmovntps",
            ud_mnemonic_code::UD_Ivmovq => "vmovq",
            ud_mnemonic_code::UD_Ivmovsd => "vmovsd",
            ud_mnemonic_code::UD_Ivmovshdup => "vmovshdup",
            ud_mnemonic_code::UD_Ivmovsldup => "vmovsldup",
            ud_mnemonic_code::UD_Ivmovss => "vmovss",
            ud_mnemonic_code::UD_Ivmovupd => "vmovupd",
            ud_mnemonic_code::UD_Ivmovups => "vmovups",
            ud_mnemonic_code::UD_Ivmpsadbw => "vmpsadbw",
            ud_mnemonic_code::UD_Ivmptrld => "vmptrld",
            ud_mnemonic_code::UD_Ivmptrst => "vmptrst",
            ud_mnemonic_code::UD_Ivmread => "vmread",
            ud_mnemonic_code::UD_Ivmresume => "vmresume",
            ud_mnemonic_code::UD_Ivmrun => "vmrun",
            ud_mnemonic_code::UD_Ivmsave => "vmsave",
            ud_mnemonic_code::UD_Ivmulpd => "vmulpd",
            ud_mnemonic_code::UD_Ivmulps => "vmulps",
            ud_mnemonic_code::UD_Ivmulsd => "vmulsd",
            ud_mnemonic_code::UD_Ivmulss => "vmulss",
            ud_mnemonic_code::UD_Ivmwrite => "vmwrite",
            ud_mnemonic_code::UD_Ivmxoff => "vmxoff",
            ud_mnemonic_code::UD_Ivmxon => "vmxon",
            ud_mnemonic_code::UD_Ivorpd => "vorpd",
            ud_mnemonic_code::UD_Ivorps => "vorps",
            ud_mnemonic_code::UD_Ivpabsb => "vpabsb",
            ud_mnemonic_code::UD_Ivpabsd => "vpabsd",
            ud_mnemonic_code::UD_Ivpabsw => "vpabsw",
            ud_mnemonic_code::UD_Ivpackssdw => "vpackssdw",
            ud_mnemonic_code::UD_Ivpacksswb => "vpacksswb",
            ud_mnemonic_code::UD_Ivpackusdw => "vpackusdw",
            ud_mnemonic_code::UD_Ivpackuswb => "vpackuswb",
            ud_mnemonic_code::UD_Ivpaddb => "vpaddb",
            ud_mnemonic_code::UD_Ivpaddd => "vpaddd",
            ud_mnemonic_code::UD_Ivpaddq => "vpaddq",
            ud_mnemonic_code::UD_Ivpaddsb => "vpaddsb",
            ud_mnemonic_code::UD_Ivpaddsw => "vpaddsw",
            ud_mnemonic_code::UD_Ivpaddusb => "vpaddusb",
            ud_mnemonic_code::UD_Ivpaddusw => "vpaddusw",
            ud_mnemonic_code::UD_Ivpaddw => "vpaddw",
            ud_mnemonic_code::UD_Ivpalignr => "vpalignr",
            ud_mnemonic_code::UD_Ivpand => "vpand",
            ud_mnemonic_code::UD_Ivpandn => "vpandn",
            ud_mnemonic_code::UD_Ivpavgb => "vpavgb",
            ud_mnemonic_code::UD_Ivpavgw => "vpavgw",
            ud_mnemonic_code::UD_Ivpblendvb => "vpblendvb",
            ud_mnemonic_code::UD_Ivpblendw => "vpblendw",
            ud_mnemonic_code::UD_Ivpclmulqdq => "vpclmulqdq",
            ud_mnemonic_code::UD_Ivpcmpeqb => "vpcmpeqb",
            ud_mnemonic_code::UD_Ivpcmpeqd => "vpcmpeqd",
            ud_mnemonic_code::UD_Ivpcmpeqq => "vpcmpeqq",
            ud_mnemonic_code::UD_Ivpcmpeqw => "vpcmpeqw",
            ud_mnemonic_code::UD_Ivpcmpestri => "vpcmpestri",
            ud_mnemonic_code::UD_Ivpcmpestrm => "vpcmpestrm",
            ud_mnemonic_code::UD_Ivpcmpgtb => "vpcmpgtb",
            ud_mnemonic_code::UD_Ivpcmpgtd => "vpcmpgtd",
            ud_mnemonic_code::UD_Ivpcmpgtq => "vpcmpgtq",
            ud_mnemonic_code::UD_Ivpcmpgtw => "vpcmpgtw",
            ud_mnemonic_code::UD_Ivpcmpistri => "vpcmpistri",
            ud_mnemonic_code::UD_Ivpcmpistrm => "vpcmpistrm",
            ud_mnemonic_code::UD_Ivperm2f128 => "vperm2f128",
            ud_mnemonic_code::UD_Ivpermilpd => "vpermilpd",
            ud_mnemonic_code::UD_Ivpermilps => "vpermilps",
            ud_mnemonic_code::UD_Ivpextrb => "vpextrb",
            ud_mnemonic_code::UD_Ivpextrd => "vpextrd",
            ud_mnemonic_code::UD_Ivpextrq => "vpextrq",
            ud_mnemonic_code::UD_Ivpextrw => "vpextrw",
            ud_mnemonic_code::UD_Ivphaddd => "vphaddd",
            ud_mnemonic_code::UD_Ivphaddsw => "vphaddsw",
            ud_mnemonic_code::UD_Ivphaddw => "vphaddw",
            ud_mnemonic_code::UD_Ivphminposuw => "vphminposuw",
            ud_mnemonic_code::UD_Ivphsubd => "vphsubd",
            ud_mnemonic_code::UD_Ivphsubsw => "vphsubsw",
            ud_mnemonic_code::UD_Ivphsubw => "vphsubw",
            ud_mnemonic_code::UD_Ivpinsrb => "vpinsrb",
            ud_mnemonic_code::UD_Ivpinsrd => "vpinsrd",
            ud_mnemonic_code::UD_Ivpinsrq => "vpinsrq",
            ud_mnemonic_code::UD_Ivpinsrw => "vpinsrw",
            ud_mnemonic_code::UD_Ivpmaddubsw => "vpmaddubsw",
            ud_mnemonic_code::UD_Ivpmaddwd => "vpmaddwd",
            ud_mnemonic_code::UD_Ivpmaxsb => "vpmaxsb",
            ud_mnemonic_code::UD_Ivpmaxsd => "vpmaxsd",
            ud_mnemonic_code::UD_Ivpmaxsw => "vpmaxsw",
            ud_mnemonic_code::UD_Ivpmaxub => "vpmaxub",
            ud_mnemonic_code::UD_Ivpmaxud => "vpmaxud",
            ud_mnemonic_code::UD_Ivpmaxuw => "vpmaxuw",
            ud_mnemonic_code::UD_Ivpminsb => "vpminsb",
            ud_mnemonic_code::UD_Ivpminsd => "vpminsd",
            ud_mnemonic_code::UD_Ivpminsw => "vpminsw",
            ud_mnemonic_code::UD_Ivpminub => "vpminub",
            ud_mnemonic_code::UD_Ivpminud => "vpminud",
            ud_mnemonic_code::UD_Ivpminuw => "vpminuw",
            ud_mnemonic_code::UD_Ivpmovmskb => "vpmovmskb",
            ud_mnemonic_code::UD_Ivpmovsxbd => "vpmovsxbd",
            ud_mnemonic_code::UD_Ivpmovsxbq => "vpmovsxbq",
            ud_mnemonic_code::UD_Ivpmovsxbw => "vpmovsxbw",
            ud_mnemonic_code::UD_Ivpmovsxwd => "vpmovsxwd",
            ud_mnemonic_code::UD_Ivpmovsxwq => "vpmovsxwq",
            ud_mnemonic_code::UD_Ivpmovzxbd => "vpmovzxbd",
            ud_mnemonic_code::UD_Ivpmovzxbq => "vpmovzxbq",
            ud_mnemonic_code::UD_Ivpmovzxbw => "vpmovzxbw",
            ud_mnemonic_code::UD_Ivpmovzxdq => "vpmovzxdq",
            ud_mnemonic_code::UD_Ivpmovzxwd => "vpmovzxwd",
            ud_mnemonic_code::UD_Ivpmovzxwq => "vpmovzxwq",
            ud_mnemonic_code::UD_Ivpmuldq => "vpmuldq",
            ud_mnemonic_code::UD_Ivpmulhrsw => "vpmulhrsw",
            ud_mnemonic_code::UD_Ivpmulhuw => "vpmulhuw",
            ud_mnemonic_code::UD_Ivpmulhw => "vpmulhw",
            ud_mnemonic_code::UD_Ivpmulld => "vpmulld",
            ud_mnemonic_code::UD_Ivpmullw => "vpmullw",
            ud_mnemonic_code::UD_Ivpor => "vpor",
            ud_mnemonic_code::UD_Ivpsadbw => "vpsadbw",
            ud_mnemonic_code::UD_Ivpshufb => "vpshufb",
            ud_mnemonic_code::UD_Ivpshufd => "vpshufd",
            ud_mnemonic_code::UD_Ivpshufhw => "vpshufhw",
            ud_mnemonic_code::UD_Ivpshuflw => "vpshuflw",
            ud_mnemonic_code::UD_Ivpsignb => "vpsignb",
            ud_mnemonic_code::UD_Ivpsignd => "vpsignd",
            ud_mnemonic_code::UD_Ivpsignw => "vpsignw",
            ud_mnemonic_code::UD_Ivpslld => "vpslld",
            ud_mnemonic_code::UD_Ivpslldq => "vpslldq",
            ud_mnemonic_code::UD_Ivpsllq => "vpsllq",
            ud_mnemonic_code::UD_Ivpsllw => "vpsllw",
            ud_mnemonic_code::UD_Ivpsrad => "vpsrad",
            ud_mnemonic_code::UD_Ivpsraw => "vpsraw",
            ud_mnemonic_code::UD_Ivpsrld => "vpsrld",
            ud_mnemonic_code::UD_Ivpsrldq => "vpsrldq",
            ud_mnemonic_code::UD_Ivpsrlq => "vpsrlq",
            ud_mnemonic_code::UD_Ivpsrlw => "vpsrlw",
            ud_mnemonic_code::UD_Ivpsubb => "vpsubb",
            ud_mnemonic_code::UD_Ivpsubd => "vpsubd",
            ud_mnemonic_code::UD_Ivpsubq => "vpsubq",
            ud_mnemonic_code::UD_Ivpsubsb => "vpsubsb",
            ud_mnemonic_code::UD_Ivpsubsw => "vpsubsw",
            ud_mnemonic_code::UD_Ivpsubusb => "vpsubusb",
            ud_mnemonic_code::UD_Ivpsubusw => "vpsubusw",
            ud_mnemonic_code::UD_Ivpsubw => "vpsubw",
            ud_mnemonic_code::UD_Ivptest => "vptest",
            ud_mnemonic_code::UD_Ivpunpckhbw => "vpunpckhbw",
            ud_mnemonic_code::UD_Ivpunpckhdq => "vpunpckhdq",
            ud_mnemonic_code::UD_Ivpunpckhqdq => "vpunpckhqdq",
            ud_mnemonic_code::UD_Ivpunpckhwd => "vpunpckhwd",
            ud_mnemonic_code::UD_Ivpunpcklbw => "vpunpcklbw",
            ud_mnemonic_code::UD_Ivpunpckldq => "vpunpckldq",
            ud_mnemonic_code::UD_Ivpunpcklqdq => "vpunpcklqdq",
            ud_mnemonic_code::UD_Ivpunpcklwd => "vpunpcklwd",
            ud_mnemonic_code::UD_Ivpxor => "vpxor",
            ud_mnemonic_code::UD_Ivrcpps => "vrcpps",
            ud_mnemonic_code::UD_Ivrcpss => "vrcpss",
            ud_mnemonic_code::UD_Ivroundpd => "vroundpd",
            ud_mnemonic_code::UD_Ivroundps => "vroundps",
            ud_mnemonic_code::UD_Ivroundsd => "vroundsd",
            ud_mnemonic_code::UD_Ivroundss => "vroundss",
            ud_mnemonic_code::UD_Ivrsqrtps => "vrsqrtps",
            ud_mnemonic_code::UD_Ivrsqrtss => "vrsqrtss",
            ud_mnemonic_code::UD_Ivshufpd => "vshufpd",
            ud_mnemonic_code::UD_Ivshufps => "vshufps",
            ud_mnemonic_code::UD_Ivsqrtpd => "vsqrtpd",
            ud_mnemonic_code::UD_Ivsqrtps => "vsqrtps",
            ud_mnemonic_code::UD_Ivsqrtsd => "vsqrtsd",
            ud_mnemonic_code::UD_Ivsqrtss => "vsqrtss",
            ud_mnemonic_code::UD_Ivstmxcsr => "vstmxcsr",
            ud_mnemonic_code::UD_Ivsubpd => "vsubpd",
            ud_mnemonic_code::UD_Ivsubps => "vsubps",
            ud_mnemonic_code::UD_Ivsubsd => "vsubsd",
            ud_mnemonic_code::UD_Ivsubss => "vsubss",
            ud_mnemonic_code::UD_Ivtestpd => "vtestpd",
            ud_mnemonic_code::UD_Ivtestps => "vtestps",
            ud_mnemonic_code::UD_Ivucomisd => "vucomisd",
            ud_mnemonic_code::UD_Ivucomiss => "vucomiss",
            ud_mnemonic_code::UD_Ivunpckhpd => "vunpckhpd",
            ud_mnemonic_code::UD_Ivunpckhps => "vunpckhps",
            ud_mnemonic_code::UD_Ivunpcklpd => "vunpcklpd",
            ud_mnemonic_code::UD_Ivunpcklps => "vunpcklps",
            ud_mnemonic_code::UD_Ivxorpd => "vxorpd",
            ud_mnemonic_code::UD_Ivxorps => "vxorps",
            ud_mnemonic_code::UD_Ivzeroall => "vzeroall",
            ud_mnemonic_code::UD_Ivzeroupper => "vzeroupper",
            ud_mnemonic_code::UD_Iwait => "wait",
            ud_mnemonic_code::UD_Iwbinvd => "wbinvd",
            ud_mnemonic_code::UD_Iwrmsr => "wrmsr",
            ud_mnemonic_code::UD_Ixadd => "xadd",
            ud_mnemonic_code::UD_Ixchg => "xchg",
            ud_mnemonic_code::UD_Ixcryptcbc => "xcryptcbc",
            ud_mnemonic_code::UD_Ixcryptcfb => "xcryptcfb",
            ud_mnemonic_code::UD_Ixcryptctr => "xcryptctr",
            ud_mnemonic_code::UD_Ixcryptecb => "xcryptecb",
            ud_mnemonic_code::UD_Ixcryptofb => "xcryptofb",
            ud_mnemonic_code::UD_Ixgetbv => "xgetbv",
            ud_mnemonic_code::UD_Ixlatb => "xlatb",
            ud_mnemonic_code::UD_Ixor => "xor",
            ud_mnemonic_code::UD_Ixorpd => "xorpd",
            ud_mnemonic_code::UD_Ixorps => "xorps",
            ud_mnemonic_code::UD_Ixrstor => "xrstor",
            ud_mnemonic_code::UD_Ixsave => "xsave",
            ud_mnemonic_code::UD_Ixsetbv => "xsetbv",
            ud_mnemonic_code::UD_Ixsha1 => "xsha1",
            ud_mnemonic_code::UD_Ixsha256 => "xsha256",
            ud_mnemonic_code::UD_Ixstore => "xstore",
            ud_mnemonic_code::UD_Iinvalid => "invalid",
            ud_mnemonic_code::UD_I3dnow => "3dnow",
            ud_mnemonic_code::UD_Inone => "none",
            ud_mnemonic_code::UD_Idb => "db",
            ud_mnemonic_code::UD_Ipause => "pause",
            _ => panic!("Invalid mnemonic code")
        })
    }
}
