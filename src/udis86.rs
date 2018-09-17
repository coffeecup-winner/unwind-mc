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
            // TODO: fix syntax generation bug in udis86 (vsnprintf calls)
            // ud_set_syntax(&mut ud, Some(ud_translate_intel));
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
                let asm_cstr = CStr::from_ptr(ud_insn_asm(&self.ud));

                result.push(Insn {
                    address: ud_insn_off(&self.ud),
                    code: ud_insn_mnemonic(&self.ud),
                    length: ud_insn_len(&self.ud) as u8,
                    hex: hex_cstr.to_str().unwrap().to_owned(),
                    assembly: asm_cstr.to_str().unwrap().to_owned(),
                    operands: Self::get_operands(&mut self.ud),
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
                });
                index += 1;
                op = ud_insn_opr(ud, index);
            }
        }
        result
    }
}
