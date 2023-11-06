const Cpu = @import("cpu.zig").Cpu;

pub const LIST = [_]@TypeOf(Cpu.XXX){
    Cpu.SPECIAL, Cpu.BcondZ,
    Cpu.J,       Cpu.JAL,
    Cpu.BEQ,     Cpu.BNE,
    Cpu.BLEZ,    Cpu.BGTZ,
    Cpu.ADDI,    Cpu.ADDIU,
    Cpu.SLTI,    Cpu.SLTIU,
    Cpu.ANDI,    Cpu.ORI,
    Cpu.XORI,    Cpu.LUI,
    Cpu.COP0,    Cpu.COP1,
    Cpu.COP2,    Cpu.COP3,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.LB,      Cpu.LH,
    Cpu.LWL,     Cpu.LW,
    Cpu.LBU,     Cpu.LHU,
    Cpu.LWR,     Cpu.XXX,
    Cpu.SB,      Cpu.SH,
    Cpu.SWL,     Cpu.SW,
    Cpu.XXX,     Cpu.XXX,
    Cpu.SWR,     Cpu.XXX,
    Cpu.LWC0,    Cpu.LWC1,
    Cpu.LWC2,    Cpu.LWC3,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.SWC0,    Cpu.SWC1,
    Cpu.SWC2,    Cpu.SWC3,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
};
pub const SUB_LIST = [_]@TypeOf(Cpu.XXX){
    Cpu.SLL,     Cpu.XXX,
    Cpu.SRL,     Cpu.SRA,
    Cpu.SLLV,    Cpu.XXX,
    Cpu.SRLV,    Cpu.SRAV,
    Cpu.JR,      Cpu.JALR,
    Cpu.XXX,     Cpu.XXX,
    Cpu.SYSCALL, Cpu.BREAK,
    Cpu.XXX,     Cpu.XXX,
    Cpu.MFHI,    Cpu.MTHI,
    Cpu.MFLO,    Cpu.MTLO,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.MULT,    Cpu.MULTU,
    Cpu.DIV,     Cpu.DIVU,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.ADD,     Cpu.ADDU,
    Cpu.SUB,     Cpu.SUBU,
    Cpu.AND,     Cpu.OR,
    Cpu.XOR,     Cpu.NOR,
    Cpu.XXX,     Cpu.XXX,
    Cpu.SLT,     Cpu.SLTU,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
    Cpu.XXX,     Cpu.XXX,
};
