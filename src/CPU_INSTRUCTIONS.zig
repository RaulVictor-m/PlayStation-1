const Cpu = @import("cpu.zig").Cpu;

pub const LIST = [_]@TypeOf(Cpu.XXX){
    Cpu.SPECIAL, Cpu.BcondZ, //0    1 
    Cpu.J,       Cpu.JAL,    //2    3 
    Cpu.BEQ,     Cpu.BNE,    //4    5 
    Cpu.BLEZ,    Cpu.BGTZ,   //6    7 
    Cpu.ADDI,    Cpu.ADDIU,  //8    9 
    Cpu.SLTI,    Cpu.SLTIU,  //10   11
    Cpu.ANDI,    Cpu.ORI,    //12   13
    Cpu.XORI,    Cpu.LUI,    //14   15
    Cpu.COP0,    Cpu.COP1,   //16   17
    Cpu.COP2,    Cpu.COP3,   //18   19
    Cpu.XXX,     Cpu.XXX,    //20   21
    Cpu.XXX,     Cpu.XXX,    //22   23
    Cpu.XXX,     Cpu.XXX,    //24   25
    Cpu.XXX,     Cpu.XXX,    //26   27
    Cpu.XXX,     Cpu.XXX,    //28   29
    Cpu.XXX,     Cpu.XXX,    //30   31
    Cpu.LB,      Cpu.LH,     //32   33
    Cpu.LWL,     Cpu.LW,     //34   35
    Cpu.LBU,     Cpu.LHU,    //36   37
    Cpu.LWR,     Cpu.XXX,    //38   39
    Cpu.SB,      Cpu.SH,     //40   41
    Cpu.SWL,     Cpu.SW,     //42   43
    Cpu.XXX,     Cpu.XXX,    //44   45
    Cpu.SWR,     Cpu.XXX,    //46   47
    Cpu.LWC0,    Cpu.LWC1,   //48   49
    Cpu.LWC2,    Cpu.LWC3,   //50   51
    Cpu.XXX,     Cpu.XXX,    //52   53
    Cpu.XXX,     Cpu.XXX,    //54   55
    Cpu.SWC0,    Cpu.SWC1,   //56   57
    Cpu.SWC2,    Cpu.SWC3,   //58   59
//  Cpu.XXX,     Cpu.XXX,    //60   61
//  Cpu.XXX,     Cpu.XXX,    //62   63
};
pub const SUB_LIST = [_]@TypeOf(Cpu.XXX){
    Cpu.SLL,     Cpu.XXX,    //0    1 
    Cpu.SRL,     Cpu.SRA,    //2    3 
    Cpu.SLLV,    Cpu.XXX,    //4    5 
    Cpu.SRLV,    Cpu.SRAV,   //6    7 
    Cpu.JR,      Cpu.JALR,   //8    9 
    Cpu.XXX,     Cpu.XXX,    //10   11
    Cpu.SYSCALL, Cpu.BREAK,  //12   13
    Cpu.XXX,     Cpu.XXX,    //14   15
    Cpu.MFHI,    Cpu.MTHI,   //16   17
    Cpu.MFLO,    Cpu.MTLO,   //18   19
    Cpu.XXX,     Cpu.XXX,    //20   21
    Cpu.XXX,     Cpu.XXX,    //22   23
    Cpu.MULT,    Cpu.MULTU,  //24   25
    Cpu.DIV,     Cpu.DIVU,   //26   27
    Cpu.XXX,     Cpu.XXX,    //28   29
    Cpu.XXX,     Cpu.XXX,    //30   31
    Cpu.ADD,     Cpu.ADDU,   //32   33
    Cpu.SUB,     Cpu.SUBU,   //34   35
    Cpu.AND,     Cpu.OR,     //36   37
    Cpu.XOR,     Cpu.NOR,    //38   39
    Cpu.XXX,     Cpu.XXX,    //40   41
    Cpu.SLT,     Cpu.SLTU,   //42   43
//  Cpu.XXX,     Cpu.XXX,    //44   45
//  Cpu.XXX,     Cpu.XXX,    //46   47
//  Cpu.XXX,     Cpu.XXX,    //48   49
//  Cpu.XXX,     Cpu.XXX,    //50   51
//  Cpu.XXX,     Cpu.XXX,    //52   53
//  Cpu.XXX,     Cpu.XXX,    //54   55
//  Cpu.XXX,     Cpu.XXX,    //56   57
//  Cpu.XXX,     Cpu.XXX,    //58   59
//  Cpu.XXX,     Cpu.XXX,    //60   61
//  Cpu.XXX,     Cpu.XXX,    //62   63
};
