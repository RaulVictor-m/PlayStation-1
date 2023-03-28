const INSTRUCTIONS = @import("CPU_INSTRUCTIONS.zig");
const Bus = @import("Bus.zig").Bus;

const std = @import("std");
const print = std.debug.print;

inline fn pInstruct(opcode: u32) u32 {
    return (opcode & 0b1111_1100_0000_0000_0000_0000_0000_0000) >> 26;
}
inline fn rs(opcode: u32) u32 {
    return (opcode & 0b0000_0011_1110_0000_0000_0000_0000_0000) >> 21;
}
inline fn rt(opcode: u32) u32 {
    return (opcode & 0b0000_0000_0001_1111_0000_0000_0000_0000) >> 16;
}
inline fn rd(opcode: u32) u32 {
    return (opcode & 0b0000_0000_0000_0000_1111_1000_0000_0000) >> 11;
}
inline fn imms(opcode: u32) u32 {
    return (opcode & 0b0000_0000_0000_0000_0000_0111_1100_0000) >> 6;
}
inline fn sInstruct(opcode: u32) u32 {
    return (opcode & 0b0000_0000_0000_0000_0000_0000_0011_1111);
}

inline fn imm(opcode: u32) u32 {
    return (opcode & 0xffff);
}
inline fn signed_imm(opcode: u32) u32 {
    const signOP = @bitCast(i32, (opcode & 0xffff));
    const signOP16 = @truncate(i16, signOP);
    const signOP32 = @intCast(i32, signOP16);
    return @bitCast(u32, signOP32);
}

inline fn imm_jump26(opcode: u32) u32 {
    return (opcode & 0b0000_0011_1111_1111_1111_1111_1111_1111);
}

//TODOO: ALL --LOAD-- FUNCTIONS NEED FIXING (NOT IMPLEMENTED PROPERLLY, DUE TO "SLOT DELAY")

//TODOO: FIXING COPROCESSOR FUNCTIONS

//TODOO: CACHE AND CACHE CONTROLL

//TODOO: INTERRUPTS
//TODOO: EXEPTIONS

const CPU = struct {
    bus: *Bus,

    // registers
    PC: u32,
    HI: u32,
    LO: u32,

    REGS: [32]u32,

    //COPROCESSOR REGISTERS
    REGS_cop0: [32]u32,
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_12 -- SR  interrupsts flag - 22 bit
    //cop_13 -- Cause
    //cop_14 -- EPC holds PC when exception occurs
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_
    //cop_

    CurrentOpcode: u32,
    NextOpcode: u32,

    fn Init() CPU {
        const BIOS_START = 0x1fc00000;
        var self: CPU = CPU{
            .PC = BIOS_START,
            .HI = 0,
            .LO = 0,
            .REGS = u32{0} ** 32,
            .REGS_cop0 = u32{0} ** 32,
            .CurrentOpcode = 0,
            .NextOpcode = 0,
        };
        self.Next();
    }
    fn ConnectBus(self: *CPU, bus: *Bus) void {
        self.bus = bus;
    }

    fn Clock(self: *CPU) void {
        self.Next();
        INSTRUCTIONS.LIST[self.CurrentOpcode](self);
    }

    inline fn Next(self: *CPU) void {
        self.CurrentOpcode = self.NextOpcode;
        self.NextOpcode = self.Read32(self.PC);
        self.PC += 4;
    }

    inline fn Read32(self: *CPU, addr: u32) u32 {
        return self.bus.Read_DWORD(addr);
    }
    inline fn Read16(self: *CPU, addr: u32) u16 {
        return self.bus.Read_WORD(addr);
    }
    inline fn Read8(self: *CPU, addr: u32) u8 {
        return self.bus.Read_BYTE(addr);
    }

    inline fn Write32(self: *CPU, addr: u32, value: u32) void {
        self.bus.Write_DWORD(addr, value);
    }
    inline fn Write16(self: *CPU, addr: u32, value: u16) void {
        self.bus.Write_WORD(addr, value);
    }
    inline fn Write8(self: *CPU, addr: u32, value: u8) void {
        self.bus.Write_BYTE(addr, value);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////
    // EXCEPTIONS STAGE
    /////////////////////////////////////////////////////////////////////////////////////////////
    fn exception(self: *CPU) void {
        // stores pc in the cooprocessor EPC
        self.REGS_cop0[13] = self.PC; //self.cop_EPC = self.PC;

        // jumps to the exception handler address
        self.PC = switch (self.REGS_cop0[12] & (1 << 22)) {
            0 => 0xbfc00180,
            else => 0x80000080,
        };

        // any function that excepts should end up here
    }
    /////////////////////////////////////////////////////////////////////////////////////////////
    // OPCODES STAGE
    /////////////////////////////////////////////////////////////////////////////////////////////

    //SENDS TO SECONDARY INSTRUCTIONS
    fn SPECIAL(self: *CPU) void {
        const sub_opcode = sInstruct(self.CurrentOpcode);
        INSTRUCTIONS.SUB_LIST[sub_opcode](self);
    }

    //INVALID
    fn XXX(self: *CPU) void {
        print("{s}", self.CurrentOpcode);
        return;
    }

    ///////////////////////////////////////
    // PRIMARY OPCODES
    ///////////////////////////////////////

    //"ADD rt, rs + imm" exception
    fn ADDI(self: *CPU) void {
        self.REGS[rt(self.CurrentOpcode)] = self.REGS[rs(self.CurrentOpcode)] + signed_imm(self.CurrentOpcode);

        //TODOO: CHECK OVERFLOW AND UNDERFLOW AND THROW A PLAYSTATION EXCEPTION IF NEEDED

    }

    //"ADD rt, rs + imm"
    fn ADDIU(self: *CPU) void {
        self.REGS[rt(self.CurrentOpcode)] = self.REGS[rs(self.CurrentOpcode)] +% signed_imm(self.CurrentOpcode);
    }

    //"AND rt, rs & imm"
    fn ANDI(self: *CPU) void {
        self.REGS[rt(self.CurrentOpcode)] = self.REGS[rs(self.CurrentOpcode)] & imm(self.CurrentOpcode);
    }

    //Branch if rt == rs
    fn BEQ(self: *CPU) void {
        if (self.REGS[rt(self.CurrentOpcode)] == self.REGS[rs(self.CurrentOpcode)]) {
            self.PC += (signed_imm(self.CurrentOpcode) << 2) - 4; // the -4 is used because my PC register is in position os NextOpcode not Current
            return;
        }
    }

    //Branch if rs > 0
    fn BGTZ(self: *CPU) void {
        if (0 < @bitCast(i32, self.REGS[rs(self.CurrentOpcode)])) {
            self.PC += (signed_imm(self.CurrentOpcode) << 2) - 4; // the -4 is used because my PC register is in position os NextOpcode not Current

        }
    }

    //Branch if rs(signed) <= 0
    fn BLEZ(self: *CPU) void {
        if (0 >= @bitCast(i32, self.REGS[rs(self.CurrentOpcode)])) {
            self.PC += (signed_imm(self.CurrentOpcode) << 2) - 4; // the -4 is used because my PC register is in position os NextOpcode not Current

        }
    }

    //Branch if rt != rs
    fn BNE(self: *CPU) void {
        if (self.REGS[rt(self.CurrentOpcode)] != self.REGS[rs(self.CurrentOpcode)]) {
            self.PC += (signed_imm(self.CurrentOpcode) << 2) - 4; // the -4 is used because my PC register is in position os NextOpcode not Current
        }
    }

    // //Branchs link
    // fn BcondZ(self: *CPU ) void
    // {
    //     const decoder = rt(self.CurrentOpcode);

    // }

    ///////////////////////////////////////
    // COOPROCESSOR HANDLING
    ///////////////////////////////////////

    fn Copx_Action(self: *CPU) void {
        switch (rs(self.CurrentOpcode)) {
            //MFCx    ----   mov rt, cop_rd
            0 => {
                self.REGS[rt(self.CurrentOpcode)] = self.REGS_cop0[rd(self.CurrentOpcode)];
            },
            0 => {},
            0 => {},
        }
    }
    //cop0_rd, rt
    fn COP0(self: *CPU) void {
        self.REGS_cop0[rd(self.CurrentOpcode)] = self.REGS[rt(self.CurrentOpcode)];
        //TODOO: FIX AND FINISH THE IMPLEMENTATION OF THE REGISTERS
    }

    // //
    // fn COP1(self: *CPU ) void
    // {

    // }

    // //
    // fn COP2(self: *CPU ) void
    // {

    // }

    // //
    // fn COP3(self: *CPU ) void
    // {

    // }

    ///////////////////////////////////////
    ///////////////////////////////////////
    ///////////////////////////////////////

    //"Jmp to imm26"
    fn J(self: *CPU) void {
        //EXECUTE NEXT INTRUCTION BEFORE JUMPING --- DONE DUE TO NEXT INSTRUCTION BEEN ALREADY LOADED
        self.PC = (self.PC & 0xf0000000) | ((imm_jump26(self.CurrentOpcode)) << 2);
    }

    //"CALL instruction :)"
    fn JAL(self: *CPU) void {
        self.REGS[31] = self.PC;
        self.J();
    }

    //LB-sign_extended rt, offset(rs) "exception"
    fn LB(self: *CPU) void {
        const value_i8 = @bitCast(i8, Read8(self.REGS[rs(self.CurrentOpcode)] + signed_imm(self.CurrentOpcode)));
        const value_i32 = @intCast(i32, value_i8);
        self.REGS[rt(self.CurrentOpcode)] = @bitCast(u32, value_i32);
    }

    //LB rt, offset(rs) "exception"
    fn LBU(self: *CPU) void {
        self.REGS[rt(self.CurrentOpcode)] = @intCast(u32, Read8(self.REGS[rs(self.CurrentOpcode)] + signed_imm(self.CurrentOpcode)));
    }

    //LH-sign_extended rt, offset(rs) "exception"
    fn LH(self: *CPU) void {
        const value_i16 = @bitCast(i16, Read16(self.REGS[rs(self.CurrentOpcode)] + signed_imm(self.CurrentOpcode)));
        const value_i32 = @intCast(i32, value_i16);
        self.REGS[rt(self.CurrentOpcode)] = @bitCast(u32, value_i32);
    }

    //LH rt, offset(rs) "exception"
    fn LHU(self: *CPU) void {
        self.REGS[rt(self.CurrentOpcode)] = @intCast(u32, Read16(self.REGS[rs(self.CurrentOpcode)] + signed_imm(self.CurrentOpcode)));
    }

    //“STORES HIGH16_rt, imm”
    fn LUI(self: *CPU) void {
        self.REGS[rt(self.CurrentOpcode)] = (imm(self.CurrentOpcode) << 16);
    }

    //LW-sign_extended rt, offset(rs) "exception"
    fn LW(self: *CPU) void {
        self.REGS[rt(self.CurrentOpcode)] = Read32(self.REGS[rs(self.CurrentOpcode)] + signed_imm(self.CurrentOpcode));

        //TODOO: implement something to make sure rt isn't used as source in the next instruction
    }

    // //
    // fn LWC0(self: *CPU ) void
    // {

    // }

    // //
    // fn LWC1(self: *CPU ) void
    // {

    // }

    // //
    // fn LWC2(self: *CPU ) void
    // {

    // }

    // //
    // fn LWC3(self: *CPU ) void
    // {

    // }

    //Load rt-most_sign-16, [offset(rs)]-16bit "exception"
    fn LWL(self: *CPU) void {
        const value = @intCast(u32, Read16(self.REGS[rs(self.CurrentOpcode)] + signed_imm(self.CurrentOpcode))) << 16;
        self.REGS[rt(self.CurrentOpcode)] |= value;
    }

    //Load rt-least_sign-16, [offset(rs)]-16bit "exception"
    fn LWR(self: *CPU) void {
        const value = @intCast(u32, Read16(self.REGS[rs(self.CurrentOpcode)] + signed_imm(self.CurrentOpcode)));
        self.REGS[rt(self.CurrentOpcode)] |= value;
    }

    //"ORI rt, rs | imm "
    fn ORI(self: *CPU) void {
        self.REGS[rt(self.CurrentOpcode)] = self.REGS[rs(self.CurrentOpcode)] | imm(self.CurrentOpcode);
    }

    //"STORES [signed_imm + base(rs)],  8bit rt "
    fn SB(self: *CPU) void {
        const addr = signed_imm(self.CurrentOpcode) +% self.REGS[rs(self.CurrentOpcode)];
        self.Write8(addr, @as(u8, self.REGS[rt(self.CurrentOpcode)]));
    }

    //"STORES [signed_imm + base(rs)],  16bit rt "
    fn SH(self: *CPU) void {
        const addr = signed_imm(self.CurrentOpcode) +% self.REGS[rs(self.CurrentOpcode)];
        self.Write16(addr, @as(u16, self.REGS[rt(self.CurrentOpcode)]));
    }

    //"SET rt, rs < IMM_signed"
    fn SLTI(self: *CPU) void {
        const bigger = @bitCast(i32, self.REGS[rs(self.CurrentOpcode)]) < @bitCast(i32, signed_imm(self.CurrentOpcode));
        self.REGS[rt(self.CurrentOpcode)] = bigger;
    }

    //"SET rt, rs < IMM_signed" to unsigned comparation
    fn SLTIU(self: *CPU) void {
        const bigger = self.REGS[rs(self.CurrentOpcode)] < signed_imm(self.CurrentOpcode);
        self.REGS[rt(self.CurrentOpcode)] = bigger;
    }

    //"STORE [rs+imm], rt"
    fn SW(self: *CPU) void {
        if (!(self.REGS_cop0[12] & 0b10000)) //WRITING TO CACHE
            self.Write32((self.REGS[rs(self.CurrentOpcode)] + signed_imm(self.CurrentOpcode)), self.REGS[rt(self.CurrentOpcode)]);
    }

    // //
    // fn SWC0(self: *CPU ) void
    // {

    // }

    // //
    // fn SWC1(self: *CPU ) void
    // {

    // }

    // //
    // fn SWC2(self: *CPU ) void
    // {

    // }

    // //
    // fn SWC3(self: *CPU ) void
    // {

    // }

    //"STORE [rs+imm], rt-most_sign16"
    fn SWL(self: *CPU) void {
        const value = self.REGS[rt(self.CurrentOpcode)] >> 16;
        self.Write16(self.REGS[rs(self.CurrentOpcode)] + signed_imm(self.CurrentOpcode), @truncate(u16, value));
    }

    //"STORE [rs+imm], rt-least_sign16"
    fn SWR(self: *CPU) void {
        const value = self.REGS[rt(self.CurrentOpcode)];
        self.Write16(self.REGS[rs(self.CurrentOpcode)] + signed_imm(self.CurrentOpcode), @truncate(u16, value));
    }

    // XOR rt, rs ^ IMM
    fn XORI(self: *CPU) void {
        self.REGS[rt(self.CurrentOpcode)] = self.REGS[rs(self.CurrentOpcode)] ^ imm(self.CurrentOpcode);
    }

    ///////////////////////////////////////
    // SECUNDARY OPCODES
    ///////////////////////////////////////

    //"ADD rd, rs(signed) + rt(signed)" exception signed overflow
    fn ADD(self: *CPU) void {
        const result = @bitCast(i32, self.REGS[rs(self.CurrentOpcode)]) + @bitCast(i32, self.REGS[rt(self.CurrentOpcode)]);
        self.REGS[rd(self.CurrentOpcode)] = @bitCast(u32, result);
    }

    //"ADD rd, rs + rt"
    fn ADDU(self: *CPU) void {
        self.REGS[rd(self.CurrentOpcode)] = self.REGS[rs(self.CurrentOpcode)] +% self.REGS[rt(self.CurrentOpcode)];
    }

    //"AND rd, rt & rs"
    fn AND(self: *CPU) void {
        self.REGS[rd(self.CurrentOpcode)] = self.REGS[rt(self.CurrentOpcode)] & self.REGS[rs(self.CurrentOpcode)];
    }

    // //
    // fn BREAK(self: *CPU ) void
    // {

    // }

    //DIV lo, rs-signed / rt-signed ------- hi = remainder
    fn DIV(self: *CPU) void {
        const numerator = @bitCast(i32, self.REGS[rs(self.CurrentOpcode)]);
        const denominator = @bitCast(i32, self.REGS[rt(self.CurrentOpcode)]);
        if (denominator == 0) {
            if (numerator < 0) {
                self.LO = 1;
            } else {
                self.LO = 0xff_ff_ff_ff;
            }
            self.HI = numerator;
        } else {
            self.HI = @bitCast(u32, numerator % denominator);
            self.LO = @bitCast(u32, numerator / denominator);
        }
    }

    //DIV lo, rs / rt ------- hi = remainder
    fn DIVU(self: *CPU) void {
        const numerator = self.REGS[rs(self.CurrentOpcode)];
        const denominator = self.REGS[rt(self.CurrentOpcode)];
        if (denominator == 0) {
            self.LO = 0xff_ff_ff_ff;
            self.HI = numerator;
        } else {
            self.HI = numerator % denominator;
            self.LO = numerator / denominator;
        }
    }

    //"JMP rs, and store RETURN in rd"
    fn JALR(self: *CPU) void {
        self.REGS[rd(self.CurrentOpcode)] = self.PC;
        self.PC = self.REGS[rs(self.CurrentOpcode)];
    }

    //"JMP rs :)"
    fn JR(self: *CPU) void {
        self.PC = self.REGS[rs(self.CurrentOpcode)];
    }

    //"LOAD rd, HI" //TODOO: CLOCK THE DIVISIONSs TO KNOW IF THIS RESULT IS READY
    fn MFHI(self: *CPU) void {
        self.REGS[rd(self.CurrentOpcode)] = self.HI;
    }

    //"LOAD rd, LO" //TODOO: CLOCK THE DIVISIONSs TO KNOW IF THIS RESULT IS READY
    fn MFLO(self: *CPU) void {
        self.REGS[rd(self.CurrentOpcode)] = self.LO;
    }

    //"Mov HI, rs"
    fn MTHI(self: *CPU) void {
        self.HI = self.REGS[rs(self.CurrentOpcode)];
    }

    //"Mov LO, rs"
    fn MTLO(self: *CPU) void {
        self.LO = self.REGS[rs(self.CurrentOpcode)];
    }

    //mult hi-lo 64bit, rt-signed * rs-signed
    fn MULT(self: *CPU) void {
        const rt_64 = @intCast(i64, @bitCast(i32, self.REGS[rt(self.CurrentOpcode)]));
        const rs_64 = @intCast(i64, @bitCast(i32, self.REGS[rs(self.CurrentOpcode)]));

        const result = @intCast(u64, (rt_64 * rs_64));

        self.HI = @truncate(u32, (result >> 32));
        self.LO = @truncate(u32, (result));
    }

    //mult hi-lo 64bit, rt * rs
    fn MULTU(self: *CPU) void {
        const rt_64 = @intCast(u64, self.REGS[rt(self.CurrentOpcode)]);
        const rs_64 = @intCast(u64, self.REGS[rs(self.CurrentOpcode)]);

        const result = rt_64 * rs_64;

        self.HI = @truncate(u32, (result >> 32));
        self.LO = @truncate(u32, (result));
    }

    // NOR rd, rt NOR rs
    fn NOR(self: *CPU) void {
        const result = !(self.REGS[rt(self.CurrentOpcode)] | self.REGS[rs[self.CurrentOpcode]]);
        self.REGS[rd(self.CurrentOpcode)] = result;
    }

    //"OR rd, rt | rs "
    fn OR(self: *CPU) void {
        self.REGS[rd(self.CurrentOpcode)] = self.REGS[rt(self.CurrentOpcode)] | self.REGS[rs(self.CurrentOpcode)];
    }

    //"SHL rd, rt << imms"
    fn SLL(self: *CPU) void {
        self.REGS[rd(self.CurrentOpcode)] = self.REGS[rt(self.CurrentOpcode)] << imms(self.CurrentOpcode);
    }

    // SHL rd, rt << rs-5_low_bits
    fn SLLV(self: *CPU) void {
        const shift = self.REGS[rs(self.CurrentOpcode)] & 0b11111;
        self.REGS[rd(self.CurrentOpcode)] = self.REGS[rt(self.CurrentOpcode)] << shift;
    }

    //"SET rd, rs(signed) < rt(signed)"
    fn SLT(self: *CPU) void {
        const bigger = @bitCast(i32, (self.REGS[rs(self.CurrentOpcode)])) < @bitCast(i32, self.REGS[rt(self.CurrentOpcode)]);

        self.REGS[rd(self.CurrentOpcode)] = bigger;
    }

    //"SET rd, rs < rt"
    fn SLTU(self: *CPU) void {
        const bigger = (self.REGS[rs(self.CurrentOpcode)] < self.REGS[rt(self.CurrentOpcode)]);

        self.REGS[rd(self.CurrentOpcode)] = bigger;
    }

    //"SHR rd, rt(signed) >> imms"
    fn SRA(self: *CPU) void {
        const result = @bitCast(i32, self.REGS[rt(self.CurrentOpcode)]) >> imms(self.CurrentOpcode);
        self.REGS[rd(self.CurrentOpcode)] = @bitCast(u32, result);
    }

    //SHR rd, rt(singed) >> rs-5_low_bits
    fn SRAV(self: *CPU) void {
        const shift = self.REGS[rs(self.CurrentOpcode)] & 0b11111;
        const result = @bitCast(i32, self.REGS[rt(self.CurrentOpcode)]) >> shift;
        self.REGS[rd(self.CurrentOpcode)] = @bitCast(u32, result);
    }

    //"SHR rd, rt >> imms"
    fn SRL(self: *CPU) void {
        const result = self.REGS[rt(self.CurrentOpcode)] >> imms(self.CurrentOpcode);
        self.REGS[rd(self.CurrentOpcode)] = result;
    }

    // SHR rd, rt >> rs-5_low_bits
    fn SRLV(self: *CPU) void {
        const shift = self.REGS[rs(self.CurrentOpcode)] & 0b11111;
        self.REGS[rd(self.CurrentOpcode)] = self.REGS[rt(self.CurrentOpcode)] >> shift;
    }

    //"SUB rd, rs - rt" exception
    fn SUB(self: *CPU) void {
        const result = @bitCast(i32, self.REGS[rs(self.CurrentOpcode)]) - @bitCast(i32, self.REGS[rt(self.CurrentOpcode)]);
        self.REGS[rd(self.CurrentOpcode)] = @bitCast(u32, result);
    }

    //"SUB rd, rs - rt"
    fn SUBU(self: *CPU) void {
        self.REGS[rd(self.CurrentOpcode)] = self.REGS[rs(self.CurrentOpcode)] -% self.REGS[rt(self.CurrentOpcode)];
    }

    // //
    // fn SYSCALL(self: *CPU ) void
    // {

    // }

    //XOR rd, rs ^ rt
    fn XOR(self: *CPU) void {
        self.REGS[rd(self.CurrentOpcode)] = self.REGS[rs(self.CurrentOpcode)] ^ self.REGS[rt(self.CurrentOpcode)];
    }
};
