const INSTRUCTIONS = @import("CPU_INSTRUCTIONS.zig");
const Bus = @import("Bus.zig").Bus;

const std = @import("std");
const print = std.debug.print;

test{
    std.testing.refAllDecls(@This());
}

inline fn maskMakeOp(op: u32, rs: u32, rt: u32, rd: u32, imm: u32) u32 {
    return (op << 26) | (rs << 21) | (rt << 16) | (rd << 11) | imm;
}
//inline fn makeSubOp(op: 32, rs: u32, rt: u32, rd: u32, imm: u32) u32 {
//    return 
//}
inline fn maskInstruction(opcode: u32) u32 {
    return (opcode & 0b1111_1100_0000_0000_0000_0000_0000_0000) >> 26;
}
inline fn maskRs(opcode: u32) u32 {
    return (opcode & 0b0000_0011_1110_0000_0000_0000_0000_0000) >> 21;
}
inline fn maskRt(opcode: u32) u32 {
    return (opcode & 0b0000_0000_0001_1111_0000_0000_0000_0000) >> 16;
}
inline fn maskRd(opcode: u32) u32 {
    return (opcode & 0b0000_0000_0000_0000_1111_1000_0000_0000) >> 11;
}
inline fn maskImms(opcode: u32) u32 {
    return (opcode & 0b0000_0000_0000_0000_0000_0111_1100_0000) >> 6;
}
inline fn maskSubInstruction(opcode: u32) u32 {
    return (opcode & 0b0000_0000_0000_0000_0000_0000_0011_1111);
}

inline fn maskImm(opcode: u32) u32 {
    return (opcode & 0xffff);
}
inline fn maskSignedImm(opcode: u32) u32 {
    const signOP = @as(i32, @bitCast((opcode & 0xffff)));
    const signOP16 = @as(i16, @truncate(signOP));
    const signOP32 = @as(i32, @intCast(signOP16));
    return @as(u32, @bitCast(signOP32));
}

inline fn maskImmJump26(opcode: u32) u32 {
    return (opcode & 0b0000_0011_1111_1111_1111_1111_1111_1111);
}

//TODOO: ALL --LOAD-- FUNCTIONS NEED FIXING (NOT IMPLEMENTED PROPERLLY, DUE TO "SLOT DELAY")
//TODOO: IMPLEMENT TESTS FOR ALL THE INSTRUCTIONS

//TODOO: FIXING COPROCESSOR FUNCTIONS

//TODOO: CACHE AND CACHE CONTROLL

//TODOO: INTERRUPTS
//TODOO: EXEPTIONS

pub const Cpu = struct {
    bus: *Bus,

    // registers
    pc: u32,
    hi: u32,
    lo: u32,

    regs: [32]u32,

    //COPROCESSOR REGISTERS
    regs_cop0: [32]u32,
    // cop0r0-r2   - N/A
    // cop0r3      - BPC - Breakpoint on execute (R/W)
    // cop0r4      - N/A
    // cop0r5      - BDA - Breakpoint on data access (R/W)
    // cop0r6      - JUMPDEST - Randomly memorized jump address (R)
    // cop0r7      - DCIC - Breakpoint control (R/W)
    // cop0r8      - BadVaddr - Bad Virtual Address (R)
    // cop0r9      - BDAM - Data Access breakpoint mask (R/W)
    // cop0r10     - N/A
    // cop0r11     - BPCM - Execute breakpoint mask (R/W)
    // cop0r12     - SR - System status register (R/W)                               //cop_12 -- SR  interrupsts flag - 22 bit
    // cop0r13     - CAUSE - (R)  Describes the most recently recognised exception   //cop_13 -- Cause
    // cop0r14     - EPC - Return Address from Trap (R)                              //cop_14 -- EPC holds PC when exception occurs
    // cop0r15     - PRID - Processor ID (R)
    // cop0r16-r31 - Garbage
    // cop0r32-r63 - N/A - None such (Control regs)

    current_op: u32,
    next_op: u32,

    pub fn init() Cpu {
        const BIOS_START = 0x1fc00000;
        return .{
            .bus = undefined,
            .pc = BIOS_START,
            .hi = 0,
            .lo = 0,
            .regs = [_]u32{0} ** 32,
            .regs_cop0 = [_]u32{0} ** 32,
            .current_op = 0,
            .next_op = 0,
        };
    }
    pub fn connectBus(self: *Cpu, bus: *Bus) void {
        self.bus = bus;
    }

    //Primary opcode stage
    pub fn clock(self: *Cpu) void {
        self.next();

        const p_opcode = maskInstruction(self.current_op);

        switch(p_opcode) {
            0...INSTRUCTIONS.LIST.len - 1 => |i| INSTRUCTIONS.LIST[i] (self),
            else => unreachable,
        }
    }

    pub inline fn next(self: *Cpu) void {
        self.current_op = self.next_op;
        self.next_op = self.read32(self.pc);
        self.pc += 4;
    }

    pub inline fn read32(self: *Cpu, addr: u32) u32 {
        return self.bus.read32(addr);
    }
    pub inline fn read16(self: *Cpu, addr: u32) u16 {
        return self.bus.read16(addr);
    }
    pub inline fn read8(self: *Cpu, addr: u32) u8 {
        return self.bus.read8(addr);
    }

    pub inline fn write32(self: *Cpu, addr: u32, value: u32) void {
        self.bus.write32(addr, value);
    }
    pub inline fn write16(self: *Cpu, addr: u32, value: u16) void {
        self.bus.write16(addr, value);
    }
    pub inline fn write8(self: *Cpu, addr: u32, value: u8) void {
        self.bus.write8(addr, value);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////
    // EXCEPTIONS STAGE
    /////////////////////////////////////////////////////////////////////////////////////////////
    pub fn exception(self: *Cpu) void {
        // stores pc in the cooprocessor EPC
        self.regs_cop0[13] = self.pc; //self.cop_EPC = self.pc;

        // jumps to the exception handler address
        self.pc = switch (self.regs_cop0[12] & (1 << 22)) {
            0 => 0xbfc00180,
            else => 0x80000080,
        };

        // any function that excepts should end up here
    }
    /////////////////////////////////////////////////////////////////////////////////////////////
    // OPCODES STAGE
    /////////////////////////////////////////////////////////////////////////////////////////////

    //SENDS TO SECONDARY INSTRUCTIONS
    pub inline fn SPECIAL(self: *Cpu) void {

        const sub_opcode = maskSubInstruction(self.current_op);

        switch(sub_opcode) {
            0...INSTRUCTIONS.SUB_LIST - 1  => INSTRUCTIONS.SUB_LIST[0] (self),
            else => unreachable,
        }
    }

    //INVALID
    pub inline fn XXX(self: *Cpu) void {
        _ = self;
        unreachable;
    }

    ///////////////////////////////////////
    // PRIMARY OPCODES
    ///////////////////////////////////////

    //"ADD rt, rs + imm" exception 
    pub inline fn ADDI(self: *Cpu) void {
        self.regs[maskRt(self.current_op)] = self.regs[maskRs(self.current_op)] + maskSignedImm(self.current_op);

        //TODOO: CHECK OVERFLOW AND UNDERFLOW AND THROW A PLAYSTATION EXCEPTION IF NEEDED

    }// TODO: TEST

    //"ADDIU: rt <- rs + immSigned" no exception
    pub inline fn ADDIU(self: *Cpu) void {
        const rt = maskRt(self.current_op);
        const rs = maskRs(self.current_op);
        const s_imm = maskSignedImm(self.current_op);

        self.regs[rt] = self.regs[rs] +% s_imm;
    }
    test "Testing func ADDIU"{
        //starting the cpu with the default
        var cpu: Cpu = Cpu.init();

        //constructing opcode
        cpu.current_op = maskMakeOp(0x9, 1, 0, 0, 100);


        //normal add
        cpu.regs[1] = 10; //rs
        cpu.ADDIU();
        std.debug.assert(cpu.regs[0] == 110); // rt == rs + immSigned

        //wrapping around add
        cpu.regs[1] = 0xffffffff; //rs
        cpu.ADDIU();
        std.debug.assert(cpu.regs[0] == 99); // rt == rs + immSigned

        //negative value on imm add
        cpu.regs[1] = 1000; //rs
        cpu.current_op |= 0xffff - 10; //immSigned = -11 
        cpu.ADDIU();
        std.debug.assert(cpu.regs[0] == 989); // rt == rs + immSigned

    }

    //"AND rt, rs & imm"
    pub inline fn ANDI(self: *Cpu) void {
        self.regs[maskRt(self.current_op)] = self.regs[maskRs(self.current_op)] & maskImm(self.current_op);
    }

    //Branch if rt == rs
    pub inline fn BEQ(self: *Cpu) void {
        if (self.regs[maskRt(self.current_op)] == self.regs[maskRs(self.current_op)]) {
            self.pc += (maskSignedImm(self.current_op) << 2) - 4; // the -4 is used because my PC register is in position os next_op not Current
            return;
        }
    }

    //Branch if rs > 0
    pub inline fn BGTZ(self: *Cpu) void {
        if (0 < @as(i32, @bitCast(self.regs[maskRs(self.current_op)]))) {
            self.pc += (maskSignedImm(self.current_op) << 2) - 4; // the -4 is used because my PC register is in position os next_op not Current

        }
    }

    //Branch if rs(signed) <= 0
    pub inline fn BLEZ(self: *Cpu) void {
        if (0 >= @as(i32, @bitCast(self.regs[maskRs(self.current_op)]))) {
            self.pc += (maskSignedImm(self.current_op) << 2) - 4; // the -4 is used because my PC register is in position os next_op not Current

        }
    }

    //Branch if rt != rs
    pub inline fn BNE(self: *Cpu) void {
        if (self.regs[maskRt(self.current_op)] != self.regs[maskRs(self.current_op)]) {
            self.pc += (maskSignedImm(self.current_op) << 2) - 4; // the -4 is used because my PC register is in position os next_op not Current
        }
    }

    //Branchs link
    pub inline fn BcondZ(self: *Cpu ) void {
        _ = self;
        @panic("BcondZ not implemented");
        //const decoder = rt(self.current_op);
    }

    ///////////////////////////////////////
    // COOPROCESSOR HANDLING
    ///////////////////////////////////////

    pub inline fn Copx_Action(self: *Cpu) void {
        switch (maskRs(self.current_op)) {
            //MFCx    ----   mov rt, cop_rd
            0 => {
                self.regs[maskRt(self.current_op)] = self.regs_cop0[maskRd(self.current_op)];
            },
            0 => {},
            0 => {},
        }
    }
    //cop0_rd, rt
    pub inline fn COP0(self: *Cpu) void {
        self.regs_cop0[maskRd(self.current_op)] = self.regs[maskRt(self.current_op)];
        //TODOO: FIX AND FINISH THE IMPLEMENTATION OF THE REGISTERS
    }

    //
    pub inline fn COP1(self: *Cpu ) void {
        _ = self;
        @panic("COP1 not implemented");

    }

    //
    pub inline fn COP2(self: *Cpu ) void {
        _ = self;
        @panic("COP2 not implemented");
    }

    //
    pub inline fn COP3(self: *Cpu ) void {
        _ = self;
        @panic("COP3 not implemented");
    }

    ///////////////////////////////////////
    ///////////////////////////////////////
    ///////////////////////////////////////

    //"Jmp to imm26"
    pub inline fn J(self: *Cpu) void {
        //EXECUTE NEXT INTRUCTION BEFORE JUMPING --- DONE DUE TO NEXT INSTRUCTION BEEN ALREADY LOADED
        self.pc = (self.pc & 0xf0000000) | ((maskImmJump26(self.current_op)) << 2);
    }

    //"CALL instruction :)"
    pub inline fn JAL(self: *Cpu) void {
        self.regs[31] = self.pc;
        self.J();
    }

    //LB-sign_extended rt, offset(rs) "exception"
    pub inline fn LB(self: *Cpu) void {
        const value_i8 = @as(i8, @bitCast(read8(self.regs[maskRs(self.current_op)] + maskSignedImm(self.current_op))));
        const value_i32 = @as(i32, @intCast(value_i8));
        self.regs[maskRt(self.current_op)] = @as(u32, @bitCast(value_i32));
    }

    //LB rt, offset(rs) "exception"
    pub inline fn LBU(self: *Cpu) void {
        self.regs[maskRt(self.current_op)] = @as(u32, @intCast(read8(self.regs[maskRs(self.current_op)] + maskSignedImm(self.current_op))));
    }

    //LH-sign_extended rt, offset(rs) "exception"
    pub inline fn LH(self: *Cpu) void {
        const value_i16 = @as(i16, @bitCast(read16(self.regs[maskRs(self.current_op)] + maskSignedImm(self.current_op))));
        const value_i32 = @as(i32, @intCast(value_i16));
        self.regs[maskRt(self.current_op)] = @as(u32, @bitCast(value_i32));
    }

    //LH rt, offset(rs) "exception"
    pub inline fn LHU(self: *Cpu) void {
        self.regs[maskRt(self.current_op)] = @as(u32, @intCast(read16(self.regs[maskRs(self.current_op)] + maskSignedImm(self.current_op))));
    }

    //“STORES HIGH16_rt, imm”
    pub inline fn LUI(self: *Cpu) void {
        self.regs[maskRt(self.current_op)] = (maskImm(self.current_op) << 16);
    }

    //LW-sign_extended rt, offset(rs) "exception"
    pub inline fn LW(self: *Cpu) void {
        self.regs[maskRt(self.current_op)] = read32(self.regs[maskRs(self.current_op)] + maskSignedImm(self.current_op));

        //TODOO: implement something to make sure rt isn't used as source in the next instruction
    }

    //
    pub inline fn LWC0(self: *Cpu ) void {
        _ = self;
        @panic("LWC0 not implemented");
    }

    //
    pub inline fn LWC1(self: *Cpu ) void {
        _ = self;
        @panic("LWC1 not implemented");
    }

    //
    pub inline fn LWC2(self: *Cpu ) void {
        _ = self;
        @panic("LWC2 not implemented");
    }

    //
    pub inline fn LWC3(self: *Cpu ) void {
        _ = self;
        @panic("LWC3 not implemented");
    }

    //Load rt-most_sign-16, [offset(rs)]-16bit "exception"
    pub inline fn LWL(self: *Cpu) void {
        const value = @as(u32, @intCast(read16(self.regs[maskRs(self.current_op)] + maskSignedImm(self.current_op)))) << 16;
        self.regs[maskRt(self.current_op)] |= value;
    }

    //Load rt-least_sign-16, [offset(rs)]-16bit "exception"
    pub inline fn LWR(self: *Cpu) void {
        const value = @as(u32, @intCast(read16(self.regs[maskRs(self.current_op)] + maskSignedImm(self.current_op))));
        self.regs[maskRt(self.current_op)] |= value;
    }

    //"ORI rt, rs | imm "
    pub inline fn ORI(self: *Cpu) void {
        self.regs[maskRt(self.current_op)] = self.regs[maskRs(self.current_op)] | maskImm(self.current_op);
    }

    //"STORES [signed_imm + base(rs)],  8bit rt "
    pub inline fn SB(self: *Cpu) void {
        const addr = maskSignedImm(self.current_op) +% self.regs[maskRs(self.current_op)];
        self.write8(addr, @as(u8, self.regs[maskRt(self.current_op)]));
    }

    //"STORES [signed_imm + base(rs)],  16bit rt "
    pub inline fn SH(self: *Cpu) void {
        const addr = maskSignedImm(self.current_op) +% self.regs[maskRs(self.current_op)];
        self.write16(addr, @as(u16, self.regs[maskRt(self.current_op)]));
    }

    //"SET rt, rs < IMM_signed"
    pub inline fn SLTI(self: *Cpu) void {
        const bigger = @as(i32, @bitCast(self.regs[maskRs(self.current_op)])) < @as(i32, @bitCast(maskSignedImm(self.current_op)));
        self.regs[maskRt(self.current_op)] = bigger;
    }

    //"SET rt, rs < IMM_signed" to unsigned comparation
    pub inline fn SLTIU(self: *Cpu) void {
        const bigger = self.regs[maskRs(self.current_op)] < maskSignedImm(self.current_op);
        self.regs[maskRt(self.current_op)] = bigger;
    }

    //"STORE [rs+imm], rt"
    pub inline fn SW(self: *Cpu) void {
        if (!(self.regs_cop0[12] & 0b10000)) //WRITING TO CACHE
            self.write32((self.regs[maskRs(self.current_op)] + maskSignedImm(self.current_op)), self.regs[maskRt(self.current_op)]);
    }

    //
    pub inline fn SWC0(self: *Cpu ) void {
        _ = self;
        @panic("SWC0 not implemented");
    }

    //
    pub inline fn SWC1(self: *Cpu ) void {
        _ = self;
        @panic("SWC1 not implemented");
    }

    //
    pub inline fn SWC2(self: *Cpu ) void {
        _ = self;
        @panic("SWC2 not implemented");
    }

    //
    pub inline fn SWC3(self: *Cpu ) void {
        _ = self;
        @panic("SWC3 not implemented");
    }

    //"STORE [rs+imm], rt-most_sign16"
    pub inline fn SWL(self: *Cpu) void {
        const value = self.regs[maskRt(self.current_op)] >> 16;
        self.write16(self.regs[maskRs(self.current_op)] + maskSignedImm(self.current_op), @as(u16, @truncate(value)));
    }

    //"STORE [rs+imm], rt-least_sign16"
    pub inline fn SWR(self: *Cpu) void {
        const value = self.regs[maskRt(self.current_op)];
        self.write16(self.regs[maskRs(self.current_op)] + maskSignedImm(self.current_op), @as(u16, @truncate(value)));
    }

    // XOR rt, rs ^ IMM
    pub inline fn XORI(self: *Cpu) void {
        self.regs[maskRt(self.current_op)] = self.regs[maskRs(self.current_op)] ^ maskImm(self.current_op);
    }

    ///////////////////////////////////////
    // SECUNDARY OPCODES
    ///////////////////////////////////////

    //"ADD rd, rs(signed) + rt(signed)" exception signed overflow
    pub inline fn ADD(self: *Cpu) void {
        const result = @as(i32, @bitCast(self.regs[maskRs(self.current_op)])) + @as(i32, @bitCast(self.regs[maskRt(self.current_op)]));
        self.regs[maskRd(self.current_op)] = @as(u32, @bitCast(result));
    }

    //"ADD rd, rs + rt"
    pub inline fn ADDU(self: *Cpu) void {
        self.regs[maskRd(self.current_op)] = self.regs[maskRs(self.current_op)] +% self.regs[maskRt(self.current_op)];
    }

    //"AND rd, rt & rs"
    pub inline fn AND(self: *Cpu) void {
        self.regs[maskRd(self.current_op)] = self.regs[maskRt(self.current_op)] & self.regs[maskRs(self.current_op)];
    }

    //
    pub inline fn BREAK(self: *Cpu ) void {
        _ = self;
        @panic("BREAK not implemented");
    }

    //DIV lo, rs-signed / rt-signed ------- hi = remainder
    pub inline fn DIV(self: *Cpu) void {
        const numerator = @as(i32, @bitCast(self.regs[maskRs(self.current_op)]));
        const denominator = @as(i32, @bitCast(self.regs[maskRt(self.current_op)]));
        if (denominator == 0) {
            if (numerator < 0) {
                self.lo = 1;
            } else {
                self.lo = 0xff_ff_ff_ff;
            }
            self.hi = numerator;
        } else {
            self.hi = @as(u32, @bitCast(numerator % denominator));
            self.lo = @as(u32, @bitCast(numerator / denominator));
        }
    }

    //DIV lo, rs / rt ------- hi = remainder
    pub inline fn DIVU(self: *Cpu) void {
        const numerator = self.regs[maskRs(self.current_op)];
        const denominator = self.regs[maskRt(self.current_op)];
        if (denominator == 0) {
            self.lo = 0xff_ff_ff_ff;
            self.hi = numerator;
        } else {
            self.hi = numerator % denominator;
            self.lo = numerator / denominator;
        }
    }

    //"JMP rs, and store RETURN in rd"
    pub inline fn JALR(self: *Cpu) void {
        self.regs[maskRd(self.current_op)] = self.pc;
        self.pc = self.regs[maskRs(self.current_op)];
    }

    //"JMP rs :)"
    pub inline fn JR(self: *Cpu) void {
        self.pc = self.regs[maskRs(self.current_op)];
    }

    //"LOAD rd, HI" //TODOO: CLOCK THE DIVISIONSs TO KNOW IF THIS RESULT IS READY
    pub inline fn MFHI(self: *Cpu) void {
        self.regs[maskRd(self.current_op)] = self.hi;
    }

    //"LOAD rd, LO" //TODOO: CLOCK THE DIVISIONSs TO KNOW IF THIS RESULT IS READY
    pub inline fn MFLO(self: *Cpu) void {
        self.regs[maskRd(self.current_op)] = self.lo;
    }

    //"Mov HI, rs"
    pub inline fn MTHI(self: *Cpu) void {
        self.hi = self.regs[maskRs(self.current_op)];
    }

    //"Mov LO, rs"
    pub inline fn MTLO(self: *Cpu) void {
        self.lo = self.regs[maskRs(self.current_op)];
    }

    //mult hi-lo 64bit, rt-signed * rs-signed
    pub inline fn MULT(self: *Cpu) void {
        const rt_64 = @as(i64, @intCast(@as(i32, @bitCast(self.regs[maskRt(self.current_op)]))));
        const rs_64 = @as(i64, @intCast(@as(i32, @bitCast(self.regs[maskRs(self.current_op)]))));

        const result = @as(u64, @intCast((rt_64 * rs_64)));

        self.hi = @as(u32, @truncate((result >> 32)));
        self.lo = @as(u32, @truncate((result)));
    }

    //mult hi-lo 64bit, rt * rs
    pub inline fn MULTU(self: *Cpu) void {
        const rt_64 = @as(u64, @intCast(self.regs[maskRt(self.current_op)]));
        const rs_64 = @as(u64, @intCast(self.regs[maskRs(self.current_op)]));

        const result = rt_64 * rs_64;

        self.hi = @as(u32, @truncate((result >> 32)));
        self.lo = @as(u32, @truncate((result)));
    }

    // NOR rd, rt NOR rs
    pub inline fn NOR(self: *Cpu) void {
        const result = !(self.regs[maskRt(self.current_op)] | self.regs[maskRs(self.current_op)]);
        self.regs[maskRd(self.current_op)] = result;
    }

    //"OR rd, rt | rs "
    pub inline fn OR(self: *Cpu) void {
        self.regs[maskRd(self.current_op)] = self.regs[maskRt(self.current_op)] | self.regs[maskRs(self.current_op)];
    }

    //"SHL rd, rt << imms"
    pub inline fn SLL(self: *Cpu) void {
        self.regs[maskRd(self.current_op)] = self.regs[maskRt(self.current_op)] << maskImms(self.current_op);
    }

    // SHL rd, rt << rs-5_low_bits
    pub inline fn SLLV(self: *Cpu) void {
        const shift = self.regs[maskRs(self.current_op)] & 0b11111;
        self.regs[maskRd(self.current_op)] = self.regs[maskRt(self.current_op)] << shift;
    }

    //"SET rd, rs(signed) < rt(signed)"
    pub inline fn SLT(self: *Cpu) void {
        const bigger = @as(i32, @bitCast((self.regs[maskRs(self.current_op)]))) < @as(i32, @bitCast(self.regs[maskRt(self.current_op)]));

        self.regs[maskRd(self.current_op)] = bigger;
    }

    //"SET rd, rs < rt"
    pub inline fn SLTU(self: *Cpu) void {
        const bigger = (self.regs[maskRs(self.current_op)] < self.regs[maskRt(self.current_op)]);

        self.regs[maskRd(self.current_op)] = bigger;
    }

    //"SHR rd, rt(signed) >> imms"
    pub inline fn SRA(self: *Cpu) void {
        const result = @as(i32, @bitCast(self.regs[maskRt(self.current_op)])) >> maskImms(self.current_op);
        self.regs[maskRd(self.current_op)] = @as(u32, @bitCast(result));
    }

    //SHR rd, rt(singed) >> rs-5_low_bits
    pub inline fn SRAV(self: *Cpu) void {
        const shift = self.regs[maskRs(self.current_op)] & 0b11111;
        const result = @as(i32, @bitCast(self.regs[maskRt(self.current_op)])) >> shift;
        self.regs[maskRd(self.current_op)] = @as(u32, @bitCast(result));
    }

    //"SHR rd, rt >> imms"
    pub inline fn SRL(self: *Cpu) void {
        const result = self.regs[maskRt(self.current_op)] >> maskImms(self.current_op);
        self.regs[maskRd(self.current_op)] = result;
    }

    // SHR rd, rt >> rs-5_low_bits
    pub inline fn SRLV(self: *Cpu) void {
        const shift = self.regs[maskRs(self.current_op)] & 0b11111;
        self.regs[maskRd(self.current_op)] = self.regs[maskRt(self.current_op)] >> shift;
    }

    //"SUB rd, rs - rt" exception
    pub inline fn SUB(self: *Cpu) void {
        const result = @as(i32, @bitCast(self.regs[maskRs(self.current_op)])) - @as(i32, @bitCast(self.regs[maskRt(self.current_op)]));
        self.regs[maskRd(self.current_op)] = @as(u32, @bitCast(result));
    }

    //"SUB rd, rs - rt"
    pub inline fn SUBU(self: *Cpu) void {
        self.regs[maskRd(self.current_op)] = self.regs[maskRs(self.current_op)] -% self.regs[maskRt(self.current_op)];
    }

    //
    pub inline fn SYSCALL(self: *Cpu ) void {
        _ = self;
        @panic("SYSCALL not implemented");
    }

    //XOR rd, rs ^ rt
    pub inline fn XOR(self: *Cpu) void {
        self.regs[maskRd(self.current_op)] = self.regs[maskRs(self.current_op)] ^ self.regs[maskRt(self.current_op)];
    }
};
