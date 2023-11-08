const std = @import("std");
const assert = std.debug.assert;
const print = std.debug.print;
const allocator = std.heap.page_allocator;


//memory POINTS
const BIOS = 0x1fc00000;
const BIOS_SIZE = 512 * 1024;

const RAM =  0;
const RAM_SIZE = 2 * 1024 * 1024;

const EXPANTION = 0x1f802041; //DEBUG HARDWARE IGNORE
const EXPANTION_SIZE = 66;

pub const Bus = struct
{

    //memory is not mapped this is just to have something to read from 
    //TODOO: implement proper memory map
    memory :[]u8,


    fn init() Bus
    {
        var self : Bus = Bus{
            .memory = undefined,
        };
        self.memory = allocator.alloc(u8, 1024*1024*64)catch{return undefined;};
        return self;
    }
    fn stop(self:*Bus) void
    {
        allocator.free(self.memory);
    }

    

    fn read32(self:*Bus, addr: u32) u32
    {
        const ptr = @as(*u32, @ptrCast(@alignCast(&self.memory[addr])));
        return ptr.*;
    }
    fn read16(self:*Bus, addr: u32) u16
    {
        const ptr = @as(*u16, @ptrCast(@alignCast(&self.memory[addr])));
        return ptr.*;
    }
    fn read8(self:*Bus, addr: u32) u8
    {
        return self.memory[addr];
    }

    

    fn write32(self:*Bus, addr: u32, value: u32) void
    {
        const ptr = @as(*u32, @ptrCast(@alignCast(&self.memory[addr])));
        ptr.* = value;
    }
    fn write16(self:*Bus, addr: u32, value: u16) void
    {
        const ptr = @as(*u16, @ptrCast(@alignCast(&self.memory[addr])));
        ptr.* = value;
    }
    fn write8(self:*Bus, addr: u32, value: u8) void
    {
        const ptr = &self.memory[addr];
        ptr.* = value;
    }


};

//test "TESING FUNCTIONS" {
//    var b = Bus.init();
//
//    defer b.stop();
//    std.debug.print("\n\n{d}\n\n\n",.{b.Read_DWORD(4)});
//    b.Write_DWORD(4, 0xf0000);
//    b.Write_WORD(4, 0x0043);
//
//    std.debug.print("\n\n{d}\n\n\n",.{b.Read_DWORD(4)});
//
//}
