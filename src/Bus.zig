const std = @import("std");
const assert = std.debug.assert;
const print = std.debug.print;
const allocator = std.heap.page_allocator;


//MEMORY POINTS
const BIOS = 0x1fc00000;
const BIOS_SIZE = 512 * 1024;

const RAM =  0;
const RAM_SIZE = 2 * 1024 * 1024;

const EXPANTION = 0x1f802041; //DEBUG HARDWARE IGNORE
const EXPANTION_SIZE = 66;

const Bus = struct
{

    MEMORY :[]u8,


    fn init() Bus
    {
        var self : Bus = Bus{
            .MEMORY = undefined,
        };
        self.MEMORY = allocator.alloc(u8, 1024*1024*64)catch{return undefined;};
        return self;
    }
    fn stop(self:*Bus) void
    {
        allocator.free(self.MEMORY);
    }

    fn Read_DWORD(self:*Bus, addr: u32) u32
    {
        //ALREADY DOES ALIMENTING CHECKING SO NO NEED TO CHECK IF ADDR IS MULTPLE OF 4
        const ptr = @ptrCast(*u32, @alignCast(4, &self.MEMORY[addr]));
        return ptr.*;
    }

    fn Write_DWORD(self:*Bus, addr: u32, value: u32) void
    {
        const ptr = @ptrCast(*u32, @alignCast(4, &self.MEMORY[addr]));
        ptr.* = value;
    }
    fn Write_WORD(self:*Bus, addr: u32, value: u16) void
    {
        const ptr = @ptrCast(*u16, @alignCast(2, &self.MEMORY[addr]));
        ptr.* = value;
    }
    fn Write_BYTE(self:*Bus, addr: u32, value: u8) void
    {
        const ptr = &self.MEMORY[addr];
        ptr.* = value;
    }


};

test "TESING FUNCTIONS" {
    var b = Bus.init();

    defer b.stop();
    std.debug.print("\n\n{d}\n\n\n",.{b.Read_DWORD(4)});
    b.Write_DWORD(4, 0xf0000);
    b.Write_WORD(4, 0x0043);

    std.debug.print("\n\n{d}\n\n\n",.{b.Read_DWORD(4)});

}