with Ada.Containers.Vectors;
with Interfaces;
use  Interfaces;

package Tostiera is

  Kilobyte       : constant := 2 ** 10;
  Display_Width  : constant := 64;
  Display_Height : constant := 32;

  -- 0.166 for 16.6 milliseconds for 60Hz.
  Period : constant :=  0.01;

  type Byte is new Unsigned_8;
  type Halfword is new Unsigned_16;
  type Display_Type is array (0 .. Display_Width - 1, 0 .. Display_Height - 1) of Boolean;
  type Memory_Type is array (0 .. 4 * Kilobyte - 1) of Byte;
  type Registers_Type is array (0 .. 15) of Byte;
  type Keyboard_Type is array (0 .. 15) of Boolean;

  subtype Address is Halfword;

  package Stacks is new
    Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Address);
  use Stacks;

  I  : Halfword := 0;
  PC : Halfword := 0;
  IR : Halfword := 0;

  Stack : Stacks.Vector;

  Delay_Timer : Byte;
  Sound_Timer : Byte;

  Display   : Display_Type;
  Memory    : Memory_Type;
  Registers : Registers_Type;
  Keyboard  : Keyboard_Type;

  Quit    : Boolean := False;
  Running : Boolean := True;

  procedure Push (Adr : Address);
  function Pop return Address;
  function Top return Address;

  procedure Decrement_Timers;

  procedure Init;
  procedure Load_Program (File_Path : String);

  procedure Clock_Cycle;
  procedure Fetch_Instruction;
  procedure Decode_And_Execute_Instruction;

  type Instruction_Type is
   (CLS,
    RET,
    SYS_addr,
    JP_addr,
    CALL_addr,
    SE_Vx_byte,
    SNE_Vx_byte,
    SE_Vx_Vy,
    LD_Vx_byte,
    ADD_Vx_byte,
    LD_Vx_Vy,
    OR_Vx_Vy,
    AND_Vx_Vy,
    XOR_Vx_Vy,
    ADD_Vx_Vy,
    SUB_Vx_Vy,
    SHR_Vx_Vy,
    SUBN_Vx_Vy,
    SHL_Vx_Vy,
    SNE_Vx_Vy,
    LD_I_addr,
    JP_V0_addr,
    RND_Vx_byte,
    DRW_Vx_Vy_nibble,
    SKP_Vx,
    SKNP_Vx,
    LD_Vx_DT,
    LD_Vx_K,
    LD_DT_Vx,
    LD_ST_Vx,
    ADD_I_Vx,
    LD_F_Vx,
    LD_B_Vx,
    LD_I_Vx,
    LD_Vx_I,
    Invalid);

  Instruction : Instruction_Type;

  procedure Dump_Registers;
  procedure Dump_Memory;

  Font : array (Natural range <>) of Byte := [
    16#F0#, 16#90#, 16#90#, 16#90#, 16#F0#, -- 0
    16#20#, 16#60#, 16#20#, 16#20#, 16#70#, -- 1
    16#F0#, 16#10#, 16#F0#, 16#80#, 16#F0#, -- 2
    16#F0#, 16#10#, 16#F0#, 16#10#, 16#F0#, -- 3
    16#90#, 16#90#, 16#F0#, 16#10#, 16#10#, -- 4
    16#F0#, 16#80#, 16#F0#, 16#10#, 16#F0#, -- 5
    16#F0#, 16#80#, 16#F0#, 16#90#, 16#F0#, -- 6
    16#F0#, 16#10#, 16#20#, 16#40#, 16#40#, -- 7
    16#F0#, 16#90#, 16#F0#, 16#90#, 16#F0#, -- 8
    16#F0#, 16#90#, 16#F0#, 16#10#, 16#F0#, -- 9
    16#F0#, 16#90#, 16#F0#, 16#90#, 16#90#, -- A
    16#E0#, 16#90#, 16#E0#, 16#90#, 16#E0#, -- B
    16#F0#, 16#80#, 16#80#, 16#80#, 16#F0#, -- C
    16#E0#, 16#90#, 16#90#, 16#90#, 16#E0#, -- D
    16#F0#, 16#80#, 16#F0#, 16#80#, 16#F0#, -- E
    16#F0#, 16#80#, 16#F0#, 16#80#, 16#80#  -- F
  ];
end Tostiera;