with Ada.Text_IO;
use  Ada.Text_IO;
with Ada.Sequential_IO;
with Ada.Integer_Text_IO;
use  Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

package body Tostiera is

  procedure Push (Adr : Address) is
  begin
    Stack.Prepend (Adr);
  end Push;

  function Pop return Address is
    Ret : constant Address := Stack.First_Element;
  begin
    Stack.Delete_First;
    return Ret;
  end Pop;

  function Top return Address is
  begin
    return Stack.First_Element;
  end Top;

  procedure Decrement_Timers is
  begin
    if Delay_Timer > 0 then
      Delay_Timer := Delay_Timer - 1;
    end if;
    if Sound_Timer > 0 then
      Sound_Timer := Sound_Timer - 1;
    end if;
  end Decrement_Timers;

  procedure Load_Program (File_Path : String) is
    package Byte_IO is
      new Ada.Sequential_IO (Byte);
    use Byte_IO;

    F : Byte_IO.File_Type;

    Current_Byte : Byte;
  begin
    Put_Line ("Loading program at: " & File_Path);

    Open (F, In_File, File_Path);
    for Adr in Memory'First + 16#200# .. Memory'Last loop
      if End_Of_File (F) then
        Put_Line ("Load_Program: finished.");
        exit;
      end if;
      Read (F, Current_Byte);
      Memory (Adr) := Current_Byte;
    end loop;
    Close (F);
    Put_Line ("Dumping memory...");
    Put_Line ("Memory: " & Memory'Image);
  end Load_Program;

  procedure Clock_Cycle is
  begin
    if not Running then
      return;
    end if;
    Fetch_Instruction;
    Decode_Instruction;
    Execute_Instruction;
  end Clock_Cycle;

  procedure Fetch_Instruction is
  begin
    IR := 0;
    IR := IR or Halfword (Memory (Natural (PC)));
    IR := Shift_Left (IR, 8);
    IR := IR or Halfword (Memory (Natural (PC + 1)));
    PC := PC + 2;
  end Fetch_Instruction;

  procedure Decode_Instruction is
    Most_Significant_Nibble  : constant Halfword := Shift_Right (IR, 12);
    Least_Significant_Nibble : constant Halfword := IR and 16#000F#;
    Least_Significant_Byte   : constant Halfword := IR and 16#00FF#;
  begin
    declare
      Prev_PC : constant Halfword := PC - 2;
    begin
      Put ("[PC: " & Prev_PC'Image & "] ");
    end;

    Put ("IR: ");
    Put (Item => Natural (IR), Base => 16);
    Put (" ");

    case IR is
      -- 00E0 - CLS
      when 16#00E0# =>
        Instruction := CLS;
        Put_Line ("00E0 - CLS");
        return;
      -- 00EE - RET
      when 16#00EE# =>
        Instruction := RET;
        Put_Line ("00EE - RET");
        return;
      when others =>
        null;
    end case;

    case Most_Significant_Nibble is
      --  1nnn - JP addr
      when 1 =>
        Instruction := JP_addr;
        Put ("JP_add - 1nnn");
      -- 2nnn - CALL addr
      when 2 =>
        Instruction := CALL_addr;
        Put ("2nnn - CALL addr");
      -- 3xkk - SE Vx, byte
      when 3 =>
        Instruction := SE_Vx_byte;
        Put ("3xkk - SE Vx, byte");
      -- 4xkk - SNE Vx, byte
      when 4 =>
        Instruction := SNE_Vx_byte;
        Put ("4xkk - SNE Vx, byte");
      -- 5xy0 - SE Vx, Vy -- No other instruction starts with 5,
                          -- so we can skip checking for the 0.
      when 5 =>
        Instruction := SE_Vx_Vy;
        Put ("5xy0 - SE Vx, Vy");
      -- 6xkk - LD Vx, byte
      when 6 =>
        Instruction := LD_Vx_byte;
        Put ("6xkk - LD Vx, byte");
      -- 7xkk - ADD Vx, byte
      when 7 =>
        Instruction := ADD_Vx_byte;
        Put ("7xkk - ADD Vx, byte");
      when 8 =>
        case Least_Significant_Nibble is
          -- 8xy0 - LD Vx, Vy
          when 0 =>
            Instruction := LD_Vx_Vy;
            Put ("8xy0 - LD Vx, Vy");
          -- 8xy1 - OR Vx, Vy
          when 1 =>
            Instruction := OR_Vx_Vy;
            Put ("8xy1 - OR Vx, Vy");
          -- 8xy2 - AND Vx, Vy
          when 2 =>
            Instruction := AND_Vx_Vy;
            Put ("8xy2 - AND Vx, Vy");
          -- 8xy3 - XOR Vx, Vy
          when 3 =>
            Instruction := XOR_Vx_Vy;
            Put ("8xy3 - XOR Vx, Vy");
          -- 8xy4 - ADD Vx, Vy
          when 4 =>
            Instruction := ADD_Vx_Vy;
            Put ("8xy4 - ADD Vx, Vy");
          -- 8xy5 - SUB Vx, Vy
          when 5 =>
            Instruction := SUB_Vx_Vy;
            Put ("8xy5 - SUB Vx, Vy");
          -- 8xy6 - SHR Vx {, Vy}
          when 6 =>
            Instruction := SHR_Vx_Vy;
            Put ("8xy6 - SHR Vx {, Vy}");
          -- 8xy7 - SUBN Vx, Vy
          when 7 =>
            Instruction := SUBN_Vx_Vy;
            Put ("8xy7 - SUBN Vx, Vy");
          -- 8xyE - SHL Vx {, Vy}
          when 16#E# =>
            Instruction := SHL_Vx_Vy;
            Put ("8xyE - SHL Vx {, Vy}");
          when others =>
            null;
        end case;
      -- 9xy0 - SNE Vx, Vy -- No other instruction starts with 9,
                          -- so we can skip checking for the 0.
      when 9 =>
        Instruction := SNE_Vx_Vy;
        Put ("9xy0 - SNE Vx, Vy");
      -- Annn - LD I, addr
      when 16#A# =>
        Instruction := LD_I_addr;
        Put ("Annn - LD I, addr");
      -- Bnnn - JP V0, addr
      when 16#B# =>
        Instruction := JP_V0_addr;
        Put ("Bnnn - JP V0, addr");
      -- Cxkk - RND Vx, byte
      when 16#C# =>
        Instruction := RND_Vx_byte;
        Put ("Cxkk - RND Vx, byte");
      -- Dxyn - DRW Vx, Vy, nibble
      when 16#D# =>
        Instruction := DRW_Vx_Vy_nibble;
        Put ("Dxyn - DRW Vx, Vy, nibble");
      when 16#E# =>
        case Least_Significant_Nibble is
          -- Ex9E - SKP Vx
          when 16#9E# =>
            Instruction := SKP_Vx;
            Put ("Ex9E - SKP Vx");
          -- ExA1 - SKNP Vx
          when 16#A1# =>
            Instruction := SKNP_Vx;
            Put ("ExA1 - SKNP Vx");
          when others =>
            null;
        end case;
      when 16#F# =>
        case Least_Significant_Byte is
          -- Fx07 - LD Vx, DT
          when 7 =>
            Instruction := LD_Vx_DT;
            Put ("Fx07 - LD Vx, DT");
          -- Fx0A - LD Vx, K
          when 16#0A# =>
            Instruction := LD_Vx_K;
            Put ("Fx0A - LD Vx, K");
          -- Fx15 - LD DT, Vx
          when 16#15# =>
            Instruction := LD_DT_Vx;
            Put ("Fx15 - LD DT, Vx");
          -- Fx18 - LD ST, Vx
          when 16#18# =>
            Instruction := LD_ST_Vx;
            Put ("Fx18 - LD ST, Vx");
          -- Fx1E - ADD I, Vx
          when 16#1E# =>
            Instruction := ADD_I_Vx;
            Put ("Fx1E - ADD I, Vx");
          -- Fx29 - LD F, Vx
          when 16#29# =>
            Instruction := LD_F_Vx;
            Put ("Fx29 - LD F, Vx");
          -- Fx33 - LD B, Vx
          when 16#33# =>
            Instruction := LD_B_Vx;
            Put ("Fx33 - LD B, Vx");
          -- Fx55 - LD [I], Vx
          when 16#55# =>
            Instruction := LD_I_Vx;
            Put ("Fx55 - LD [I], Vx");
          -- Fx65 - LD Vx, [I]
          when 16#65# =>
            Instruction := LD_Vx_I;
            Put ("Fx65 - LD Vx, [I]");
          when others =>
            null;
        end case;
      when others =>
        Put ("ERROR: Invalid instruction");
    end case;
    New_Line;
  end Decode_Instruction;

  -- http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#3.1
  procedure Execute_Instruction is
    -- 16#0xy0#
    X   : constant Natural := Natural (Shift_Right (IR, 8) and 16#000F#);
    Y   : constant Natural := Natural (Shift_Right (IR, 4) and 16#000F#);
    NNN : constant Halfword := IR and 16#0FFF#;
    -- 16#00kk#
    Least_Significant_Byte : constant Byte := Byte (IR and 16#00FF#);
  begin
    case Instruction is
      when CLS =>
        for Pixel of Display loop
          Pixel := False;
        end loop;

      when RET =>
        PC := Pop;

      when JP_addr =>
        PC := NNN;

      when CALL_addr =>
        Push (PC);
        PC := NNN;

      when SE_Vx_byte =>
        if Registers (X) = Least_Significant_Byte then
          PC := PC + 2;
        end if;

      when SNE_Vx_byte =>
        if Registers (X) /= Least_Significant_Byte then
          PC := PC + 2;
        end if;

      when SE_Vx_Vy =>
        if Registers (X) = Registers (Y) then
          PC := PC + 2;
        end if;

      when LD_Vx_byte =>
        Registers (X) := Least_Significant_Byte;

      when ADD_Vx_byte =>
        Registers (X) := Registers (X) + Least_Significant_Byte;

      when LD_Vx_Vy =>
        Registers (X) := Registers (Y);

      when OR_Vx_Vy =>
        Registers (X) := Registers (X) or Registers (Y);

      when AND_Vx_Vy =>
        Registers (X) := Registers (X) and Registers (Y);

      when XOR_Vx_Vy =>
        Registers (X) := Registers (X) xor Registers (Y);

      -- Registers (16#F#) := (if Registers (X) + Registers (Y) > 16#FF# then 1 else 0);
      when ADD_Vx_Vy =>
        declare
          Overflow : constant Byte := Registers (X) + Registers (Y);
        begin
          Registers (16#F#) := 0;
        exception
          -- Overflow
          when Constraint_Error =>
            Registers (16#F#) := 1;
        end;
        Registers (X) := Registers (X) + Registers (Y);

      when SUB_Vx_Vy =>
        Registers (16#F#) := (if Registers (X) > Registers (Y) then 1 else 0);
        Registers (X) := Registers (X) - Registers (Y);

      when SHR_Vx_Vy =>
        Registers (16#F#) := (if (Registers (X) and 1) = 1 then 1 else 0);
        Registers (X) := Shift_Right (Registers (X), 1);

      when SUBN_Vx_Vy =>
        Registers (16#F#) := (if Registers (Y) > Registers (X) then 1 else 0);
        Registers (X) := Registers (Y) - Registers (X);

      when SHL_Vx_Vy =>
        Registers (16#F#) :=
          (if (Registers (X) and 16#80#) = 16#80# then 1 else 0);
        Registers (X) := Shift_Left (Registers (X), 1);

      when SNE_Vx_Vy =>
        if Registers (X) /= Registers (Y) then
          PC := PC + 2;
        end if;

      when LD_I_addr =>
        I := NNN;

      when JP_V0_addr =>
        PC := NNN + Halfword (Registers (0));

      when RND_Vx_byte =>
        declare
          subtype Random_Range is Byte range 0 .. 255;

          package R is new
            Ada.Numerics.Discrete_Random (Random_Range);
          use R;

          RNG  : Generator;
          Rand : Random_Range;
        begin
          Reset (RNG);
          Rand := Random (RNG);
          Registers (X) := Byte (Rand) and Least_Significant_Byte;
        end;

      when DRW_Vx_Vy_nibble =>
        declare
          X_Coord : constant Natural := Natural (Registers (X));
          Y_Coord : constant Natural := Natural (Registers (Y));
          N : constant Natural := Natural (Least_Significant_Byte and 16#0F#);
          Sprite_Byte : Byte := 0;
          Bit : Byte := 0;
        begin
          Registers (16#F#) := 0;
          for Row in 0 .. N - 1 loop
            Sprite_Byte := Memory (Natural (I) + Row);
            for Bit_Position in 0 .. 7 loop
              Bit := Shift_Right (Sprite_Byte, 7 - Bit_Position) and 1;
              declare
                X_Offset : constant Natural :=
                  (X_Coord + Bit_Position) mod Display_Width;
                Y_Offset : constant Natural :=
                  (Y_Coord + Row) mod Display_Height;
              begin
                if Bit = 1 then
                  if Display (X_Offset, Y_Offset) then
                    Registers (16#F#) := 1;
                  end if;
                  Display (X_Offset, Y_Offset) :=
                    Display (X_Offset, Y_Offset) xor True;
                end if;
              end;
            end loop;
          end loop;
        end;

      when SKP_Vx =>
        if Keyboard (X) then
          PC := PC + 2;
        end if;

      when SKNP_Vx =>
        if not Keyboard (X) then
          PC := PC + 2;
        end if;

      when LD_Vx_DT =>
        Registers (X) := Delay_Timer;

      when LD_Vx_K =>
        declare
          Pressed : Boolean := False;
        begin
          for Key in Keyboard'Range loop
            if Keyboard (Key) then
              Registers (X) := Byte (Key);
              Pressed := True;
            end if;
          end loop;
          if not Pressed then
            PC := PC - 2;
            return;
          end if;
        end;

      when LD_DT_Vx =>
        Delay_Timer := Registers (X);

      when LD_ST_Vx =>
        Sound_Timer := Registers (X);

      when ADD_I_Vx =>
        I := I + Halfword (Registers (X));

      when LD_F_Vx =>
        I := Halfword (Registers (X) * 5);

      when LD_B_Vx =>
        Memory (Natural (I))     := Registers (X) / 100;
        Memory (Natural (I) + 1) := Registers (X) / 10 mod 10;
        Memory (Natural (I) + 2) := Registers (X) mod 10;

      when LD_I_Vx =>
        for Index in 0 .. X loop
          Memory (Natural (I) + Natural (Index)) := Registers (Natural (Index));
        end loop;

      when LD_Vx_I =>
        for Index in 0 .. X loop
          Registers (Natural (Index)) := Memory (Natural (I) + Natural (Index));
        end loop;

      when others =>
        Put_Line ("ERROR: Invalid instruction!");
    end case;
  end Execute_Instruction;

  procedure Init is
  begin
    PC := 16#200#;
    for Index in Font'Range loop
      Memory (Index) := Font (Index);
    end loop;
  end Init;

  procedure Dump_Registers is
  begin
    Put_Line ("Dumping...");
    Put_Line ("[Registers]");
    Put ("IR: ");
    Put (Item => Natural (IR), Base => 16);
    New_Line;
    Put ("PC: ");
    Put (Item => Natural (PC), Base => 16);
    New_Line;
    Put ("I: ");
    Put (Item => Natural (I), Base => 16);
    New_Line;

    Put_Line ("[Timers]");
    Put ("DT: ");
    Put (Item => Natural (Delay_Timer), Base => 16);
    New_Line;
    Put ("ST: ");
    Put (Item => Natural (Sound_Timer), Base => 16);
    New_Line;

    Put_Line ("[Data Registers]");
    for Index in Registers'Range loop
      Put ("V" & Index'Image & ": ");
      Put (Item => Natural (Registers (Index)), Base => 16);
      --  Put (", ");
      --  if Index mod 3 = 0 then
      --    New_Line;
      --  end if;
      New_Line;
    end loop;
  end Dump_Registers;

  -- TODO: Print addresses and values in hex
  procedure Dump_Memory is
  begin
    Put_Line ("Dumping...");
    Put_Line ("[Memory]");
    Put_Line (Memory'Image);
  end Dump_Memory;
end Tostiera;