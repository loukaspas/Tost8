with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Command_Line;
use  Ada.Command_Line;

with SDL;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Video.Renderers.Makers;
with SDL.Video.Windows.Makers;
with SDL.Video.Palettes;

with Tostiera;
use  Tostiera;

procedure Tost8 is
  Pixel_Size : constant := 20;
  W_Width    : constant := Display_Width * Pixel_Size;
  W_Height   : constant := Display_Height * Pixel_Size;

  Black : constant SDL.Video.Palettes.Colour := (  0,   0,   0, 255);
  White : constant SDL.Video.Palettes.Colour := (255, 255, 255, 255);

  W        : SDL.Video.Windows.Window;
  W_Size   : constant SDL.Positive_Sizes := (W_Width, W_Height);
  Renderer : SDL.Video.Renderers.Renderer;
  Event    : SDL.Events.Events.Events;

  use type SDL.Events.Event_Types;

  procedure Draw_Display is
    X : SDL.C.int := 0;
    Y : SDL.C.int := 0;
  begin
    Renderer.Clear;
    for J in Display'Range (2) loop
      for I in Display'Range (1) loop
        Renderer.Set_Draw_Colour (if Display (I, J) then White else Black);
          X := SDL.C.int(I * Pixel_Size);
          Y := SDL.C.int(J * Pixel_Size);
          Renderer.Fill (Rectangle => (X, Y, Pixel_Size, Pixel_Size));
          -- Black outline for each pixel.
          Renderer.Set_Draw_Colour (Black);
          Renderer.Draw (Rectangle => (X, Y, Pixel_Size, Pixel_Size));
      end loop;
    end loop;
    Renderer.Present;
  end Draw_Display;

  procedure Handle_Event is
  begin
    while SDL.Events.Events.Poll (Event) loop
      if Event.Common.Event_Type = SDL.Events.Quit then
        Tostiera.Quit := True;
      end if;
      ------------------------
      --  KEYBOARD MAPPING  --
      ------------------------
      -- 1 2 3 C -- 1 2 3 4 --
      -- 4 5 6 D -- Q W E R --
      -- 7 8 9 E -- A S D F --
      -- A 0 B F -- Z X C V --
      ------------------------
      if Event.Keyboard.Event_Type = SDL.Events.Keyboards.Key_Down then
        case Event.Keyboard.Key_Sym.Key_Code is
          when SDL.Events.Keyboards.Code_1 =>
            Keyboard (16#1#) := True;
          when SDL.Events.Keyboards.Code_2 =>
            Keyboard (16#2#) := True;
          when SDL.Events.Keyboards.Code_3 =>
            Keyboard (16#3#) := True;
          when SDL.Events.Keyboards.Code_4 =>
            Keyboard (16#C#) := True;
          when SDL.Events.Keyboards.Code_Q =>
            Keyboard (16#4#) := True;
          when SDL.Events.Keyboards.Code_W =>
            Keyboard (16#5#) := True;
          when SDL.Events.Keyboards.Code_E =>
            Keyboard (16#6#) := True;
          when SDL.Events.Keyboards.Code_R =>
            Keyboard (16#D#) := True;
          when SDL.Events.Keyboards.Code_A =>
            Keyboard (16#7#) := True;
          when SDL.Events.Keyboards.Code_S =>
            Keyboard (16#8#) := True;
          when SDL.Events.Keyboards.Code_D =>
            Keyboard (16#9#) := True;
          when SDL.Events.Keyboards.Code_F =>
            Keyboard (16#E#) := True;
          when SDL.Events.Keyboards.Code_Z =>
            Keyboard (16#A#) := True;
          when SDL.Events.Keyboards.Code_X =>
            Keyboard (16#0#) := True;
          when SDL.Events.Keyboards.Code_C =>
            Keyboard (16#B#) := True;
          when SDL.Events.Keyboards.Code_V =>
            Keyboard (16#F#) := True;

          -- TODO: 
          -- - Faster/slower emulation
          -- - Step    
          when SDL.Events.Keyboards.Code_P =>
            Put_Line ("Emulator " & (if Running then "Paused." else "Resumed."));
            Running := not Running;
          when SDL.Events.Keyboards.Code_Escape =>
            Tostiera.Quit := True;
          when SDL.Events.Keyboards.Code_L =>
            Tostiera.Dump_Registers;
          when SDL.Events.Keyboards.Code_M =>
            Tostiera.Dump_Memory;

          when others =>
            null;
        end case;
      elsif Event.Keyboard.Event_Type = SDL.Events.Keyboards.Key_Up then
        case Event.Keyboard.Key_Sym.Key_Code is
          when SDL.Events.Keyboards.Code_1 =>
            Keyboard (16#1#) := False;
          when SDL.Events.Keyboards.Code_2 =>
            Keyboard (16#2#) := False;
          when SDL.Events.Keyboards.Code_3 =>
            Keyboard (16#3#) := False;
          when SDL.Events.Keyboards.Code_4 =>
            Keyboard (16#C#) := False;
          when SDL.Events.Keyboards.Code_Q =>
            Keyboard (16#4#) := False;
          when SDL.Events.Keyboards.Code_W =>
            Keyboard (16#5#) := False;
          when SDL.Events.Keyboards.Code_E =>
            Keyboard (16#6#) := False;
          when SDL.Events.Keyboards.Code_R =>
            Keyboard (16#D#) := False;
          when SDL.Events.Keyboards.Code_A =>
            Keyboard (16#7#) := False;
          when SDL.Events.Keyboards.Code_S =>
            Keyboard (16#8#) := False;
          when SDL.Events.Keyboards.Code_D =>
            Keyboard (16#9#) := False;
          when SDL.Events.Keyboards.Code_F =>
            Keyboard (16#E#) := False;
          when SDL.Events.Keyboards.Code_Z =>
            Keyboard (16#A#) := False;
          when SDL.Events.Keyboards.Code_X =>
            Keyboard (16#0#) := False;
          when SDL.Events.Keyboards.Code_C =>
            Keyboard (16#B#) := False;
          when SDL.Events.Keyboards.Code_V =>
            Keyboard (16#F#) := False;
          when others =>
            null;
        end case;
      end if;
    end loop;
  end Handle_Event;

  Test_File_Path : String := "/home/loukas/src/roms/chip8-test-rom/test_opcode.ch8";
begin
  if Argument_Count /= 1 then
    Put_Line ("USAGE");
    Put_Line ("    tost8 path/to/rom");
    New_Line;
    Put_Line ("KEYBINDS");
    Put_Line ("    Keyboard");
    Put_Line ("    1 2 3 C -- 1 2 3 4");
    Put_Line ("    4 5 6 D -- Q W E R");
    Put_Line ("    7 8 9 E -- A S D F");
    Put_Line ("    A 0 B F -- Z X C V");
    New_Line;
    Put_Line ("    Other");
    Put_Line ("    P   - Pause");
    Put_Line ("    Esc - Quit");
    Put_Line ("    L   - Dump register values to console");
    Put_Line ("    M   - Dump memory to console");
    return;
  end if;

  if not SDL.Initialise then
    Put_Line ("SDL could not be initialised.");
    return;
  end if;

  SDL.Video.Windows.Makers.Create
   (Win      => W,
    Title    => "Tost8 Emulator",
    Position => SDL.Natural_Coordinates'(X => 0, Y => 0),
    Size     => W_Size);
    --  Flags    => SDL.Video.Windows.Resizable);

  SDL.Video.Renderers.Makers.Create (Renderer, W);

  Init;
  --  Load_Program (Test_File_Path);
  Load_Program (Argument (1));

  while not Tostiera.Quit loop
    Handle_Event;
    Clock_Cycle;
    Draw_Display;
    -- TODO: Implement sound.
    Decrement_Timers;
    delay Period;
  end loop;

  SDL.Finalise;

end Tost8;
