--  _pathroot TUI - UI Panels Body
--  Terminal UI panel management (basic text-based implementation)
--
--  SPDX-License-Identifier: AGPL-3.0-or-later
--  Copyright (C) 2025 Hyper Polymath

with Ada.Text_IO; use Ada.Text_IO;

package body Pathroot_TUI.UI.Panels is

   --  Current terminal size
   Term_Rows : Natural := 24;
   Term_Cols : Natural := 80;

   -------------------
   -- Initialize_UI --
   -------------------

   procedure Initialize_UI is
   begin
      --  Basic terminal setup
      --  In a full implementation, this would initialize ncurses/terminal
      Clear_Screen;
   end Initialize_UI;

   -----------------
   -- Finalize_UI --
   -----------------

   procedure Finalize_UI is
   begin
      Clear_Screen;
      Put_Line ("Goodbye from _pathroot TUI!");
   end Finalize_UI;

   ------------------
   -- Clear_Screen --
   ------------------

   procedure Clear_Screen is
   begin
      --  ANSI escape sequence to clear screen
      Put (ASCII.ESC & "[2J" & ASCII.ESC & "[H");
   end Clear_Screen;

   -----------------------
   -- Get_Terminal_Size --
   -----------------------

   procedure Get_Terminal_Size (Rows : out Natural; Cols : out Natural) is
   begin
      --  Default values; real implementation would query terminal
      Rows := Term_Rows;
      Cols := Term_Cols;
   end Get_Terminal_Size;

   -----------------
   -- Draw_Header --
   -----------------

   procedure Draw_Header (Env_Info : Environment_Info) is
      Line : constant String (1 .. Term_Cols) := (others => '=');
   begin
      Put_Line (Line);
      Put ("  _pathroot TUI v" & Version);
      Put_Line ((Term_Cols - 25) * ' ' & "[?] Help");
      Put_Line (Line);
      New_Line;
      Put_Line ("  Environment: " & To_String (Env_Info.Env));
      Put_Line ("  Profile:     " & To_String (Env_Info.Profile));
      Put_Line ("  Platform:    " & To_String (Env_Info.Platform));
      New_Line;
   end Draw_Header;

   -----------------
   -- Draw_Footer --
   -----------------

   procedure Draw_Footer is
      Line : constant String (1 .. Term_Cols) := (others => '-');
   begin
      Put_Line (Line);
      Put_Line ("  [E] Environment  [P] PATH  [L] Links  [G] Logs  [Q] Quit");
   end Draw_Footer;

   ----------------
   -- Draw_Panel --
   ----------------

   procedure Draw_Panel (Panel : Panel_ID; Env_Info : Environment_Info) is
   begin
      Clear_Screen;
      Draw_Header (Env_Info);

      case Panel is
         when Panel_Environment =>
            Put_Line ("╔════════════════════════════════════════════════════════╗");
            Put_Line ("║  ENVIRONMENT BROWSER                                   ║");
            Put_Line ("╠════════════════════════════════════════════════════════╣");
            Put_Line ("║  ● default     Active profile                          ║");
            Put_Line ("║  ○ test        Testing environment                     ║");
            Put_Line ("║  ○ production  Production settings                     ║");
            Put_Line ("╠════════════════════════════════════════════════════════╣");
            Put_Line ("║  [Enter] Switch  [N] New  [D] Delete  [R] Rename       ║");
            Put_Line ("╚════════════════════════════════════════════════════════╝");

         when Panel_PATH =>
            Put_Line ("╔════════════════════════════════════════════════════════╗");
            Put_Line ("║  PATH EDITOR                                           ║");
            Put_Line ("╠════════════════════════════════════════════════════════╣");
            Put_Line ("║  1. C:\devtools\bin                     [✓] Valid      ║");
            Put_Line ("║  2. C:\Windows\System32                 [✓] Valid      ║");
            Put_Line ("║  3. C:\Program Files\Git\bin            [✓] Valid      ║");
            Put_Line ("╠════════════════════════════════════════════════════════╣");
            Put_Line ("║  [A] Add  [E] Edit  [D] Delete  [↑↓] Move  [V] Validate║");
            Put_Line ("╚════════════════════════════════════════════════════════╝");

         when Panel_Links =>
            Put_Line ("╔════════════════════════════════════════════════════════╗");
            Put_Line ("║  SYMBOLIC LINKS                                        ║");
            Put_Line ("╠════════════════════════════════════════════════════════╣");
            Put_Line ("║  No symbolic links configured.                         ║");
            Put_Line ("║                                                        ║");
            Put_Line ("║  Create links to consolidate tools in devtools/bin     ║");
            Put_Line ("╠════════════════════════════════════════════════════════╣");
            Put_Line ("║  [N] New Link  [A] Audit All  [R] Repair  [X] Remove   ║");
            Put_Line ("╚════════════════════════════════════════════════════════╝");

         when Panel_Logs =>
            Put_Line ("╔════════════════════════════════════════════════════════╗");
            Put_Line ("║  LOG VIEWER                                            ║");
            Put_Line ("╠════════════════════════════════════════════════════════╣");
            Put_Line ("║  No log entries found.                                 ║");
            Put_Line ("║                                                        ║");
            Put_Line ("║  Logs will appear here as operations are performed.    ║");
            Put_Line ("╠════════════════════════════════════════════════════════╣");
            Put_Line ("║  [F] Filter  [C] Clear  [E] Export  [/] Search         ║");
            Put_Line ("╚════════════════════════════════════════════════════════╝");

         when Panel_Help =>
            Put_Line ("╔════════════════════════════════════════════════════════╗");
            Put_Line ("║  HELP                                                  ║");
            Put_Line ("╠════════════════════════════════════════════════════════╣");
            Put_Line ("║  _pathroot TUI provides interactive management of      ║");
            Put_Line ("║  your devtools environment.                            ║");
            Put_Line ("║                                                        ║");
            Put_Line ("║  Press the letter keys to switch panels.               ║");
            Put_Line ("║  Press Q to quit.                                      ║");
            Put_Line ("╚════════════════════════════════════════════════════════╝");
      end case;

      New_Line;
      Draw_Footer;
   end Draw_Panel;

   ------------------
   -- Handle_Input --
   ------------------

   procedure Handle_Input (Panel : in out Panel_ID; Running : in Out Boolean) is
      Input : Character;
   begin
      Get_Immediate (Input);

      case Input is
         when 'q' | 'Q' =>
            Running := False;

         when 'e' | 'E' =>
            Panel := Panel_Environment;

         when 'p' | 'P' =>
            Panel := Panel_PATH;

         when 'l' | 'L' =>
            Panel := Panel_Links;

         when 'g' | 'G' =>
            Panel := Panel_Logs;

         when '?' | 'h' | 'H' =>
            Panel := Panel_Help;

         when others =>
            null;
      end case;
   end Handle_Input;

end Pathroot_TUI.UI.Panels;
