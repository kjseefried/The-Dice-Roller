-------------------------------------------------------------------------------
-- Dice.CLI (body)
--
-- The methods necessary for checking parameters passed to DiceRoller when
-- the program was started.
--
-- Author: Thomas Løcke
-- Copyleft 2010. You may freely do with this source as you wish.
-------------------------------------------------------------------------------
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;         use Ada.Directories;

package body Dice.CLI is
   procedure Check_Passed_Parameters is
   begin
      for i in 1 .. Argument_Count loop
         if To_Lower (Argument (i)) = "-h" then
            raise Help_Needed;
         end if;

         if not Exists (Name => Argument (Number => i)) then
            raise File_Error with Argument (Number => i);
         end if;
      end loop;
   end Check_Passed_Parameters;
end Dice.CLI;