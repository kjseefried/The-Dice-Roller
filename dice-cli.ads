-------------------------------------------------------------------------------
-- Dice.CLI (spec)
--
-- Defines methods necessary for checking parameters passed to DiceRoller when
-- the program was started.
--
-- Author: Thomas Løcke
-- Copyleft 2010. You may freely do with this source as you wish.
-------------------------------------------------------------------------------
package Dice.CLI is
   File_Error  : exception;
   --  A File_Error is raised when a dice roll configuration cannot be read by
   --  the program. Raising File_Error should:
   --    1: Inform the user about the missing file
   --    2: Terminate the program

   Help_Needed : exception;
   --  A Help_Needed exception is raised if -h/-H option is given when the
   --  program is invoked. Raising Help should:
   --    1: Output the help text
   --    2: Exit the program

   procedure Check_Passed_Parameters;
   --  This procedure checks for the -h commandline parameter. If -h is found,
   --  the Help_Needed exception is raised.
   --  If no -h parameter is found Check_Passed_Parameters assumes that any
   --  remaining parameters are files containing dice roll configurations,
   --  each of which is checked. If a file cannot be found, the File_Error
   --  exception is raised.
end Dice.CLI;
