-------------------------------------------------------------------------------
-- Dice.Action (spec)
--
-- Defines various actions to perform on a Dice_Rolls object. Stuff like List,
-- Count, Roll and so on.
--
-- Author: Thomas Løcke
-- Copyleft 2010. You may freely do with this source as you wish.
-------------------------------------------------------------------------------
package Dice.Action is
   function Count_Rolls (Some_Dice_Rolls : in Dice_Rolls) return Natural;
   --  Return the number of dice rolls loaded.
   --  EXCEPTIONS:
   --    none

   procedure List_Dice_Rolls (Some_Dice_Rolls   : in     Dice_Rolls;
                              A_WTD             : in out What_To_Do);
   --  Output the loaded dice rolls with numerical alias and name.
   --  EXCEPTIONS:
   --    none

   procedure Read_Input (A_WTD      : in out What_To_Do;
                         A_String   : in     String);
   --  Parse and interpret user-input.
   --  EXCEPTIONS:
   --    Constraint_Error

   procedure Roll_It (Some_Dice_Rolls  : in     Dice_Rolls;
                      A_WTD            : in out What_To_Do);
   --  Perform an actual dice roll and output the result.
   --  EXCEPTIONS:
   --    none
end Dice.Action;
