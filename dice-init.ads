-------------------------------------------------------------------------------
-- Dice.Init (spec)
--
-- Load the dice rolls, either from dice roll configuration files or from
-- a default set of dice rolls.
--
-- Author: Thomas Løcke
-- Copyleft 2010. You may freely do with this source as you wish.
-------------------------------------------------------------------------------
package Dice.Init is
   Parse_Error : exception;
   --  A Parse_Error is raised when a dice roll configuration string does not
   --  adhere to the proper syntax. Raising Parse_Error should:
   --    1: Warn the user about the failing roll
   --    2: Continue processing the remaining rolls

   Too_Many_Rolls_Error : exception;
   --  A Too_Many_Rolls_Error is raised when an attempt is made to load more
   --  than Max_Amount_Of_Rolls dice rolls.
   --  Raising Too_Many_Rolls_Error should:
   --    1: Warn the user about the error.
   --    2: Exit the program.

   procedure Initialize (Some_Dice_Rolls : in out Dice_Rolls);
   --  Initialize populates the Dice_Rolls record according to the given
   --  dice roll configuration files.
   --  EXCEPTIONS:
   --    none

private

   procedure Load_Default_Dice (Some_Dice_Rolls : in out Dice_Rolls);
   --  Load an arbitrary set of default dice rolls when no configuration
   --  files are provided.
   --  EXCEPTIONS:
   --    Too_Many_Rolls_Error

   procedure Parse (A_Roll             : in out Roll;
                    Roll_Config_String : in     String_256);
   --  Parse is responsible for building the Roll and Roll_Spec records from
   --  the dice roll configuration strings. See dice.ads for the format.
   --  A configuration string is parsed and used to build a Roll record
   --  consisting of one or more Roll_Spec records.
   --  EXCEPTIONS:
   --    Parse_Error

   procedure Read_Into_List (File_Name       : in     String;
                             Some_Dice_Rolls : in out Dice_Rolls);
   --  If a given dice roll configuration file exists (File_Name), this
   --  procedure reads its contents and dispatches each dice roll string to the
   --  Parse procedure. If a roll is parsed successfully, then the roll is
   --  added to the Some_Dice_Rolls.Vector.
   --  EXCEPTIONS:
   --    Too_Many_Rolls_Error

   procedure Set_Name (A_Roll             : in out Roll;
                       Roll_Config_String : in out Unbounded_String);
   --  Set the A_Roll.Name element based on the dice roll configuration
   --  string Roll_Config_String.
   --  EXCEPTIONS:
   --    Parse_Error

   procedure Set_Spec (A_Roll_Spec        : in out Roll_Spec;
                       A_Roll             : in out Roll;
                       Roll_Config_String : in out Unbounded_String;
                       Split_Index        : in     Natural);
   --  Build the Roll_Spec record A_Spec and add it to the A_Roll.Roll_Specs
   --  vector based on the dice roll configuration string
   --  Roll_Config_String.
   --  EXCEPTIONS:
   --    Parse_Error
end Dice.Init;
