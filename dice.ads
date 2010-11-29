-------------------------------------------------------------------------------
-- Dice (spec)
--
-- Dice defines the core types and exceptions that are used throughout the
-- DiceRoller program.
--
-- Author: Thomas Løcke
-- Copyleft 2010. You may freely do with this source as you wish.
-------------------------------------------------------------------------------
with Ada.Containers.Vectors;  use Ada.Containers;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

package Dice is

   package Result_Container is new Vectors (Positive, Positive);
   package IIO is new Ada.Text_IO.Integer_IO (Integer);

   type Mode_Type is (Mode_Interactive, Mode_Standard);
   --  How we interact with the program. Mode_Interactive means that we have
   --  fewer than 10 roll definitions loaded and can execute the roll without
   --  waiting for the Enter key to be pressed. (We could use the order of
   --  magnitude of the roll table to decide whether a second or subsequent
   --  keystroke was needed. E.g., if we knew we loaded 25 roll definitions,
   --  we could accept either a second digit or the Enter key as the stroke
   --  that starts the rolling.)
   --  When using Mode_Standard, we have to follow up with a return to activate
   --  the entered dice roll.
   subtype Dice_Type_Range is Integer range 2 .. 20;
   --  The smallest die we allow is 2 sides, largest 20 sides.
   subtype Dice_Amount_Range is Integer range 1 .. 20;
   --  We can roll no fewer than 1 die, and no more than 20 dice.
   subtype Adjustment_Range is Integer range -20 .. 20;
   --  Biggest bonus is +20 and biggest penalty is -20.
   subtype Max_Amount_Of_Rolls is Integer range 1 .. 100;
   --  Max number of dice rolls we can configure in one or more dice roll
   --  configuration files.

   subtype String_256 is String (1 .. 256);
   --  We use the String_256 to read dice roll configuration files. Dice roll
   --  configurations above 256 characters are cut down to 256 characters, and
   --  then evaluated as any other dice roll.

   type What_To_Do is record
      Interface_Mode : Mode_Type;
      Roll_Number    : Integer range 0 .. Max_Amount_Of_Rolls'Last := 0;
      Do_Roll        : Boolean := False;
      Quit           : Boolean := False;
      List_Rolls     : Boolean := False;
   end record;
   --  When input is read, we either quit the program, list the rolls or roll
   --  some dice.
   --  if Do_Roll is Boolean True, we try to roll the dice roll in question.
   --  If Quit is Boolean True, we quit the program.
   --  If List_Rolls is Boolean True we output the dice rolls.

   type Dice_Rolls is tagged limited private;

private

   type Roll_Spec is tagged record
      Roll_Type   : Unbounded_String;
      Dice_Type   : Dice_Type_Range;
      Dice_Amount : Dice_Amount_Range;
      Adjustment  : Adjustment_Range := 0;
   end record;
   --  The Roll_Spec type defines a single dice roll. A Roll_Spec is build from
   --  the following string:
   --    (xxx)NdS+B  (The "+" may also be a "-".)
   --  Where:
   --    xxx is the Roll_Type. It is not limited to 3 characters, but
   --    please remember the 256-character line length limit.
   --    N is the Dice_Amount (the number of S-sided dice to roll)
   --    S is the Dice_Type (the number of sides on the die)
   --    +/-B is an Adjustment (a bonus or penalty to be applied to the total)
   --  Exaple: (dmg)2d4+2
   --    The roll is documented as a "dmg" ("damage") roll, calculated by
   --    rolling a 4-sided die twice (in physical play, two dice at once),
   --    and adding 2 to the result.

   package Roll_Spec_Container is new Vectors (Positive, Roll_Spec);
   type Roll is tagged record
      Name        : Unbounded_String;
      Is_Valid    : Boolean := True;
      Roll_Specs  : Roll_Spec_Container.Vector;
   end record;
   --  The Roll type collects a group of Roll_Spec's. The Name element is
   --  fetched from the dice roll configuration string:
   --    Magic Staff:(atk)1d20+2:(dmg)2d4+2
   --  Where:
   --    Magic Staff becomes Name. (This element and all string punctuation
   --    counts against the 256-character line length limit!)
   --  The remainder of the string is used to build one or more Roll_Spec
   --  records.

   package Dice_Rolls_Container is new Vectors (Max_Amount_Of_Rolls, Roll);
   type Dice_Rolls is tagged limited record
      Vector : Dice_Rolls_Container.Vector;
   end record;
   --  The "main" dice roll record. This record contains a vector of
   --  Parse_Dice_Roll.Roll records, which in turn contains one or more
   --  Parse_Dice_Roll.Roll_Spec records.
end Dice;
