-------------------------------------------------------------------------------
-- Dice.Init (body)
--
-- Load the dice rolls, either from dice roll configuration files or from
-- a default set of dice rolls.
--
-- Author: Thomas Løcke
-- Copyleft 2010. You may freely do with this source as you wish.
-------------------------------------------------------------------------------
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Dice.Init is
   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize (Some_Dice_Rolls : in out Dice_Rolls) is
   begin
      if Argument_Count > 0 then
         for i in 1 .. Argument_Count loop
            Read_Into_List (File_Name        => Argument (Number => i),
                            Some_Dice_Rolls  => Some_Dice_Rolls);
            --  We will read as many files as are given in the CLI,
            --  as long as the filenames are valid. If the list gets too long,
            --  then the reader will raise an exception.
         end loop;
      else
         Load_Default_Dice (Some_Dice_Rolls => Some_Dice_Rolls);
         --  We have no dice roll configuration files, so lets load a few
         --  default dice.
      end if;
   end Initialize;

   -------------------------
   --  Load_Default_Dice  --
   -------------------------
   procedure Load_Default_Dice (Some_Dice_Rolls : in out Dice_Rolls) is
      Roll_Array : array (1 .. 6) of String_256;
   begin
      Move (Source => "1d4:(roll)1d4",
            Target => Roll_Array (1),
            Drop   => Right);
      Move (Source => "1d6:(roll)1d6",
            Target => Roll_Array (2),
            Drop   => Right);
      Move (Source => "1d8:(roll)1d8",
            Target => Roll_Array (3),
            Drop   => Right);
      Move (Source => "1d10:(roll)1d10",
            Target => Roll_Array (4),
            Drop   => Right);
      Move (Source => "1d12:(roll)1d12",
            Target => Roll_Array (5),
            Drop   => Right);
      Move (Source => "1d20:(roll)1d20",
            Target => Roll_Array (6),
            Drop   => Right);

      for i in Roll_Array'Range loop
         declare
            A_Roll : Roll;
         begin
            Parse (A_Roll              => A_Roll,
                   Roll_Config_String  => Roll_Array (i));
            if A_Roll.Is_Valid then
               Some_Dice_Rolls.Vector.Append (New_Item => A_Roll);
            end if;
         end;
      end loop;

   exception
      when Constraint_Error =>
         raise Too_Many_Rolls_Error;
   end Load_Default_Dice;

   -------------
   --  Parse  --
   -------------
   procedure Parse (A_Roll             : in out Roll;
                    Roll_Config_String : in     String_256) is
      A_Spec            : Roll_Spec;
      Raw_Roll_String   : Unbounded_String;
      Split_Index       : Natural;
   begin
      Raw_Roll_String := To_Unbounded_String (Source => Roll_Config_String);
      Trim (Source => Raw_Roll_String, Side   => Both);
      --  Make the Roll_Config_String into an Unbounded_String and Trim it.
      --  We do this because there are some nice tools available for parsing
      --  Unbounded Strings.

      Set_Name (A_Roll             => A_Roll,
                Roll_Config_String => Raw_Roll_String);
      --  Lets see if we can grab a valid name from the Raw_Roll_String. Note
      --  that if we successfully parse a roll name, then that part of the
      --  Raw_Roll_String is deleted, so if Raw_Roll_String looks like this
      --  before the Set_Name call
      --    Magic Staff:(atk)1d20+2:(dmg)2d4+2
      --  then it will look like this after Set_Name
      --    (atk)1d20+2:(dmg)2d4+2

      loop
         --  We loop until the Raw_Roll_String is empty.

         exit when Length (Source => Raw_Roll_String) = 0;
         --  There's no more Raw_Roll_String left. We exit the loop.

         Split_Index := Index (Source  => Raw_Roll_String,
                               Pattern => ":");
         --  Figure out the position of the first : in the dice roll
         --  configuration string.
         if Split_Index > 0 then
            --  We have found a : in the dice roll configuration. This means
            --  that it appears that we have at least two dice roll specs, ie.
            --  something like this:  (atk)1d20+2:(dmg)2d4+2.
            --  Next we try to parse the first roll spec and add it to the
            --  Roll_Specs vector using the Set_Spec procedure.
            Set_Spec (A_Roll_Spec         => A_Spec,
                      A_Roll              => A_Roll,
                      Roll_Config_String  => Raw_Roll_String,
                      Split_Index         => Split_Index - 1);
            Delete (Source  => Raw_Roll_String,
                    From    => 1,
                    Through => 1);
            --  Delete the ":"
         else
            Split_Index := Length (Source => Raw_Roll_String);
            --  This is the last dice roll spec, so we set the Split_Index to
            --  the length of the remaining Raw_Roll_String.
            Set_Spec (A_Roll_Spec         => A_Spec,
                      A_Roll              => A_Roll,
                      Roll_Config_String  => Raw_Roll_String,
                      Split_Index         => Split_Index);
         end if;
      end loop;

   exception
      when Parse_Error =>
         A_Roll.Is_Valid := False;
         Put (Item => "WARNING: Bad syntax for roll: ");
         Put_Line (Item => Trim (Source => Roll_Config_String, Side => Both));
   end Parse;

   ----------------------
   --  Read_Into_List  --
   ----------------------
   procedure Read_Into_List (File_Name       : in     String;
                             Some_Dice_Rolls : in out Dice_Rolls) is
      Input : File_Type;
   begin
      Open (File => Input,
            Mode => In_File,
            Name => File_Name);
      while not End_Of_File (File => Input) loop
         declare
            A_Roll   : Roll;
            Raw_Roll : String_256;
         begin
            Move (Source => Trim (Source => Get_Line (Input), Side => Both),
                  Target => Raw_Roll,
                  Drop   => Right);
            --  Read a dice roll configuration from the file, and place it in
            --  Raw_Roll. The Move procedure automatically discards characters
            --  if the string is longer than 256 characters and it does so
            --  according to the Drop parameter, which in our case is set to
            --  Right.
            Parse (A_Roll              => A_Roll,
                   Roll_Config_String  => Raw_Roll);
            if A_Roll.Is_Valid then
               Some_Dice_Rolls.Vector.Append (New_Item => A_Roll);
            end if;
         end;
      end loop;
      Close (File => Input);

   exception
      when Constraint_Error =>
         raise Too_Many_Rolls_Error;
   end Read_Into_List;

   ----------------
   --  Set_Name  --
   ----------------
   procedure Set_Name (A_Roll             : in out Roll;
                       Roll_Config_String : in out Unbounded_String) is
      First_Colon_Index : Natural;
   begin
      First_Colon_Index := Index (Source  => Roll_Config_String,
                                  Pattern => ":");
      if First_Colon_Index < 2 then
         raise Parse_Error;
         --  We found the first : at position 1 in the Roll_Config_String. This
         --  is not acceptable, so we raise the Parse_Error, which is caught
         --  in the Parse procedure.
      end if;

      A_Roll.Name := Unbounded_Slice (Source => Roll_Config_String,
                                      Low    => 1,
                                      High   => First_Colon_Index - 1);
      Trim (Source => A_Roll.Name, Side => Right);
      --  Add the roll name to the Name component of the A_Roll record, and
      --  then trim it for excessive whitespace.
      Delete (Source  => Roll_Config_String,
              From    => 1,
              Through => First_Colon_Index);
      --  Delete the name part and the first : from the Roll_Config_String.
   end Set_Name;

   ----------------
   --  Set_Spec  --
   ----------------
   procedure Set_Spec (A_Roll_Spec        : in out Roll_Spec;
                       A_Roll             : in out Roll;
                       Roll_Config_String : in out Unbounded_String;
                       Split_Index        : in     Natural) is
      Raw_Spec          : Unbounded_String;
      Open_Parentheses  : Natural; --  Position of the first (
      Close_Parentheses : Natural; --  Position of the first )
      Search_Index      : Natural;
   begin
      Raw_Spec := Unbounded_Slice (Source => Roll_Config_String,
                                   Low    => 1,
                                   High   => Split_Index);
      Trim (Source => Raw_Spec, Side => Both);
      --  Grab the first dice roll specification and trim it for excessive
      --  whitespace.

      Open_Parentheses := Index (Source   => Raw_Spec,
                                 Pattern  => "(",
                                 Going    => Backward);
      Close_Parentheses := Index (Source  => Raw_Spec,
                                  Pattern => ")",
                                  Going   => Backward);
      --  Find the positions of the first "(" and ")"

      if Open_Parentheses /= 1 or Close_Parentheses < 3 then
         raise Parse_Error;
         --  Invalid parentheses found. The raised Parse_Error is caught in
         --  the Parse procedure.
      end if;

      A_Roll_Spec.Roll_Type :=
        Unbounded_Slice (Source => Raw_Spec,
                         Low    => 2,
                         High   => Close_Parentheses - 1);
      Trim (Source => A_Roll_Spec.Roll_Type, Side => Both);
      if Length (Source => A_Roll_Spec.Roll_Type) < 3 then
         raise Parse_Error;
         --  The found Roll_Type is too short. It must be at least 3 characters
         --  long.
      end if;

      Delete (Source  => Raw_Spec,
              From    => 1,
              Through => Close_Parentheses);
      --  Delete the Roll_Type part of the dice roll specification.

      Search_Index := Index (Source  => Raw_Spec,
                             Pattern => "d",
                             Going   => Backward);
      --  Find the position of the first d, as in eg. 1d20
      if Search_Index < 2 then
         raise Parse_Error;
         --  Invalid position. We raise a Parse_Error.
      end if;

      A_Roll_Spec.Dice_Amount := Dice_Amount_Range'Value
        (Slice (Source => Raw_Spec,
                Low    => 1,
                High   => Search_Index - 1));
      --  Grab the first number before the d, eg. 1 for 1d20, and cast it as
      --  a Dice_Amount_Range type (Integer range 2 .. 20). If the number found
      --  breaks the constriant of Dice_Amount_Range, a Constraint_Error is
      --  Raised, which we catch at the end of this procedure.

      Delete (Source  => Raw_Spec,
              From    => 1,
              Through => Search_Index);
      --  Delete the found number from the dice roll specification.

      Search_Index := Index (Source  => Raw_Spec,
                             Pattern => "+");
      --  Search for a + character. If one is found, then we have a positive
      --  adjustment to the dice roll, eg. 1d20+4

      if Search_Index /= 0 then
         --  A positive adjustment found!

         A_Roll_Spec.Dice_Type := Dice_Type_Range'Value
           (Slice (Source => Raw_Spec,
                   Low    => 1,
                   High   => Search_Index - 1));
         --  Grab the kind of dice to be rolled, eg. 20 dice for a 3d20+2
         --  dice roll. We cast the found value as a Dice_Type_Range type,
         --  and that fails, a Constraint_Error exception is raised.

         A_Roll_Spec.Adjustment := Adjustment_Range'Value
           (Slice (Source => Raw_Spec,
                   Low    => Search_Index + 1,
                   High   => Length (Source => Raw_Spec)));
         --  Grab the positive adjustment to the roll, eg. 2 for 1d20+2, and
         --  cast it as a Adjustment_Range type. As usual, the Constraint_Error
         --  exception is raised if we break the constraints of the type.
      else
         --  No positive adjustment found. Perhaps we have a negative?

         Search_Index := Index (Source  => Raw_Spec,
                                Pattern => "-");
         --  Search for the - character. If one is found, then we have a
         --  negative adjustment to the dice roll, eg. 2d20-5

         if Search_Index /= 0 then
            --  A negative adjustment found!

            A_Roll_Spec.Dice_Type := Dice_Type_Range'Value
              (Slice (Source => Raw_Spec,
                      Low    => 1,
                      High   => Search_Index - 1));
            A_Roll_Spec.Adjustment := Adjustment_Range'Value
           (Slice (Source => Raw_Spec,
                   Low    => Search_Index,
                   High   => Length (Source => Raw_Spec)));
            --  The same mechanics as for the positive adjustment above.
         else
            A_Roll_Spec.Dice_Type := Dice_Type_Range'Value
              (To_String (Source => Raw_Spec));
            --  No adjustments found. The last number must be the type of
            --  dice for the roll, eg. 8 for 2d8
         end if;
      end if;

      A_Roll.Roll_Specs.Append (New_Item => A_Roll_Spec);
      --  We now have a complete A_Roll_Spec record. Lets append it to the
      --  Roll_Specs vector.

      Delete (Source  => Roll_Config_String,
              From    => 1,
              Through => Split_Index);
      --  Finally we delete the roll from the Roll_Config_String.

   exception
      when Constraint_Error =>
         raise Parse_Error;
   end Set_Spec;
end Dice.Init;
