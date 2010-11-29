-------------------------------------------------------------------------------
-- Dice.Action (body)
--
-- Defines various actions to perform on a Dice_Rolls object. Stuff like List,
-- Count, Roll and so on.
--
-- Author: Thomas Løcke
-- Copyleft 2010. You may freely do with this source as you wish.
-------------------------------------------------------------------------------
with Ada.Characters.Latin_1;     use Ada.Characters.Latin_1;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Text_IO.Unbounded_IO;   use Ada.Text_IO.Unbounded_IO;
with Dice.Random;                use Dice.Random;

package body Dice.Action is
   -------------------
   --  Count_Rolls  --
   -------------------
   function Count_Rolls (Some_Dice_Rolls : in Dice_Rolls) return Natural is
   begin
      return Natural (Some_Dice_Rolls.Vector.Length);
   end Count_Rolls;

   ------------
   --  List  --
   ------------
   procedure List_Dice_Rolls (Some_Dice_Rolls   : in Dice_Rolls;
                              A_WTD             : in out What_To_Do) is
      package IIO is new Integer_IO (Natural);
      SDR      : Dice_Rolls renames Some_Dice_Rolls;
      --  We rename Some_Dice_Rolls to lessen the typing burden.
      A_Roll   : Roll;
   begin
      New_Line;
      Put_Line (Item => "List of registered Dice Rolls:");
      Set_Col (To => 2);
      Put_Line (Item => "Enter " & Quotation & "l" & Quotation &
                " to list rolls.");
      Set_Col (To => 2);
      Put_Line (Item => "Enter " & Quotation & "q" & Quotation & " to quit");

      for i in SDR.Vector.First_Index .. SDR.Vector.Last_Index loop
         A_Roll := SDR.Vector.Element (Index => i);
         Set_Col (To => 2);
         Put (Item => "Enter " & Quotation);
         IIO.Put (Item  => i, Width => 1);
         Put (Item => Quotation & " to roll ");
         Put_Line (Item => A_Roll.Name);
      end loop;

      A_WTD.List_Rolls := False;
      --  Reset the List_Rolls component to Boolean False

      New_Line;
      Put (Item => "Enter your choice: ");
   end List_Dice_Rolls;

   ------------------
   --  Read_Input  --
   ------------------
   procedure Read_Input (A_WTD      : in out What_To_Do;
                         A_String   : in     String) is
   begin
      if To_Lower (A_String (A_String'First)) = 'q' then
         A_WTD.Quit := True;
      elsif To_Lower (A_String (A_String'First)) = 'l' then
         A_WTD.List_Rolls := True;
      else
         A_WTD.Roll_Number := Integer'Value (A_String);
         --  Try to convert user input to an Integer. A Constraint_Error is
         --  raised if this fails.
         A_WTD.Do_Roll := True;
      end if;

   exception
      when Constraint_Error =>
         A_WTD.Roll_Number := 0;
         A_WTD.Do_Roll := True;
         --  The user input, A_String, could not be converted to an Integer.
         --  We set the Roll_Number component to 0 and Do_Roll to Boolean True.
         --  The Roll_It procedure simply return a "unknown dice roll" on 0.
   end Read_Input;

   ---------------
   --  Roll_It  --
   ---------------
   procedure Roll_It (Some_Dice_Rolls  : in     Dice_Rolls;
                      A_WTD            : in out What_To_Do) is
      SDR      : Dice_Rolls renames Some_Dice_Rolls;
      A_Roll   : Roll;
   begin
      if A_WTD.Roll_Number <= SDR.Vector.Last_Index
        and A_WTD.Roll_Number >= SDR.Vector.First_Index then
         A_Roll := SDR.Vector.Element (Index => A_WTD.Roll_Number);
         New_Line (2);
         Put_Line (Item => "Rolling: " & A_Roll.Name);
         for i in
           A_Roll.Roll_Specs.First_Index .. A_Roll.Roll_Specs.Last_Index loop
            declare
               A_Spec      : constant Roll_Spec :=
                 A_Roll.Roll_Specs.Element (Index => i);
               --  We declare A_Spec a constant because we don't need to alter
               --  its value.
               Result_List : Result_Container.Vector;
               --  We keep track of the results in this vector.
               Total       : Natural := 0;
            begin
               Put (Item => A_Spec.Roll_Type & ": ");
               Get_Random (A_Low          => 1,
                           A_High         => A_Spec.Dice_Type,
                           A_Dice_Amount  => A_Spec.Dice_Amount,
                           A_Total        => Total,
                           A_Result_List  => Result_List);
               --  Roll the dice! All the results are appended to the
               --  Result_List vector.
               IIO.Put (Item  => Total + A_Spec.Adjustment,
                        Width => 3);
               --  Output the result of the roll.

               Set_Col (To => Col + 3);
               for i in Result_List.First_Index .. Result_List.Last_Index loop
                  IIO.Put (Item  => Result_List.Element (Index => i),
                           Width => 1);
                  if i < Result_List.Last_Index then
                     Put (Item => "+");
                  end if;
               end loop;
               --  In this loop we output the roll sequence, in case the roll
               --  requires more than one die.

               if A_Spec.Adjustment < 0 then
                  Put (Item => "(");
                  IIO.Put (Item  => A_Spec.Adjustment,
                           Width => 1);
                  Put (Item => ")");
                  --  Output the (-n) parentheses
               elsif A_Spec.Adjustment >= 0 then
                  Put (Item => "(+");
                  IIO.Put (Item  => A_Spec.Adjustment,
                           Width => 1);
                  Put (Item => ")");
                  --  Output the (+n) parentheses
               end if;
               New_Line;
            end;
         end loop;
      else
         if A_WTD.Interface_Mode = Mode_Interactive then
            New_Line (2); --  Output two newlines when using interactive mode.
         else
            New_Line; --  Output one newline when using standard mode.
         end if;
         Put_Line (Item => "Unknown dice roll. Try again.");
      end if;

      A_WTD.Do_Roll := False;
      --  We did what we set out to do, so now we reset the Do_Roll component
      --  to Boolean False.
      New_Line;
      Put (Item => "Enter your choice: ");
   end Roll_It;
end Dice.Action;
