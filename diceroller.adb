-------------------------------------------------------------------------------
-- DiceRoller (MAIN)
--
-- The DiceRoller rolls dice. It can either roll dice set up from one or more
-- dice configuration files, or it can roll dice from a default setup, which
-- is loaded if there are no dice configuration files.
--
-- Author: Thomas Løcke
-- Copyleft 2010. You may freely do with this source as you wish.
-------------------------------------------------------------------------------
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Dice;           use Dice;
with Dice.Action;    use Dice.Action;
with Dice.CLI;       use Dice.CLI;
with Dice.Help;      use Dice.Help;
with Dice.Init;      use Dice.Init;

procedure DiceRoller is
   My_Dice_Rolls : Dice_Rolls;
   --  Declare My_Dice_Rolls as a Dice_Roll type. See dice.ads.
   WTD : What_To_Do;
   --  Declare WTD as a What_To_Do type. See dice.ads.
begin
   Check_Passed_Parameters;
   --  Check the commandline parameters. See dice-cli.ads

   Initialize (My_Dice_Rolls);
   --  Load dice rolls into the My_Dice_Rolls object. See dice-init.ads

   New_Line;
   Put_Line (Item => "DiceRoller is ready to serve!");
   New_Line;

   if Count_Rolls (Some_Dice_Rolls => My_Dice_Rolls) < 10 then
      WTD.Interface_Mode := Mode_Interactive;
      Put_Line (Item => "DiceRoller is running in interactive mode.");
   else
      WTD.Interface_Mode := Mode_Standard;
      Put_Line (Item => "DiceRoller is running in standard mode.");
   end if;
   --  Set the WTD object according to the amount of dice rolls available.

   List_Dice_Rolls (Some_Dice_Rolls => My_Dice_Rolls,
                    A_WTD           => WTD);
   --  Here we list the dice rolls available in My_Dice_Rolls.
   --  See dice-action.ads for more info about List_Dice_Rolls.

   loop
      declare
         function User_Command return String;
         --  This function grabs the user input. The method used depends on
         --  the mode the program is running in. If the Interface_Mode is
         --  Mode_Interactice, the user input is grabbed using Get_Immediate,
         --  else we just use regular Get_Line.

         function User_Command return String is
         begin
            if WTD.Interface_Mode = Mode_Interactive then
               declare
                  Buffer : String (1 .. 1);
               begin
                  Get_Immediate (Item => Buffer (1));
                  --  Wait here for the user to press a key. When a key is
                  --  pressed, return it as a String. Get_Immediate reads the
                  --  next character immediately. The user does not need to
                  --  press enter to "send" the input.

                  return Buffer;
               end;
            else
               return Get_Line;
               --  Note that we don't use a specifically declared buffer to
               --  hold the user input. Instead we rely on the Get_Line
               --  function, which simply return the user input as a string.
               --  This approach requires the user to terminate her input by
               --  pressing "enter".
            end if;
         end User_Command;
      begin
         Read_Input (A_WTD    => WTD,
                     A_String => User_Command);
         --  Pass WTD and the return value from User_Command to Read_Input. The
         --  Read_Input procedure parse A_String and set the components of WTD
         --  accordingly. See dice-action.ads

         if WTD.List_Rolls then
            --  The user entered either 'l' or 'L', so we list the dice
            --  rolls again.
            List_Dice_Rolls (Some_Dice_Rolls => My_Dice_Rolls,
                             A_WTD           => WTD);
         end if;

         if WTD.Quit then
            --  The user entered either 'q' or 'Q', so we quit the program,
            --  which in this case simply means exit the loop and fall to
            --  the bottom of the program. We can get away with this
            --  because nothing happens between the final 'end if;' and
            --  the 'exception' statement.
            New_Line (2);
            Put_Line (Item => "DiceRoller exiting.");
            New_Line;
            exit;
         end if;

         if WTD.Do_Roll then
            --  A dice roll is requested, so we hand it over to the
            --  Roll_It procedure. Remember that the roll number is stored
            --  in the WTD object (see dice.ads).
            --  My_Dice_Rolls is also passed to Roll_It, because it contains
            --  the actual data about the roll(s). At this point it is not
            --  yet decided whether the requested roll is actually valid,
            --  ie. it exists in the My_Dice_Rolls list of dice rolls. This
            --  is decided in the Roll_It procedure by checking if the
            --  WTD.Roll_Number exists in the My_Dice_Rolls.Vector.
            Roll_It (Some_Dice_Rolls   => My_Dice_Rolls,
                     A_WTD             => WTD);
         end if;
      end;
   end loop;

exception
   when Event : File_Error =>
      Put (Item => "ERROR: Cannot load configuration file: ");
      Put_Line (Item => Exception_Message (X => Event));
      --  We cannot load one or more of the given dice roll configuration
      --  files. Output an error message and exit. See dice-cli.ads
   when Help_Needed =>
      Print_Help;
      --  -h or -H found as one of the commandline parameters. Output the help
      --  text and exit the program. See dice-help.ads
   when Too_Many_Rolls_Error =>
      Put_Line (Item => "ERROR: Too many dice rolls configured.");
      Put_Line (Item => "Maximum amount allowed:" &
                Max_Amount_Of_Rolls'Last'Img);
end DiceRoller;
