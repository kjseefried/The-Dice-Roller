-------------------------------------------------------------------------------
-- Dice.Help (body)
--
-- Output the DiceRoller help text.
--
-- Author: Thomas Løcke
-- Copyleft 2010. You may freely do with this source as you wish.
-------------------------------------------------------------------------------
package body Dice.Help is
   procedure Print_Help is
   begin
      New_Line;
      Put_Line (Item => "DiceRoller v. 1.0.0");
      New_Line (1);
      Put_Line (Item => "usage: diceroller [options] " &
                "[dice roll configuration file(s)]");
      New_Line (1);
      Put_Line (Item => "Options:");
      Set_Col (To => 5);
      Put_Line (Item => "-h | -H : Output this help");
      Set_Col (To => 1);
      New_Line;
      Put_Line (Item => "Usage example:");
      Set_Col (To => 5);
      Put_Line (Item => "./diceroller rolls_1 rolls_2");
      New_Line;
      Put_Line (Item => "The above example will start the DiceRoller program");
      Put_Line (Item => "and attempt to load the files rolls_1 and rolls2.");
      Put_Line (Item => "You must provide at least one dice roll");
      Put_Line (Item => "configuration file.");
      New_Line;
      Put_Line (Item => "The syntax to describe a roll looks like this:");
      New_Line;
      Set_Col (To => 5);
      Put_Line (Item => "Battleaxe:(atk)1d20:(dmg)1d8");
      New_Line;
      Set_Col (To => 1);
      Put_Line (Item => "Where 'Battleaxe' is the name of the roll, a");
      Put_Line (Item => "20-sided die is rolled once for the (atk) label and");
      Put_Line (Item => "an 8-sided die is rolled once for the (dmg) label.");
      Put_Line ("You can add a bonus or a penalty to a roll like this:");
      New_Line;
      Set_Col (To => 5);
      Put_Line (Item => "Battleaxe:(atk)1d20+2:(dmg)1d8+2");
      New_Line;
      Set_Col (To => 1);
      Put_Line (Item => "In which case 1d20+2 and 1d8+2 are rolled.");
      Put_Line (Item => "You're not limited to (atk) and (dmg) labels. You");
      Put_Line (Item => "make up your own labels if you wish, e.g.:");
      New_Line;
      Set_Col (To => 5);
      Put_Line (Item => "Sword:(normal)2d12+3:(frenzy)4d8+2:(dazed)1d20");
      New_Line;
      Set_Col (To => 1);
      Put_Line (Item => "The name of the roll and the names of the labels");
      Put_Line (Item => "can be set to whatever you like, though the labels");
      Put_Line (Item =>  "MUST be enclosed in ().");
      New_Line;
      Put_Line (Item => "The following limits are imposed on dice roll");
      Put_Line (Item => "configurations:");
      Set_Col (To => 5);
      Put_Line (Item => "A maximum of 100 configured dice rolls allowed.");
      Set_Col (To => 5);
      Put_Line (Item => "Allowed dice range from 2 to 20 sides.");
      Set_Col (To => 5);
      Put_Line (Item => "Maximum number of dice rolled is 20.");
      Set_Col (To => 5);
      Put_Line (Item => "Bonus/penalty range is -20 to +20.");
      Set_Col (To => 5);
      Put_Line (Item => "Maximum 256 characters are allowed to describe ");
      Set_Col (To => 5);
      Put_Line (Item => "a dice roll. Excess characters are ignored.");
      New_Line;
      Set_Col (To => 1);
      Put_Line (Item => "If no dice configuration files are passed to the");
      Put_Line (Item => "program when invoked, a default set of dice is");
      Put_Line (Item => "loaded.");
      New_Line;
      Put_Line (Item => "If fewer than 11 dice rolls are loaded, the program");
      Put_Line (Item => "goes into interactive mode, which basically means");
      Put_Line (Item => "you don't have to press enter when selecting a");
      Put_Line (Item => "roll.");
      Put_Line (Item => "If there are more than 10 dice rolls loaded, the");
      Put_Line (Item => "program goes into standard mode, which means you");
      Put_Line (Item => "will have to press enter to activate a roll.");
      New_Line;
      Put_Line (Item => "The dice roll list is printed by pressing 'l'.");
      Put_Line (Item => "The program is stopped by pressing 'q'.");
      New_Line;
   end Print_Help;
end Dice.Help;
