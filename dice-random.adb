-------------------------------------------------------------------------------
-- Dice.Random (body)
--
-- Random number generator. Generates the actual dice rolls.
--
-- Author: Thomas Løcke
-- Copyleft 2010. You may freely do with this source as you wish.
-------------------------------------------------------------------------------
with Ada.Numerics.Discrete_Random; use Ada.Numerics;

package body Dice.Random is
   ------------------
   --  Get_Random  --
   ------------------
   procedure Get_Random (A_Low         : in     Natural;
                         A_High        : in     Natural;
                         A_Dice_Amount : in     Natural;
                         A_Total       : in out Natural;
                         A_Result_List : in out Result_Container.Vector) is
      subtype My_Range is Integer range A_Low .. A_High;
      package My_Ran is new Discrete_Random (My_Range);
      Randomizer  : My_Ran.Generator;
      Num         : Natural;
   begin
      My_Ran.Reset (Gen => Randomizer);
      for i in 1 .. A_Dice_Amount loop
         Num := My_Ran.Random (Gen => Randomizer);
         A_Total := A_Total + Num;
         A_Result_List.Append (New_Item => Num);
      end loop;
   end Get_Random;
end Dice.Random;
