-------------------------------------------------------------------------------
-- Dice.Random (spec)
--
-- Random number generator. Generates the actual dice rolls.
--
-- Author: Thomas Løcke
-- Copyleft 2010. You may freely do with this source as you wish.
-------------------------------------------------------------------------------
package Dice.Random is
   procedure Get_Random (A_Low         : in     Natural;
                         A_High        : in     Natural;
                         A_Dice_Amount : in     Natural;
                         A_Total       : in out Natural;
                         A_Result_List : in out Result_Container.Vector);
   --  Get_Random builds a list of dice roll results and appends them to the
   --  Result_Container.Vector.
   --  The results are random numbers in the range A_Low .. A_High.
   --  An A_Dice_Amount of results are calculated, along with the A_Total,
   --  which is the sum of all the individual dice rolls.
   --  EXCEPTIONS:
   --    none
end Dice.Random;
