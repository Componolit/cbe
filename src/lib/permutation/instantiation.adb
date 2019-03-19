with Permutation;

package body Instantiation with
  SPARK_Mode
is

   procedure Test is
      type Number is mod 2**32;
      type Element is mod 2**16;
      package Perm is new Permutation (Number, Element); use Perm;
      N : Number;
   begin
      while Has_Element loop
         Next (N);
         pragma Inspection_Point (N);
      end loop;
   end Test;

end Instantiation;
