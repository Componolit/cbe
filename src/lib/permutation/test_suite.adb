with Permutation.Tests;

package body Test_Suite is

   type N12 is mod 2**12;
   type E6 is mod 2**6;
   package Perm_12 is new Permutation (N12, E6);
   package Perm_12_Test is new Perm_12.Tests;

   type N14 is mod 2**14;
   type E7 is mod 2**7;
   package Perm_14 is new Permutation (N14, E7);
   package Perm_14_Test is new Perm_14.Tests;

   function Suite return Access_Test_Suite is
      Result : constant Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      Result.Add_Test (new Perm_12_Test.Test);
      Result.Add_Test (new Perm_14_Test.Test);
      return Result;
   end Suite;

end Test_Suite;
