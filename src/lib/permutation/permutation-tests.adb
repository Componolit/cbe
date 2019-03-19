with SPARK.Assertions; use SPARK.Assertions;

package body Permutation.Tests is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Permutation");
   end Name;

   procedure Test_Bijectivity (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      N : Number_Type;
   begin
      Reset;
      for I in Number_Type'First .. Number_Type'Last loop
         N := I;
         N := Permute (N);
         N := Inverse (N);
         if N /= Number_Type (I) then
            Assert (I'Img, N'Img, "Inversion failed");
         end if;
      end loop;
   end Test_Bijectivity;

   procedure Test_No_Duplicates (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      N : Number_Type;
      type Number_Array_Type is array (Number_Type) of Number_Type;
      N_Array : Number_Array_Type := (others => 0);
   begin
      Reset;
      for I in Number_Type'First .. Number_Type'Last loop
         Next (N);
         for J in Number_Type'First .. I - 1 loop
            Assert (N /= N_Array (J), "Duplicate");
         end loop;
         N_Array (I) := N;
      end loop;
      Assert (not Has_Element, "More elements");
      Reset;
      Assert (Has_Element, "No more elements");
   end Test_No_Duplicates;

   procedure Test_Determinism (T : in out Aunit.Test_Cases.Test_Case'Class) with
     SPARK_Mode, Global => null
   is
      pragma Unreferenced (T);
      N : Number_Type;
   begin
      Reset;
      if Number_Type'Size = 12 then
         Next (N);
         Assert (N'Img, Number_Type'Image (250), "Unexpected value");
         Next (N);
         Assert (N'Img, Number_Type'Image (173), "Unexpected value");
         Next (N);
         Assert (N'Img, Number_Type'Image (442), "Unexpected value");
      elsif Number_Type'Size = 14 then
         Next (N);
         Assert (N'Img, Number_Type'Image (442), "Unexpected value");
         Next (N);
         Assert (N'Img, Number_Type'Image (8559), "Unexpected value");
         Next (N);
         Assert (N'Img, Number_Type'Image (9302), "Unexpected value");
      end if;
   end Test_Determinism;

   procedure Anonymous_Register (T : in out Test;
                                 P : access procedure (T : in out Aunit.Test_Cases.Test_Case'Class);
                                 S : String) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, P, S);
   end Anonymous_Register;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Anonymous_Register (T, Test_Bijectivity'Access, "Bijectivity");
      Anonymous_Register (T, Test_No_Duplicates'Access, "No Duplicates");
      Anonymous_Register (T, Test_Determinism'Access, "Determinism");
   end Register_Tests;

end Permutation.Tests;
