generic
   type Number_Type is mod <>;
   type Element_Type is mod <>;
   Rounds : Element_Type := 2;
   Key_Schedule : Element_Type := 16#11#;
   K0 : Element_Type := 2;
   K1 : Element_Type := 3;
   K2 : Element_Type := 5;
   K3 : Element_Type := 7;
package Permutation with
  SPARK_Mode
is
   pragma Compile_Time_Error (Number_Type'Size / 2 /= Number_Type (Element_Type'Size),
                              "Element_Type'Size must be half the size of Number_Type'Size");
   pragma Compile_Time_Error (Number_Type'Last /= 2**Number_Type'Size - 1,
                              "modulus of Number_Type must be a power of 2");
   pragma Compile_Time_Error (Element_Type'Last /= 2**Element_Type'Size - 1,
                              "modulus of Element_Type must be a power of 2");
   pragma Compile_Time_Error (Element_Type'Size <= 5,
                              "Element_Type'Size must be at least 6");

   function Has_Element return Boolean;
   procedure Next (Number : out Number_Type) with
     Pre => Has_Element;
   procedure Reset;

private

   type Data_Type is
      record
         L : Element_Type;
         R : Element_Type;
      end record;

   function Convert (Data : Data_Type) return Number_Type;
   function Convert (Number : Number_Type) return Data_Type;

   function Permute (Number : Number_Type) return Number_Type;
   function Inverse (Number : Number_Type) return Number_Type;

   State : Number_Type := Number_Type'First;
   Last_Reached : Boolean := False;

end Permutation;
