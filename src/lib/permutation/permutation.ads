generic
   type Output_Type is (<>);
   Rounds : Positive := 3;
   Key_Schedule : Natural := 16#11#;
   Key0 : Natural := 2;
   Key1 : Natural := 3;
   Key2 : Natural := 5;
   Key3 : Natural := 7;
package Permutation with
  SPARK_Mode
is
   pragma Compile_Time_Error (Output_Type'Size <= 11, "Output_Type'Size must be at least 12");

   procedure Initialize;
   function Has_Element return Boolean;
   procedure Next (Number : out Output_Type) with
     Pre => Has_Element;

private

   type U64 is mod 2**64;
   type U32 is mod 2**32;

   type Data_Type is
      record
         L : U32;
         R : U32;
      end record;

   SIZE : constant Natural := Output_Type'Size + Output_Type'Size mod 2;

   FIRST : constant U64 := 0;
   LAST  : constant U64 := 2**SIZE - 1;

   M : constant U32 := 2**(SIZE / 2);

   RNDS : constant U32 := U32 (Rounds);
   KS   : constant U32 := U32 (Key_Schedule);
   K0   : constant U32 := U32 (Key0);
   K1   : constant U32 := U32 (Key1);
   K2   : constant U32 := U32 (Key2);
   K3   : constant U32 := U32 (Key3);

   State        : U64 := FIRST;
   Next_Number  : U64 := 0;
   Next_Found   : Boolean := False;
   Last_Reached : Boolean := False;

   function Permute (Number : U64) return U64 with
     Pre => Number <= LAST;
   function Inverse (Number : U64) return U64 with
     Pre => Number <= LAST;

end Permutation;
