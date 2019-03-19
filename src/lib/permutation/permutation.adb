package body Permutation with
  SPARK_Mode
is

   function Has_Element return Boolean is
      (not Last_Reached);

   procedure Next (Number : out Number_Type) is
   begin
      Number := Permute (State);
      if State = Number_Type'Last then
         Last_Reached := True;
      else
         State := State + 1;
      end if;
   end Next;

   procedure Reset is
   begin
      State := Number_Type'First;
      Last_Reached := False;
   end Reset;

   function Permute (Number : Number_Type) return Number_Type
   is
      Data : Data_Type := Convert (Number);
      Sum  : Element_Type := Element_Type (0);
      L    : Element_Type := Data.L;
      R    : Element_Type := Data.R;
   begin
      for I in 1 .. Rounds loop
         Sum := Sum + Key_Schedule;
         L := L + (((R * 2**4) + K0) xor (R + Sum) xor ((R / 2**5) + K1));
         R := R + (((L * 2**4) + K2) xor (L + Sum) xor ((L / 2**5) + K3));
      end loop;

      return Convert((L, R));
   end Permute;

   function Inverse (Number : Number_Type) return Number_Type
   is
      Data : Data_Type := Convert (Number);
      Sum  : Element_Type := Rounds * Key_Schedule;
      L    : Element_Type := Data.L;
      R    : Element_Type := Data.R;
   begin
      for I in 1 .. Rounds loop
         R := R - (((L * 2**4) + K2) xor (L + Sum) xor ((L / 2**5) + K3));
         L := L - (((R * 2**4) + K0) xor (R + Sum) xor ((R / 2**5) + K1));
         Sum := Sum - Key_Schedule;
      end loop;

      return Convert((L, R));
   end Inverse;

   function Convert (Data : Data_Type) return Number_Type is
     (Number_Type (Data.L) * Number_Type (2**Element_Type'Size) + Number_Type (Data.R));

   function Convert (Number : Number_Type) return Data_Type is
     (Element_Type (Number / 2**Element_Type'Size),
      Element_Type (Number and Number_Type (Element_Type'Last)));

end Permutation;
