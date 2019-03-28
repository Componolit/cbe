package body Permutation with
  SPARK_Mode
is

   function Map (O : Output_Type) return U64 is
     (U64 (Output_Type'Pos (O) - Output_Type'Pos (Output_Type'First)));

   function In_Range (N : U64) return Boolean is
     (N >= Map (Output_Type'First) and then N <= Map (Output_Type'Last));

   function Map (N : U64) return Output_Type is
     (Output_Type'Val (U64'Pos (N) + Output_Type'Pos (Output_Type'First)))
     with
       Pre => In_Range (N);

   function Has_Element return Boolean is
     (Next_Found and then In_Range (Next_Number));

   procedure Find_Next is
      N : U64;
   begin
      if State < FIRST or State > LAST or Last_Reached then
         Next_Found := False;
         return;
      end if;

      loop
         pragma Loop_Invariant (State >= FIRST and then State <= LAST);
         N := Permute (State);

         if State = LAST then
            Last_Reached := True;
            exit;
         end if;

         State := State + 1;
         exit when In_Range (N);
      end loop;

      if In_Range (N) then
         Next_Number := N;
         Next_Found := True;
      else
         Next_Found := False;
      end if;
   end Find_Next;

   procedure Initialize is
   begin
      State := U64'First;
      Last_Reached := False;
      Find_Next;
   end Initialize;

   procedure Next (Number : out Output_Type) is
      N : U64 := Next_Number;
   begin
      Find_Next;
      Number := Map (N);
   end Next;

   function Convert (Data : Data_Type) return U64 is
     (U64 (Data.L) * U64 (M) + U64 (Data.R));

   function Convert (Number : U64) return Data_Type is
     (U32 (Number / U64 (M)), U32 (Number and U64 (M - 1)))
     with
       Pre => Number <= LAST;

   function Permute (Number : U64) return U64 is
      Data : Data_Type := Convert (Number);
      L    : U32 := Data.L;
      R    : U32 := Data.R;
      Sum  : U32 := 0;
   begin
      for I in 1 .. RNDS loop
         Sum := Sum + KS mod M;
         L := (L + (((((R * 2**4) mod M) + K0) mod M) xor ((R + Sum) mod M) xor ((R / 2**5) mod M + K1) mod M) mod M) mod M;
         R := (R + (((((L * 2**4) mod M) + K2) mod M) xor ((L + Sum) mod M) xor ((L / 2**5) mod M + K3) mod M) mod M) mod M;
      end loop;

      return Convert ((L, R));
   end Permute;

   function Inverse (Number : U64) return U64 is
      Data : Data_Type := Convert (Number);
      L    : U32 := Data.L;
      R    : U32 := Data.R;
      Sum  : U32 := RNDS * KS;
   begin
      for I in 1 .. RNDS loop
         R := (R - (((((L * 2**4) mod M) + K2) mod M) xor ((L + Sum) mod M) xor (((L / 2**5) mod M + K3) mod M)) mod M) mod M;
         L := (L - (((((R * 2**4) mod M) + K0) mod M) xor ((R + Sum) mod M) xor (((R / 2**5) mod M + K1) mod M)) mod M) mod M;
         Sum := Sum - KS mod M;
      end loop;

      return Convert ((L, R));
   end Inverse;

end Permutation;
