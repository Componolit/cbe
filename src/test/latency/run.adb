
package body Run is

   function Create return Run_Type
   is
   begin
      return Run_Type' (others => Iter.Create (0));
   end Create;

   procedure Initialize (R : in out Run_Type)
   is
   begin
      for I in R'Range loop
         R (I).Offset := Cai.Block.Count ((I - 1) * R (I).Data'Length);
      end loop;
   end Initialize;

   function First (R : Run_Type) return Integer
   is
   begin
      for I in R'Range loop
         if R (I).Finished = False then
            return I;
         end if;
      end loop;
      return -1;
   end First;

   procedure Run (C : in out Cai.Block.Client_Session; R : in out Run_Type)
   is
      F : Integer := First (R);
   begin
      if F in R'Range then
         Iter.Receive (C, R (F));
         Iter.Send (C, R (F));
         if R (F).Finished and F + 1 in R'Range then
            Iter.Receive (C, R (F + 1));
            Iter.Send (C, R (F + 1));
         end if;
      end if;
   end Run;

   function Finished (R : Run_Type) return Boolean
   is
      Fin : Boolean := True;
   begin
      for T of R loop
         Fin := Fin and T.Finished;
      end loop;
      return Fin;
   end Finished;

end Run;
