
with Cai.Log.Client;

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
      F : constant Integer := First (R);
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

   procedure Xml (Xml_Log : in out Cai.Log.Client_Session; R : Run_Type)
   is
   begin
      Cai.Log.Client.Info (Xml_Log, "<run burst_size=""" & Cai.Log.Image (Long_Integer (R (R'First).Data'Length))
                                    & """ iterations=""" & Cai.Log.Image (Long_Integer (R'Length))
                                    & """ operation=""" & (case Operation is
                                                         when Cai.Block.None | Cai.Block.Sync => "INVALID",
                                                         when Cai.Block.Read => "READ",
                                                         when Cai.Block.Write => "WRITE")
                                    & """ transfer_size=""1""/>");
      for I in R'Range loop
         Cai.Log.Client.Info (Xml_Log, "<iteration num=""" & Cai.Log.Image (I) & """/>");
         Iter.Xml (Xml_Log, R (I).Data, R (I).Offset);
         Cai.Log.Client.Info (Xml_Log, "</iteration>");
      end loop;
      Cai.Log.Client.Info (Xml_Log, "</run>");
   end Xml;

end Run;
