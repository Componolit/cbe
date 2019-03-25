
with Ada.Unchecked_Conversion;
with Cai.Block;
with Cai.Log.Client;

use all type Cai.Block.Request_Kind;
use all type Cai.Block.Request_Status;

package body Iteration is

   procedure Start (Item : Client.Request; Offset : Cai.Block.Count; Data : in out Burst) with
      Pre => Long_Integer (Item.Start - Offset) in Data'Range;

   procedure Finish (Item : Client.Request; Offset : Cai.Block.Count; Data : in out Burst) with
      Pre => Long_Integer (Item.Start - Offset) in Data'Range;


   procedure Start (Item : Client.Request; Offset : Cai.Block.Count; Data : in out Burst)
   is
   begin
      Data (Long_Integer (Item.Start - Offset)).Start := Ada.Real_Time.Clock;
   end Start;

   procedure Finish (Item : Client.Request; Offset : Cai.Block.Count; Data : in out Burst)
   is
   begin
      Data (Long_Integer (Item.Start - Offset)).Finish := Ada.Real_Time.Clock;
      Data (Long_Integer (Item.Start - Offset)).Success := Item.Status = Cai.Block.Ok;
   end Finish;

   procedure Initialize (T : out Test; Offset : Cai.Block.Count; Sync : Boolean)
   is
   begin
      T.Sent      := -1;
      T.Received  := -1;
      T.Offset    := Offset;
      T.Finished  := False;
      T.Sync      := Sync;
      T.Buffer    := (others => 0);
      for I in T.Data'Range loop
         T.Data (I) := (Success => False, others => Ada.Real_Time.Time_First);
      end loop;
   end Initialize;

   procedure Send (C : in out Cai.Block.Client_Session; T : in out Test; Log : in out Cai.Log.Client_Session) is
      Read_Request : Client.Request := (Kind => Cai.Block.Read,
                                        Priv => Cai.Block.Null_Data,
                                        Start => 0,
                                        Length => 1,
                                        Status => Cai.Block.Raw);
      Write_Request : Client.Request := (Kind => Cai.Block.Write,
                                         Priv => Cai.Block.Null_Data,
                                         Start => 0,
                                         Length => 1,
                                         Status => Cai.Block.Raw);
   begin
      if T.Sent < T.Data'Last then
         if Client.Initialized (C) then
            for I in T.Sent .. T.Data'Last - 1 loop
               Read_Request.Start := Cai.Block.Id (I + 1) + T.Offset;
               Write_Request.Start := Cai.Block.Id (I + 1) + T.Offset;
               if
                  Client.Ready (C, Write_Request)
                  and Client.Ready (C, Read_Request)
               then
                  case Operation is
                     when Cai.Block.Write =>
                        Start (Write_Request, T.Offset, T.Data);
                        Client.Enqueue_Write (C, Write_Request,
                                              T.Buffer (1 .. Cai.Block.Unsigned_Long (Client.Block_Size (C))));
                     when Cai.Block.Read =>
                        Start (Read_Request, T.Offset, T.Data);
                        Client.Enqueue_Read (C, Read_Request);
                     when others =>
                        null;
                  end case;
                  T.Sent := T.Sent + 1;
               else
                  Client.Submit (C);
                  exit;
               end if;
            end loop;
         else
            Cai.Log.Client.Error (Log, "Failed to run test, client not initialized");
         end if;
      end if;
      if T.Sent = T.Data'Last and T.Received = T.Data'Last then
         if Operation = Cai.Block.Write and T.Sync then
            declare
               S : constant Client.Request := (Kind => Cai.Block.Sync, Priv => Cai.Block.Null_Data);
            begin
               while not Client.Ready(C, S) loop
                  null;
               end loop;
               Client.Enqueue_Sync (C, S);
               Client.Submit (C);
            end;
         end if;
         T.Finished := True;
      end if;
   end Send;

   procedure Receive (C : in out Cai.Block.Client_Session; T : in out Test; Log : in out Cai.Log.Client_Session) is
   begin
      if Client.Initialized (C) then
         while T.Received < T.Data'Last loop
            declare
               R : Client.Request := Client.Next (C);
            begin
               if R.Kind = Operation then
                  if R.Kind = Cai.Block.Read then
                     Client.Read (C, R, T.Buffer (1 .. R.Length * Client.Block_Size (C)));
                  end if;
                  Finish (R, T.Offset, T.Data);
                  T.Received := T.Received + 1;
                  Client.Release (C, R);
               elsif R.Kind = Cai.Block.None then
                  exit;
               else
                  Cai.Log.Client.Warning (Log, "Received unexpected request");
               end if;
            end;
         end loop;
      else
         Cai.Log.Client.Error (Log, "Failed to run test, client not Initialized");
      end if;
      if T.Sent = T.Data'Last and T.Received = T.Data'Last then
         T.Finished := True;
      end if;
   end Receive;

   procedure Xml (Xml_Log : in out Cai.Log.Client_Session; R : Request; Block : Cai.Block.Id)
   is
      function Time_Conversion is new Ada.Unchecked_Conversion (Ada.Real_Time.Time, Duration);
   begin
      Cai.Log.Client.Info (Xml_Log, "<request id=""" & Cai.Log.Image (Long_Integer (Block))
                                    & """ sent=""" & Cai.Log.Image (Time_Conversion (R.Start))
                                    & """ received=""" & Cai.Log.Image (Time_Conversion (R.Finish))
                                    & """ status=""" & (if R.Success then "OK" else "ERROR")
                                    & """/>");
   end Xml;

   procedure Xml (Xml_Log : in out Cai.Log.Client_Session; B : Burst; Offset : Cai.Block.Count)
   is
   begin
      for I in B'Range loop
         Xml (Xml_Log, B (I), Cai.Block.Id (I) + Offset);
      end loop;
   end Xml;

end Iteration;
