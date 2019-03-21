
with Ada.Unchecked_Conversion;
with Cai.Block;
with Cai.Log.Client;

use all type Cai.Block.Request_Kind;

package body Iteration is

   Log : Cai.Log.Client_Session := Cai.Log.Client.Create;

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
   end Finish;

   function Create (Offset : Cai.Block.Count) return Test
   is
   begin
      return Test' (Sent      => -1,
                    Received  => -1,
                    Offset    => Offset,
                    Finished  => False,
                    Buffer    => (others => 0),
                    Data      => (others => (Ada.Real_Time.Time_First, Ada.Real_Time.Time_First)));
   end Create;

   procedure Send (C : in out Cai.Block.Client_Session; T : in out Test) is
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
      Current_Block : Long_Integer;
   begin
      if T.Sent < T.Data'Last then
         if Client.Initialized (C) then
            Current_Block := T.Sent + 1 + Long_Integer (T.Offset);
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
                  Cai.Log.Client.Info (Log, "Sent: " & (case Operation is
                                                         when Cai.Block.Read => "read ",
                                                         when Cai.Block.Write => "write ",
                                                         when others => "invalid ")
                                            & Cai.Log.Image (Current_Block)
                                            & " .. " & Cai.Log.Image (T.Sent + Long_Integer (T.Offset)));
                  exit;
               end if;
            end loop;
         else
            Cai.Log.Client.Error (Log, "Failed to run test, client not initialized");
         end if;
      end if;
      if T.Sent = T.Data'Last and T.Received = T.Data'Last then
         T.Finished := True;
      end if;
   end Send;

   procedure Receive (C : in out Cai.Block.Client_Session; T : in out Test) is
      Current_Block : Long_Integer;
   begin
      if Client.Initialized (C) then
         Current_Block := T.Received + 1 + Long_Integer (T.Offset);
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
                  Cai.Log.Client.Info (Log, "Received: " & (case Operation is
                                                            when Cai.Block.Read => "read ",
                                                            when Cai.Block.Write => "write ",
                                                            when others => "invalid ")
                                            & Cai.Log.Image (Current_Block)
                                            & " .. " & Cai.Log.Image (T.Received + Long_Integer (T.Offset)));
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
                                    & """/>");
   end Xml;

   procedure Xml (Xml_Log : in out Cai.Log.Client_Session; B : Burst; Offset : Cai.Block.Count)
   is
   begin
      for I in B'Range loop
         Xml (Xml_Log, B (I), Cai.Block.Id (I) + Offset);
      end loop;
   end Xml;

begin
   Cai.Log.Client.Initialize (Log, "Latency");

end Iteration;
