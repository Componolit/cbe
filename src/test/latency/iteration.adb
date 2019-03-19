
with Cai.Log.Client;
with Cai.Block;

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
                    Data      => (others => (0, Ada.Real_Time.Time_First, Ada.Real_Time.Time_First)));
   end Create;

   procedure Send (C : Cai.Block.Client_Session; T : in out Test) is
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
               Read_Request.Start := Cai.Block.Id (I + 1);
               Write_Request.Start := Cai.Block.Id (I + 1);
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
         T.Finished := True;
      end if;
   end Send;

   procedure Receive (C : Cai.Block.Client_Session; T : in out Test) is
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

begin
   Cai.Log.Client.Initialize (Log, "Test " & Cai.Log.Image (Integer (Request_Count)));

end Iteration;
