
with Ada.Real_Time;
with Cai.Log.Client;

use all type Ada.Real_Time.Time;

package body Test is

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

   function Create (Count : Cai.Block.Count; Offset : Cai.Block.Count) return Test
   is
   begin
      return Test' (Sent      => -1,
                    Received  => -1,
                    Count     => Count,
                    Offset    => Offset,
                    Finished  => False,
                    Buffer    => (others => 0),
                    Data      => (others => (0, Ada.Real_Time.Time_First, Ada.Real_Time.Time_First)));
   end Create;

   procedure Send (C : Cai.Block.Client_Session, T : Test) is
      Request : Client.Request := (Kind => Operation,
                                   Priv => Cai.Block.Null_Data,
                                   Start => 0,
                                   Length => 1,
                                   Status => Cai.Block.Raw);
   begin
      if T.Sent < T.Data'Last then
         if Client.Initialized (C) then
            for I in T.Sent .. T.Data'Last - 1 loop
               Request.Start := Cai.Block.Id (I + 1);
               if Client.Ready (C, Request) then
                  Start (Request, T.Offset, T.Data);
                  case Request.Kind is
                     when Cai.Block.Write =>
                        Client.Enqueue_Write (C, Request,
                                              T.Buffer (1 .. Cai.Block.Unsigned_Long (Client.Block_Size (C))));
                     when Cai.Block.Read =>
                        Client.Enqueue_Read (C, Request);
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
      if Sent = Data'Last and Received = Data'Last then
         T.Finished = True;
      end if;
   end Send;

   procedure Receive (C : Cai.Block.Client_Session; T : Test) is
   begin
      if Client.Initialized (C) then
         while T.Received < T.Data'Last loop
            declare
               R : Client.Request := Client.Next (C);
            begin
               case R.Kind is
                  when T.Operation =>
                     Finish (R, T.Offset, T.Data);
                     T.Received := T.Received + 1;
                     Client.Release (C, R);
                  when Cai.Block.None =>
                     exit;
                  when others =>
                     Cai.Log.Client.Warning (Log, "Received unexpected request");
               end case;
            end;
         end loop;
      else
         Cai.Log.Client.Error (Log, "Failed to run test, client not Initialized");
      end if;
      if Sent = Data'Last and Received = Data'Last then
         T.Finished = True;
      end if;
   end Receive;

begin
   Cai.Log.Client.Initialize (Log, "Test " & Cai.Log.Image (Integer (Request_Count)));

end Test;
