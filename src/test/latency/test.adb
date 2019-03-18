
with Ada.Real_Time;
with Cai.Log.Client;

use all type Ada.Real_Time.Time;

package body Test is

   Log : Cai.Log.Client_Session := Cai.Log.Client.Create;

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

   function Image (Item : Long_Integer; Data : Burst) return String
   is
   begin
      return Cai.Log.Image (Ada.Real_Time.To_Duration (Data (Item).Finish - Data (Item).Start));
   end Image;

   Data : Burst (0 .. Long_Integer (Request_Count - 1)) := (others => (0,
                                                                       Ada.Real_Time.Time_First,
                                                                       Ada.Real_Time.Time_First));

   Block_Data : Cai.Block.Buffer (1 .. 4096) := (others => 0);

   Sent : Long_Integer := -1;
   Received : Long_Integer := -1;

   procedure Send (C : Cai.Block.Client_Session) is
      Request : Client.Request := (Kind => Cai.Block.Write,
                                   Priv => Cai.Block.Null_Data,
                                   Start => 0,
                                   Length => 1,
                                   Status => Cai.Block.Raw);
   begin
      if Sent < Data'Last then
         if Client.Initialized (C) then
            for I in Sent .. Data'Last - 1 loop
               Request.Start := Cai.Block.Id (I + 1);
               if Client.Ready (C, Request) then
                  Start (Request, Request_Offset, Data);
                  Client.Enqueue_Write (C, Request, Block_Data (1 .. Cai.Block.Unsigned_Long (Client.Block_Size (C))));
                  Sent := Sent + 1;
               else
                  exit;
               end if;
            end loop;
         else
            Cai.Log.Client.Error (Log, "Failed to run test, client not initialized");
         end if;
      end if;
   end Send;

   procedure Receive (C : Cai.Block.Client_Session) is
   begin
      if Client.Initialized (C) then
         while Received < Data'Last loop
            declare
               R : Client.Request := Client.Next (C);
            begin
               case R.Kind is
                  when Cai.Block.Write =>
                     Finish (R, Request_Offset, Data);
                     Received := Received + 1;
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
   end Receive;

   procedure Evaluate
   is
   begin
      if Sent = Data'Last and Received = Data'Last then
         for I in Data'Range loop
            Cai.Log.Client.Info (Log, Cai.Log.Image (I) & ": " & Image (I, Data));
         end loop;
      end if;
   end Evaluate;

begin
   Cai.Log.Client.Initialize (Log, "Test " & Cai.Log.Image (Integer (Request_Count)));

end Test;
