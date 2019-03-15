
with Ada.Real_Time;
with Cai.Log.Client;
with Cai.Block;
with Cai.Block.Client;
with Burst;

use all type Cai.Block.Count;
use all type Cai.Block.Size;

package body Component is

   procedure Event;

   package Block_Client is new Cai.Block.Client (Event);
   Client : Cai.Block.Client_Session := Block_Client.Create;

   Burst_Size : constant := 1;

   Data : Burst.Burst (0 .. Cai.Block.Id (Burst_Size - 1)) := (others => (0,
                                                                          Ada.Real_Time.Time_First,
                                                                          Ada.Real_Time.Time_First));

   Block_Data : Cai.Block.Buffer (1 .. 4096) := (others => 0);

   Bs : Cai.Block.Size;

   procedure Construct is
   begin
      Cai.Log.Client.Info ("CBE Latency test");
      Block_Client.Initialize (Client, "");
      Bs := Block_Client.Block_Size (Client);
      if Bs > Block_Data'Length then
         Cai.Log.Client.Error ("Block size too large!");
         return;
      end if;
      Event;
   end Construct;

   Sending : Cai.Block.Id := Data'First;
   Received : Cai.Block.Count := 0;

   procedure Event is
      Request : Block_Client.Request := (Kind => Cai.Block.Write,
                                         Priv => Cai.Block.Null_Data,
                                         Start => 0,
                                         Length => 1,
                                         Status => Cai.Block.Raw);
   begin
      for I in Sending .. Data'Last loop
         Request.Start := I;
         if Block_Client.Ready (Client) then
            Burst.Start (I, 0, Data);
            Block_Client.Enqueue_Write (Client, Request, Block_Data (1 .. Cai.Block.Unsigned_Long (Bs)));
         else
            Sending := I;
            exit;
         end if;
      end loop;
      while Received < Burst_Size loop
         declare
            R : constant Block_Client.Request := Block_Client.Next (Client);
         begin
            case R.Kind is
               when Cai.Block.Write =>
                  Burst.Finish (R.Start, 0, Data);
                  Received := Received + 1;
               when Cai.Block.None =>
                  exit;
               when others =>
                  Cai.Log.Client.Warning ("Received unexpected request");
            end case;
         end;
      end loop;
   end;


end Component;
