
with Test;
with Cai.Log;
with Cai.Log.Client;
with Cai.Block;
with Cai.Block.Client;

package body Component with
   SPARK_Mode
is

   procedure Event;

   package Block_Client is new Cai.Block.Client (Event);
   Client : Cai.Block.Client_Session := Block_Client.Create;
   Log : Cai.Log.Client_Session := Cai.Log.Client.Create;

   package Test10 is new Test (Block_Client, 0, 10);

   procedure Construct is
   begin
      Cai.Log.Client.Initialize (Log, "Latency");
      Cai.Log.Client.Info (Log, "CBE Latency test");
      Block_Client.Initialize (Client, "");
      Event;
   end Construct;

   procedure Event is
   begin
      Test10.Receive (Client);
      Test10.Send (Client);
      Test10.Evaluate;
   end;


end Component;
