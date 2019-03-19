
with Cai.Log;
with Cai.Log.Client;
with Cai.Block;
with Cai.Block.Client;
with Iteration;

package body Component with
   SPARK_Mode
is

   procedure Event;

   package Block_Client is new Cai.Block.Client (Event);
   Client : Cai.Block.Client_Session := Block_Client.Create;
   Log : Cai.Log.Client_Session := Cai.Log.Client.Create;

   package Iter is new Iteration (Block_Client, 100);

   Write_Data : Iter.Test := Iter.Create (0, Cai.Block.Write);
   Read_Data : Iter.Test := Iter.Create (0, Cai.Block.Read);

   procedure Construct is
   begin
      Cai.Log.Client.Initialize (Log, "Latency");
      Cai.Log.Client.Info (Log, "CBE Latency test");
      Block_Client.Initialize (Client, "");
      Event;
   end Construct;

   procedure Event is
   begin
      if not Write_Data.Finished then
         Cai.Log.Client.Info (Log, "Running write test");
         Iter.Receive (Client, Write_Data);
         Iter.Send (Client, Write_Data);
      end if;
      if Write_Data.Finished and not Read_Data.Finished then
         Cai.Log.Client.Info (Log, "Running read test");
         Iter.Receive (Client, Read_Data);
         Iter.Send (Client, Read_Data);
      end if;
      if Write_Data.Finished and Read_Data.Finished then
         Cai.Log.Client.Info (Log, "Tests finished");
      end if;
   end;


end Component;
