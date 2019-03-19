
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

   package Write_Iter is new Iteration (Block_Client, 100, Cai.Block.Write);
   package Read_Iter is new Iteration (Block_Client, 100, Cai.Block.Read);

   Write_Data : Write_Iter.Test := Write_Iter.Create (0);
   Read_Data : Read_Iter.Test := Read_Iter.Create (0);

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
         Write_Iter.Receive (Client, Write_Data);
         Write_Iter.Send (Client, Write_Data);
      end if;
      if Write_Data.Finished and not Read_Data.Finished then
         Cai.Log.Client.Info (Log, "Running read test");
         Read_Iter.Receive (Client, Read_Data);
         Read_Iter.Send (Client, Read_Data);
      end if;
      if Write_Data.Finished and Read_Data.Finished then
         Cai.Log.Client.Info (Log, "Tests finished");
      end if;
   end;


end Component;
