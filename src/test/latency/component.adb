
with Cai.Log;
with Cai.Log.Client;
with Cai.Block;
with Cai.Block.Client;
with Run;

package body Component with
   SPARK_Mode
is

   procedure Event;

   package Block_Client is new Cai.Block.Client (Event);
   Client : Cai.Block.Client_Session := Block_Client.Create;
   Log : Cai.Log.Client_Session := Cai.Log.Client.Create;
   Xml : Cai.Log.Client_Session := Cai.Log.Client.Create;

   package Write_Run is new Run (Block_Client, 100, 4, Cai.Block.Write);
   package Read_Run is new Run (Block_Client, 100, 4, Cai.Block.Read);

   Write_Data : Write_Run.Run_Type := Write_Run.Create;
   Read_Data : Read_Run.Run_Type := Read_Run.Create;

   procedure Construct is
   begin
      Cai.Log.Client.Initialize (Log, "Latency");
      Cai.Log.Client.Info (Log, "CBE Latency test");
      Cai.Log.Client.Initialize (Xml, "XML");
      Block_Client.Initialize (Client, "");
      Write_Run.Initialize (Write_Data);
      Read_Run.Initialize (Read_Data);
      Event;
   end Construct;

   procedure Event is
   begin
      if not Write_Run.Finished (Write_Data) then
         Cai.Log.Client.Info (Log, "Run: Write");
         Write_Run.Run (Client, Write_Data);
      end if;
      if Write_Run.Finished (Write_Data) and not Read_Run.Finished (Read_Data) then
         Cai.Log.Client.Info (Log, "Run: Read");
         Read_Run.Run (Client, Read_Data);
      end if;
      if Write_Run.Finished (Write_Data) and Read_Run.Finished (Read_Data) then
         Cai.Log.Client.Info (Log, "Tests finished");
         Cai.Log.Client.Info (Xml, "<test name=Latency platform=Genode hardware=Qemu block_size="
                                   & Cai.Log.Image (Long_Integer (Block_Client.Block_Size (Client)))
                                   & " />");
         Write_Run.Xml (Xml, Write_Data);
         Read_Run.Xml (Xml, Read_Data);
         Cai.Log.Client.Info (Xml, "</test>");
      end if;
   end;


end Component;
