
with Cai.Log;
with Cai.Log.Client;
with Cai.Block;
with Cai.Block.Client;
with Rwr;

package body Component with
   SPARK_Mode
is

   procedure Event;

   package Block_Client is new Cai.Block.Client (Event);
   Client : Cai.Block.Client_Session := Block_Client.Create;
   Log : Cai.Log.Client_Session := Cai.Log.Client.Create;
   Xml : Cai.Log.Client_Session := Cai.Log.Client.Create;

   generic
      with package Last is new Rwr (<>);
      with package Current is new Rwr (<>);
      Name : String;
   procedure Checked_Run (L : Last.Rwr_Run; C : in out Current.Rwr_Run);

   procedure Pre_Print (Test : String);
   procedure Post_Print (Finished : Boolean);

   procedure Checked_Run (L : Last.Rwr_Run; C : in out Current.Rwr_Run)
   is
   begin
      if Last.Finished (L) and not Current.Finished (C) then
         Pre_Print (Name);
         Current.Run (Client, C, Log);
         Post_Print (Current.Finished (C));
      end if;
   end Checked_Run;

   package Small_1 is new Rwr (Block_Client, 1, 100);
   package Small_2 is new Rwr (Block_Client, 2, 100);
   package Small_4 is new Rwr (Block_Client, 4, 100);

   package Medium_500 is new Rwr (Block_Client, 500, 100);
   package Medium_1000 is new Rwr (Block_Client, 1000, 100);
   package Medium_5000 is new Rwr (Block_Client, 5000, 100);

   Small_1_Data : Small_1.Rwr_Run;
   Small_2_Data : Small_2.Rwr_Run;
   Small_4_Data : Small_4.Rwr_Run;

   Medium_500_Data : Medium_500.Rwr_Run;
   Medium_1000_Data : Medium_1000.Rwr_Run;
   Medium_5000_Data : Medium_5000.Rwr_Run;

   procedure Small_2_Run is new Checked_Run (Small_1, Small_2, "Small_2");
   procedure Small_4_Run is new Checked_Run (Small_2, Small_4, "Small_4");
   procedure Medium_500_Run is new Checked_Run (Small_4, Medium_500, "Medium_500");
   procedure Medium_1000_Run is new Checked_Run (Medium_500, Medium_1000, "Medium_1000");
   procedure Medium_5000_Run is new Checked_Run (Medium_1000, Medium_5000, "Medium_5000");

   procedure Construct is
   begin
      Cai.Log.Client.Initialize (Log, "Latency");
      Cai.Log.Client.Info (Log, "Initializing test data");
      Cai.Log.Client.Initialize (Xml, "XML");
      Block_Client.Initialize (Client, "");
      Small_1.Initialize (Small_1_Data);
      Small_2.Initialize (Small_2_Data);
      Small_4.Initialize (Small_4_Data);
      Medium_500.Initialize (Medium_500_Data);
      Medium_1000.Initialize (Medium_1000_Data);
      Medium_5000.Initialize (Medium_5000_Data);
      Event;
   end Construct;

   Printed : Boolean := False;

   procedure Pre_Print (Test : String)
   is
   begin
      if not Printed then
         Cai.Log.Client.Info (Log, "Test: " & Test);
         Printed := True;
      end if;
   end Pre_Print;

   procedure Post_Print (Finished : Boolean)
   is
   begin
      if Finished then
         Cai.Log.Client.Flush (Log);
         Printed := False;
      end if;
   end Post_Print;

   procedure Event is
   begin
      if not Small_1.Finished (Small_1_Data) then
         Pre_Print ("Small_1");
         Small_1.Run (Client, Small_1_Data, Log);
         Post_Print (Small_1.Finished (Small_1_Data));
      end if;
      Small_2_Run (Small_1_Data, Small_2_Data);
      Small_4_Run (Small_2_Data, Small_4_Data);
      Medium_500_Run (Small_4_Data, Medium_500_Data);
      Medium_1000_Run (Medium_500_Data, Medium_1000_Data);
      Medium_5000_Run (Medium_1000_Data, Medium_5000_Data);
      if
         Small_1.Finished (Small_1_Data)
         and Small_2.Finished (Small_2_Data)
         and Small_4.Finished (Small_4_Data)
         and Medium_500.Finished (Medium_500_Data)
         and Medium_1000.Finished (Medium_1000_Data)
         and Medium_5000.Finished (Medium_5000_Data)
      then
         Cai.Log.Client.Info (Log, "Tests finished, writing data...");
         Cai.Log.Client.Info (Xml, "<test name=""Latency"" platform=""Genode"" hardware=""Qemu"" block_size="""
                                   & Cai.Log.Image (Long_Integer (Block_Client.Block_Size (Client)))
                                   & """/>");
         Cai.Log.Client.Info (Log, "Small_1...");
         Small_1.Xml (Xml, Small_1_Data, Log);
         Cai.Log.Client.Info (Log, "Small_2...");
         Small_2.Xml (Xml, Small_2_Data, Log);
         Cai.Log.Client.Info (Log, "Small_4...");
         Small_4.Xml (Xml, Small_4_Data, Log);
         Cai.Log.Client.Info (Log, "Medium_500...");
         Medium_500.Xml (Xml, Medium_500_Data, Log);
         Cai.Log.Client.Info (Log, "Medium_1000...");
         Medium_1000.Xml (Xml, Medium_1000_Data, Log);
         Cai.Log.Client.Info (Log, "Medium_5000...");
         Medium_5000.Xml (Xml, Medium_5000_Data, Log);
         Cai.Log.Client.Info (Xml, "</test>");
         Cai.Log.Client.Flush (Xml);
         Cai.Log.Client.Info (Log, "Data written.");
         Cai.Log.Client.Flush (Log);
      end if;
   end;


end Component;
