
with Cai.Log;
with Cai.Log.Client;
with Cai.Block;
with Cai.Block.Client;
with Test;

use all type Cai.Block.Id;

package body Component with
   SPARK_Mode
is

   Log : Cai.Log.Client_Session := Cai.Log.Client.Create;

   package Block_Client is new Cai.Block.Client (Event);

   Block : Cai.Block.Client_Session := Block_Client.Create;

   function Next (Current : Cai.Block.Id) return Cai.Block.Id
   is
   begin
      if Current = Cai.Block.Id'Last then
         return 0;
      else
         return Current + Cai.Block.Count (1);
      end if;
   end Next;

   procedure PR_Block (B : in out Cai.Block.Buffer)
   is
   begin
      B := (others => 0);
   end PR_Block;

   package Disk_Test is new Test (Block_Client, Next, PR_Block);

   Data : Disk_Test.Test_State;

   procedure Construct
   is
      Count : Long_Integer;
      Size : Long_Integer;
   begin
      Cai.Log.Client.Initialize (Log, "Correctness");
      Cai.Log.Client.Info (Log, "Correctness");
      Block_Client.Initialize (Block, "");
      Count := Long_Integer (Block_Client.Block_Count (Block));
      Size := Long_Integer (Block_Client.Block_Size (Block));
      Cai.Log.Client.Info (Log, "Running correctness test over "
                                & Cai.Log.Image (Count)
                                & " blocks of "
                                & Cai.Log.Image (Size)
                                & " byte size ("
                                & (if
                                      Count * Size < 1024 ** 3
                                   then
                                      Cai.Log.Image (Count * Size / 1024 ** 2) & " MiB"
                                   else
                                      Cai.Log.Image (Count * Size / 1024 ** 3) & " GiB")
                                & ")...");
      Disk_Test.Initialize (Block, Data, Log);
      Event;
   end Construct;

   Success : Boolean := True;

   procedure Event
   is
   begin
      if
         Success
         and not Disk_Test.Bounds_Check_Finished (Data)
      then
         Disk_Test.Bounds_Check (Block, Data, Success, Log);
      end if;

      if
         Success
         and Disk_Test.Bounds_Check_Finished (Data)
         and not Disk_Test.Write_Finished (Data)
      then
         Disk_Test.Write (Block, Data, Success, Log);
      end if;

      if
         Success
         and Disk_Test.Write_Finished (Data)
         and not Disk_Test.Read_Finished (Data)
      then
         Disk_Test.Read (Block, Data, Success, Log);
      end if;

      if
         (Disk_Test.Write_Finished (Data)
          and Disk_Test.Read_Finished (Data))
         or not Success
      then
         Cai.Log.Client.Info (Log, "Correctness test "
                                   & (if Success then "succeeded." else "failed."));
      end if;
   end Event;

end Component;
