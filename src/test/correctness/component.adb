
with Ada.Unchecked_Conversion;
with Cai.Log;
with Cai.Log.Client;
with Cai.Block;
with Cai.Block.Client;
with LSC.AES_Generic;
with LSC.AES_Generic.CBC;
with Permutation;
with Test;

use all type Cai.Block.Id;
use all type Cai.Block.Count;

package body Component with
   SPARK_Mode
is

   package Block_Permutation is new Permutation (Cai.Block.Id);

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

   procedure PR_Block (B : in out Cai.Block.Buffer; I : Cai.Block.Id)
   is
      function CBC_Key is new LSC.AES_Generic.Enc_Key (Cai.Block.Unsigned_Long,
                                                       Cai.Block.Byte,
                                                       Cai.Block.Buffer);
      procedure CBC is new LSC.AES_Generic.CBC.Encrypt (Cai.Block.Unsigned_Long,
                                                        Cai.Block.Byte,
                                                        Cai.Block.Buffer,
                                                        Cai.Block.Unsigned_Long,
                                                        Cai.Block.Byte,
                                                        Cai.Block.Buffer);
      subtype Id is Cai.Block.Buffer (1 .. 8);
      function Convert_Id is new Ada.Unchecked_Conversion (Cai.Block.Id, Id);
      Null_Block : constant Cai.Block.Buffer (1 .. B'Length) := (others => 0);
      IV : Cai.Block.Buffer (1 .. 16) := (others => 0);
      Key : constant Cai.Block.Buffer (1 .. 128) := (others => 16#42#);
      --  This is no cryptographically secure encryption and only used to generate pseudo random blocks
   begin
      IV (1 .. 8) := Convert_Id (I);
      CBC (Null_Block, IV, CBC_Key (Key, LSC.AES_Generic.L128), B);
   end PR_Block;

   package Disk_Test is new Test (Block_Client, Next, PR_Block);

   Data : Disk_Test.Test_State;

   procedure Check_Permutation
   is
      Num : Cai.Block.Id;
   begin
      while Block_Permutation.Has_Element loop
         Block_Permutation.Next (Num);
         Cai.Log.Client.Info (Log, "Perm " & Cai.Log.Image (Long_Integer (Num)));
      end loop;
   end Check_Permutation;

   procedure Construct
   is
      Count : Long_Integer;
      Size : Long_Integer;
   begin
      Cai.Log.Client.Initialize (Log, "Correctness");
      Cai.Log.Client.Info (Log, "Correctness");
      Block_Client.Initialize (Block, "");
      Block_Permutation.Initialize (23);
      Check_Permutation;
      Block_Permutation.Initialize (Cai.Block.Id (Block_Client.Block_Count (Block) - 1));
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
         and Disk_Test.Bounds_Check_Finished (Data)
         and Disk_Test.Write_Finished (Data)
         and not Disk_Test.Read_Finished (Data)
      then
         Disk_Test.Read (Block, Data, Success, Log);
      end if;

      if
         Success
         and Disk_Test.Bounds_Check_Finished (Data)
         and Disk_Test.Write_Finished (Data)
         and Disk_Test.Read_Finished (Data)
         and not Disk_Test.Compare_Finished (Data)
      then
         Disk_Test.Compare (Data, Success);
      end if;

      if
         (Disk_Test.Bounds_Check_Finished (Data)
          and Disk_Test.Write_Finished (Data)
          and Disk_Test.Read_Finished (Data)
          and Disk_Test.Compare_Finished (Data))
         or not Success
      then
         Cai.Log.Client.Info (Log, "Correctness test "
                                   & (if Success then "succeeded." else "failed."));
      end if;
   end Event;

end Component;
