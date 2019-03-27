
with Ada.Unchecked_Conversion;
with Ada.Real_Time;
with Cai.Log.Client;
with Cai.Block;
with LSC.Internal.Types;
with LSC.Internal.SHA256;

use all type Ada.Real_Time.Time;
use all type Ada.Real_Time.Time_Span;
use all type Cai.Block.Count;
use all type Cai.Block.Size;
use all type Cai.Block.Request_Kind;
use all type Cai.Block.Request_Status;
use all type LSC.Internal.Types.Word32_Array_Type;
use all type LSC.Internal.SHA256.Message_Index;

package body Test is

   procedure Initialize (C : in out Cai.Block.Client_Session;
                         T : out Test_State;
                         L : in out Cai.Log.Client_Session)
   is
   begin
      T.Last := Cai.Block.Id'Last;
      T.Sent := 0;
      T.Written := 0;
      T.Read := 0;
      T.Count := Client.Block_Count (C);
      T.Bounds_Checked := False;
      T.Write_Context := LSC.Internal.SHA256.SHA256_Context_Init;
      T.Read_Context := LSC.Internal.SHA256.SHA256_Context_Init;
      if Client.Block_Size (C) > 4096 then
         Cai.Log.Client.Warning (L, "Block size "
                                    & Cai.Log.Image (Long_Integer (Client.Block_Size (C)))
                                    & " is too large, requests might fail");
      end if;
      Ring.Initialize (T.Data);
   end Initialize;

   Progress : Long_Integer := -1;
   Start : Ada.Real_Time.Time;

   function Remain (S : Ada.Real_Time.Time;
                    C : Ada.Real_Time.Time;
                    P : Long_Integer) return Duration
   is
   begin
      if P < 1 or P > 99 then
         return Duration (0);
      end if;
      return Ada.Real_Time.To_Duration (((C - S) / Integer (P)) * Integer (100 - P));
   end Remain;

   procedure Bounds_Check (C : in out Cai.Block.Client_Session;
                           T : in out Test_State;
                           Success : out Boolean;
                           L : in out Cai.Log.Client_Session)
   is
      Request : constant Client.Request := (Kind => Cai.Block.Read,
                                   Priv => Cai.Block.Null_Data,
                                   Start => Cai.Block.Id (T.Count),
                                   Length => 1,
                                   Status => Cai.Block.Raw);
      R : Client.Request := Client.Next (C);
   begin
      Success := True;
      if R.Kind = Cai.Block.Read then
         Success := R.Status = Cai.Block.Error;
         if not Success then
            Cai.Log.Client.Error (L, "Bounds check failed, block "
                                     & Cai.Log.Image (Long_Integer (R.Start))
                                     & " should not be: "
                                     & (case R.Status is
                                          when Cai.Block.Raw => "Raw",
                                          when Cai.Block.Ok => "Ok",
                                          when Cai.Block.Error => "Error",
                                          when Cai.Block.Acknowledged => "Acknowledged"));
         end if;
         Client.Release (C, R);
         T.Bounds_Checked := True;
         return;
      end if;
      while not Client.Ready (C, Request) loop
         null;
      end loop;
      Client.Enqueue_Read (C, Request);
      Client.Submit(C);
      Start := Ada.Real_Time.Clock;
   end Bounds_Check;

   function Bounds_Check_Finished (T : Test_State) return Boolean
   is
   begin
      return T.Bounds_Checked;
   end Bounds_Check_Finished;

   procedure Hash_Block (Context : in out LSC.Internal.SHA256.Context_Type; Buffer : Cai.Block.Buffer) with
      Pre => Buffer'Length mod (LSC.Internal.SHA256.Block_Size / 8) = 0
   is
      subtype Block_Message is LSC.Internal.SHA256.Message_Type
         (1 .. Buffer'Length / (LSC.Internal.SHA256.Block_Size / 8));
      subtype Sub_Block is Cai.Block.Buffer (1 .. Buffer'Length);
      function Convert_Block is new Ada.Unchecked_Conversion (Sub_Block, Block_Message);
      Message : constant Block_Message := Convert_Block (Buffer);
   begin
      for Block of Message loop
         LSC.Internal.SHA256.Context_Update (Context, Block);
      end loop;
   end Hash_Block;

   procedure Write_Recv (C : in out Cai.Block.Client_Session;
                         T : in out Test_State;
                         Success : in out Boolean;
                         L : in out Cai.Log.Client_Session)
   is
   begin
      while T.Written < T.Count loop
         declare
            R : Client.Request := Client.Next (C);
         begin
            if R.Kind = Cai.Block.Write then
               T.Written := T.Written + 1;
               Success := R.Status = Cai.Block.Ok;
               if not Success then
                  Cai.Log.Client.Error (L, "Write received erroneous request "
                                           & Cai.Log.Image (Long_Integer (R.Start)) & "/"
                                           & Cai.Log.Image (Long_Integer (Client.Block_Count (C))));
               end if;
               Client.Release (C, R);
            else
               exit;
            end if;
         end;
      end loop;
   end Write_Recv;

   procedure Write_Send (C : in out Cai.Block.Client_Session;
                         T : in out Test_State;
                         Success : in out Boolean;
                         L : in out Cai.Log.Client_Session)
   is
      Request : Client.Request := (Kind => Cai.Block.Write,
                                   Priv => Cai.Block.Null_Data,
                                   Start => 0,
                                   Length => 1,
                                   Status => Cai.Block.Raw);
      Buf : Block;
   begin
      if T.Sent < T.Count then
         loop
            exit when not Client.Ready (C, Request) or T.Sent >= T.Count;
            Request.Start := Next (T.Last);
            PR_Block (Buf (1 .. Request.Length * Client.Block_Size (C)), Request.Start);
            Client.Enqueue_Write (C, Request,
                                  Buf (1 .. Request.Length * Client.Block_Size (C)));
            Hash_Block (T.Write_Context, Buf (1 .. Request.Length * Client.Block_Size (C)));
            T.Sent := T.Sent + 1;
            T.Last := Request.Start;
         end loop;
         Client.Submit (C);
      end if;
   end Write_Send;

   procedure Write (C : in out Cai.Block.Client_Session;
                    T : in out Test_State;
                    Success : out Boolean;
                    L : in out Cai.Log.Client_Session)
   is
      Current : Ada.Real_Time.Time;
   begin
      Success := True;
      Write_Recv (C, T, Success, L);
      Write_Send (C, T, Success, L);
      if T.Count >= 50 and then Long_Integer (T.Written) / Long_Integer (T.Count / 50) /= Progress then
         Progress := Long_Integer (T.Written) / Long_Integer (T.Count / 50);
         Cai.Log.Client.Info (L, "Writing... (" & Cai.Log.Image (Progress) & "%)");
         Current := Ada.Real_Time.Clock;
         Cai.Log.Client.Info (L, "Elapsed: "
                                 & Cai.Log.Image (Ada.Real_Time.To_Duration (Current - Start))
                                 & " Remaining: "
                                 & Cai.Log.Image (Remain (Start, Current, Progress)));
      end if;
      if Write_Finished (T) then
         T.Sent := 0;
         T.Last := Cai.Block.Id'Last;
      end if;
   end Write;

   function Write_Finished (T : Test_State) return Boolean
   is
   begin
      return T.Written = T.Count;
   end Write_Finished;

   procedure Read_Recv (C : in out Cai.Block.Client_Session;
                        T : in out Test_State;
                        Success : in out Boolean;
                        L : in out Cai.Log.Client_Session)
   is
      Buf : Block;
   begin
      while T.Read < T.Count loop
         declare
            R : Client.Request := Client.Next (C);
         begin
            if R.Kind = Cai.Block.Read then
               if R.Status = Cai.Block.Ok and then Ring.Has_Block (T.Data, R.Start) then
                  Client.Read (C, R, Buf (1 .. R.Length * Client.Block_Size (C)));
                  Ring.Set_Data (T.Data, R.Start, Buf);
               else
                  Cai.Log.Client.Error (L, "Read received erroneous request "
                                           & Cai.Log.Image (Long_Integer (R.Start)) & "/"
                                           & Cai.Log.Image (Long_Integer (Client.Block_Count (C))));
                  Success := False;
               end if;
               T.Read := T.Read + 1;
               Client.Release (C, R);
            else
               exit;
            end if;
         end;
      end loop;
   end Read_Recv;

   procedure Read_Send (C : in out Cai.Block.Client_Session;
                        T : in out Test_State;
                        Success : in out Boolean;
                        L : in out Cai.Log.Client_Session)
   is
      Request : Client.Request := (Kind => Cai.Block.Read,
                                   Priv => Cai.Block.Null_Data,
                                   Start => 0,
                                   Length => 1,
                                   Status => Cai.Block.Raw);
   begin
      if T.Sent < T.Count then
         loop
            exit when not Client.Ready (C, Request) or not Ring.Free (T.Data) or T.Sent >= T.Count;
            Request.Start := Next (T.Last);
            Client.Enqueue_Read (C, Request);
            if not Ring.Has_Block (T.Data, Request.Start) then
               Ring.Add (T.Data, Request.Start);
            else
               Cai.Log.Client.Error (L, "Tried to insert duplicated block");
               Success := False;
            end if;
            T.Sent := T.Sent + 1;
            T.Last := Request.Start;
         end loop;
         Client.Submit (C);
      end if;
   end Read_Send;

   procedure Read (C : in out Cai.Block.Client_Session;
                   T : in out Test_State;
                   Success : out Boolean;
                   L : in out Cai.Log.Client_Session)
   is
      Current : Ada.Real_Time.Time;
   begin
      Success := True;
      Read_Recv (C, T, Success, L);
      Read_Send (C, T, Success, L);
      while Ring.Block_Ready (T.Data) loop
         declare
            Buf : Block;
            Id : Cai.Block.Id;
         begin
            Ring.Get_Block (T.Data, Id, Buf);
            Hash_Block (T.Read_Context, Buf (1 .. Cai.Block.Count (1) * Client.Block_Size (C)));
         end;
      end loop;
      if T.Count >= 50 and then Long_Integer (T.Read) / Long_Integer (T.Count / 50) + 50 /= Progress then
         Progress := Long_Integer (T.Read) / Long_Integer (T.Count / 50) + 50;
         Cai.Log.Client.Info (L, "Reading... (" & Cai.Log.Image (Progress) & "%)");
         Current := Ada.Real_Time.Clock;
         Cai.Log.Client.Info (L, "Elapsed: "
                                 & Cai.Log.Image (Ada.Real_Time.To_Duration (Current - Start))
                                 & " Remaining: "
                                 & Cai.Log.Image (Remain (Start, Current, Progress)));
      end if;
   end Read;

   function Read_Finished (T : Test_State) return Boolean
   is
   begin
      return T.Read = T.Count;
   end Read_Finished;

   procedure Compare (T : in out Test_State; Equal : out Boolean)
   is
      Block : constant LSC.Internal.SHA256.Block_Type := (others => 0);
   begin
      LSC.Internal.SHA256.Context_Finalize (T.Write_Context, Block, 0);
      LSC.Internal.SHA256.Context_Finalize (T.Read_Context, Block, 0);
      Equal := LSC.Internal.SHA256.SHA256_Get_Hash (T.Write_Context) =
               LSC.Internal.SHA256.SHA256_Get_Hash (T.Read_Context);
      T.Compared := True;
   end Compare;

   function Compare_Finished (T : Test_State) return Boolean
   is
   begin
      return T.Compared;
   end Compare_Finished;

end Test;
