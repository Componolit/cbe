
with Cai.Log.Client;
with Cai.Block;

use all type Cai.Block.Count;
use all type Cai.Block.Size;
use all type Cai.Block.Request_Kind;
use all type Cai.Block.Request_Status;

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
      if Client.Block_Size (C) > 4096 then
         Cai.Log.Client.Warning (L, "Block size "
                                    & Cai.Log.Image (Long_Integer (Client.Block_Size (C)))
                                    & " is too large, requests might fail");
      end if;
      Ring.Initialize (T.Data);
   end Initialize;

   Progress : Long_Integer := -1;

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
            Request.Start := Next (T.Last);
            exit when not Client.Ready (C, Request) or T.Sent >= T.Count;
            PR_Block (Buf);
            Client.Enqueue_Write (C, Request,
                                  Buf (1 .. Request.Length * Client.Block_Size (C)));
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
   begin
      Success := True;
      Write_Recv (C, T, Success, L);
      Write_Send (C, T, Success, L);
      if Long_Integer (T.Written) / Long_Integer (T.Count / 50) /= Progress then
         Progress := Long_Integer (T.Written) / Long_Integer (T.Count / 50);
         Cai.Log.Client.Info (L, "Writing... (" & Cai.Log.Image (Progress) & "%)");
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
            Request.Start := Next (T.Last);
            exit when not Client.Ready (C, Request) or not Ring.Free (T.Data) or T.Sent >= T.Count;
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
         end;
      end loop;
      if Long_Integer (T.Read) / Long_Integer (T.Count / 50) + 50 /= Progress then
         Progress := Long_Integer (T.Read) / Long_Integer (T.Count / 50) + 50;
         Cai.Log.Client.Info (L, "Reading... (" & Cai.Log.Image (Progress) & "%)");
      end if;
   end Read;

   function Read_Finished (T : Test_State) return Boolean
   is
   begin
      return T.Read = T.Count;
   end Read_Finished;

end Test;
