
with Gnat.Io;

with Cai.Block;

use all type Cai.Block.Id;
use all type Cai.Block.Count;
use all type Cai.Block.Request_Kind;
use all type Cai.Block.Request_Status;

package body Component is

   use all type Block_Server.Request;

   Client : Cai.Block.Client_Session := Block_Client.Create;
   Dispatcher : Cai.Block.Dispatcher_Session := Block_Dispatcher.Create;
   Server : Cai.Block.Server_Session := Block_Server.Create;

   procedure Construct
   is
   begin
      Block_Dispatcher.Initialize (Dispatcher);
      Block_Dispatcher.Register (Dispatcher);
   end Construct;

   type Cache_Entry is record
      Used : Boolean;
      Request : Block_Server.Request;
   end record;

   type Registry is array (1 .. 16) of Cache_Entry;

   Cache : Registry := (others => (False, (Kind => Cai.Block.None, Priv => Cai.Block.Null_Data)));

   procedure Store (R : Block_Server.Request; Success : out Boolean)
   is
      First_Free : Integer := 0;
   begin
      Success := False;
      for I in Cache'Range loop
         if not Cache (I).Used and First_Free = 0 then
            First_Free := I;
         end if;
         if Cache (I).Used and then Cache (I).Request = R then
            Success := True;
            return;
         end if;
      end loop;
      if First_Free > 0 then
         Cache (First_Free).Used := True;
         Cache (First_Free).Request := R;
         Success := True;
      end if;
   end Store;

   procedure Load (R : out Block_Server.Request; K : Cai.Block.Request_Kind; B : Cai.Block.Id)
   is
   begin
      R := Block_Server.Request' (Kind => None, Priv => Cai.Block.Null_Data);
      for I in Cache'Range loop
         if
            Cache (I).Used
            and then Cache (I).Request.Kind = K
            and then Cache (I).Request.Start = B
         then
            R := Cache (I).Request;
            Cache (I).Used := False;
            return;
         end if;
      end loop;
   end Load;

   procedure Handle_Write (R : Block_Server.Request)
   is
      Success : Boolean;
      B : Cai.Block.Buffer (1 .. Cai.Block.Unsigned_Long (R.Length * Cai.Block.Count (Block_Size (Block_Server.Get_Instance (Server)))));
      WR : Block_Client.Request := (Kind => Cai.Block.Write,
                                    Priv => Cai.Block.Null_Data,
                                    Start => R.Start,
                                    Length => R.Length,
                                    Status => Cai.Block.Raw);
   begin
      Store (R, Success);
      if Success then
         Block_Server.Write (Server, R, B, Success);
         if Success then
            Block_Client.Submit_Write (Client, WR, B);
            Block_Server.Discard (Server);
         end if;
      end if;
   end Handle_Write;

   procedure Handle_Read (R : Block_Server.Request)
   is
      Success : Boolean;
      WR : Block_Client.Request := (Kind => Cai.Block.Write,
                                    Priv => Cai.Block.Null_Data,
                                    Start => R.Start,
                                    Length => R.Length,
                                    Status => Cai.Block.Raw);
   begin
      Store (R, Success);
      if Success then
         Block_Client.Submit_Read (Client, WR);
         Block_Server.Discard (Server);
      end if;
   end Handle_Read;

   procedure Event
   is
      R : Block_Server.Request;
      A : Block_Client.Request;
   begin
      if
         Block_Client.Initialized (Client)
         and Block_Server.Initialized (Server)
      then
         loop
            R := Block_Server.Head (Server);
            case R.Kind is
               when Cai.Block.Write =>
                  Handle_Write (R);
               when Cai.Block.Read =>
                  Handle_Read (R);
               when others =>
                  null;
            end case;
            exit when R.Kind = Cai.Block.None;
         end loop;

         loop
            A := Block_Client.Next (Client);
            case A.Kind is
               when Cai.Block.Write =>
                  Load (R, A.Kind, A.Start);
                  if R.Kind = Cai.Block.Write then
                     R.Status := A.Status;
                     while R.Status /= Cai.Block.Acknowledged loop
                        Block_Server.Acknowledge (Server, R);
                     end loop;
                  else
                     A.Status := Cai.Block.Error;
                  end if;
                  Block_Client.Acknowledge (Client, A);
               when Cai.Block.Read =>
                  declare
                     B : Cai.Block.Buffer (1 .. Cai.Block.Unsigned_Long (A.Length * Cai.Block.Count (Block_Client.Block_Size (Client))));
                     Success : Boolean;
                  begin
                     Load (R, A.Kind, A.Start);
                     if R.Kind = Cai.Block.Read then
                        Block_Client.Read (Client, A, B);
                        R.Status := A.Status;
                        if R.Status = Cai.Block.Ok then
                           Block_Server.Read (Server, R, B, Success);
                           R.Status := (if Success then Cai.Block.Ok else Cai.Block.Error);
                        end if;
                        Block_Server.Acknowledge (Server, R);
                     else
                        A.Status := Cai.Block.Error;
                     end if;
                  end;
                  Block_Client.Acknowledge (Client, A);
               when others =>
                  null;
            end case;
            exit when A.Kind = Cai.Block.None;
         end loop;
      end if;
   end Event;

   procedure Dispatch
   is
      Label : String (1 .. 160);
      Last : Natural;
      Valid : Boolean;
   begin
      Block_Dispatcher.Session_Request (Dispatcher, Valid, Label, Last);
      if Valid and not Block_Server.Initialized (Server) then
         Block_Dispatcher.Session_Accept (Dispatcher, Server, Label (1 .. Last));
      end if;
      Block_Dispatcher.Session_Cleanup (Dispatcher, Server);
   end Dispatch;

   procedure Initialize_Server (S : Cai.Block.Server_Instance; L : String)
   is
   begin
      if not Block_Client.Initialized (Client) then
         Block_Client.Initialize (Client, L);
      end if;
   end Initialize_Server;

   procedure Finalize_Server (S : Cai.Block.Server_Instance)
   is
   begin
      if Block_Client.Initialized (Client) then
         Block_Client.Finalize (Client);
      end if;
   end Finalize_Server;

   function Block_Count (S : Cai.Block.Server_Instance) return Cai.Block.Count
   is
   begin
      if Block_Client.Initialized (Client) then
         return Block_Client.Block_Count (Client);
      else
         return 0;
      end if;
   end Block_Count;

   function Block_Size (S : Cai.Block.Server_Instance) return Cai.Block.Size
   is
   begin
      if Block_Client.Initialized (Client) then
         return Block_Client.Block_Size (Client);
      else
         return 0;
      end if;
   end Block_Size;

   function Writable (S : Cai.Block.Server_Instance) return Boolean
   is
   begin
      if Block_Client.Initialized (Client) then
         return Block_Client.Writable (Client);
      else
         return false;
      end if;
   end Writable;

   function Maximal_Transfer_Size (S : Cai.Block.Server_Instance) return Cai.Block.Unsigned_Long
   is
   begin
      return 16#ffffffff#;
   end Maximal_Transfer_Size;

end Component;
