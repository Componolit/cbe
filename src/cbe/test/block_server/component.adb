
with Gnat.Io;
with Cai.Block;
use all type Cai.Block.Id;
use all type Cai.Block.Count;
use all type Cai.Block.Unsigned_Long;
use all type Cai.Block.Request_Kind;
use all type Cai.Block.Request_Status;

package body Component is

   Dispatcher : Cai.Block.Dispatcher_Session := Block_Dispatcher.Create;
   Server : Cai.Block.Server_Session := Block_Server.Create;

   subtype Block is Cai.Block.Buffer (1 .. 512);
   type Disk is array (Cai.Block.Id range 0 .. 1023) of Block;

   Ram_Disk : Disk;

   procedure Construct
   is
   begin
      Block_Dispatcher.Initialize (Dispatcher);
      Block_Dispatcher.Register (Dispatcher);
   end Construct;

   procedure Read (R : in out Block_Server.Request)
   is
      Buf : Cai.Block.Buffer (1 .. R.Length * Block_Size (Block_Server.Get_Instance (Server)));
      Success : Boolean;
   begin
      if Buf'Length mod Block'Length = 0 and then
         R.Start in Ram_Disk'Range and then
         R.Start + (R.Length - 1) in Ram_Disk'Range
      then
         for I in Cai.Block.Id range R.Start .. R.Start + (R.Length - 1) loop
            Buf (Buf'First + (I - R.Start) * Block'Length ..
               Buf'First + ((I - R.Start) + 1) * Block'Length - 1) := Ram_Disk (I);
         end loop;
         Block_Server.Read (Server, R, Buf, Success);
         R.Status := (if Success then Cai.Block.Ok else Cai.Block.Error);
      else
         R.Status := Cai.Block.Error;
      end if;
   end Read;

   procedure Write (R : in out Block_Server.Request)
   is
      B : Cai.Block.Buffer (1 .. Cai.Block.Unsigned_Long (R.Length * Cai.Block.Count (Block_Size (Block_Server.Get_Instance (Server)))));
      Success : Boolean;
   begin
      R.Status := Cai.Block.Error;
      if
         B'Length mod Block'Length = 0 and then
         R.Start in Ram_Disk'Range and then
         R.Start + (R.Length - 1) in Ram_Disk'Range
      then
         Block_Server.Write (Server, R, B, Success);
         if Success then
            for I in Cai.Block.Id range R.Start .. R.Start + (R.Length - 1) loop
               Ram_Disk (I) :=
                  B (B'First + (I - R.Start) * Block'Length ..
                     B'First + ((I - R.Start) + 1) * Block'Length - 1);
            end loop;
            R.Status := Cai.Block.Ok;
         end if;
      end if;
   end Write;

   procedure Event
   is
      R : Block_Server.Request;
   begin
      if Block_Server.Initialized (Server) then
         loop
            R := Block_Server.Head (Server);
            case R.Kind is
               when Cai.Block.Read =>
                  Read (R);
                  while R.Status /= Cai.Block.Acknowledged loop
                     Block_Server.Acknowledge (Server, R);
                  end loop;
                  Block_Server.Discard (Server);
               when Cai.Block.Write =>
                  Write (R);
                  while R.Status /= Cai.Block.Acknowledged loop
                     Block_Server.Acknowledge (Server, R);
                  end loop;
                  Block_Server.Discard (Server);
               when others => null;
            end case;
            exit when R.Kind = Cai.Block.None;
         end loop;
      end if;
   end Event;

   function Block_Count (S : Cai.Block.Server_Instance) return Cai.Block.Count
   is
   begin
      return Cai.Block.Count (Ram_Disk'Length);
   end Block_Count;

   function Block_Size (S : Cai.Block.Server_Instance) return Cai.Block.Size
   is
   begin
      return Cai.Block.Size (Block'Length);
   end Block_Size;

   function Writable (S : Cai.Block.Server_Instance) return Boolean
   is
   begin
      return True;
   end Writable;

   function Maximal_Transfer_Size (S : Cai.Block.Server_Instance) return Cai.Block.Unsigned_Long
   is
   begin
      return 16#ffffffff#;
   end Maximal_Transfer_Size;

   procedure Initialize (S : Cai.Block.Server_Instance; L : String)
   is
   begin
      Gnat.Io.Put_Line ("Server initialize with label: " & L);
      Ram_Disk := (others => (others => 0));
   end Initialize;

   procedure Finalize (S : Cai.Block.Server_Instance)
   is
   begin
      null;
   end Finalize;

   procedure Request
   is
      Label : String (1 .. 160);
      Last : Natural;
      Valid : Boolean;
   begin
      Block_Dispatcher.Session_Request (Dispatcher, Valid, Label, Last);
      if Valid and not Block_Server.Initialized (Server) then
         Gnat.Io.Put_Line ("Received request with label " & Label (1 .. Last));
         Block_Dispatcher.Session_Accept (Dispatcher, Server, Label (1 .. Last));
      end if;
      Block_Dispatcher.Session_Cleanup (Dispatcher, Server);
   end Request;

end Component;
