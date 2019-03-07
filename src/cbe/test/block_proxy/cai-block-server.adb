with Cai.Block.Client;
with Gnat.Io;

package body Cai.Block.Server is

   procedure Callback (D : in out Component.Block_Server_Device);

   package Block_Client is new Cai.Block.Client (Component.Block_Server_Device, Callback);

   Null_Request : constant Request := Request' (Kind => None, Priv => Null_Data);

   type Write_Request is record
      Free : Boolean;
      Req : Request;
   end record;

   type Read_Request is record
      Free : Boolean;
      Req : Request;
      Buf : System.Address;
   end record;

   type Write_Request_Queue is array (Integer range <>) of Write_Request;
   type Read_Request_Queue is array (Integer range <>) of Read_Request;

   Write_Queue : Write_Request_Queue (1 .. 16) := (others => (Free => True, Req => Null_Request));
   Read_Queue : Read_Request_Queue (1 .. 16) := (others => (Free => True, Req => Null_Request, Buf => System.Null_Address));

   procedure Store (R : Request; Success : out Boolean)
   is
   begin
      Success := False;
      for I in Write_Queue'Range loop
         if Write_Queue (I).Free then
            Write_Queue (I).Req := R;
            Write_Queue (I).Free := False;
            Success := True;
            exit;
         end if;
      end loop;
   end Store;

   procedure Load (Start : Id; Length : Count; R : out Request)
   is
   begin
      R := (Kind => None, Priv => Null_Data);
      for I in Write_Queue'Range loop
         if
            not Write_Queue (I).Free and then
            Write_Queue (I).Req.Start = Start and then
            Write_Queue (I).Req.Length = Length
         then
            R := Write_Queue (I).Req;
            Write_Queue (I).Free := True;
            exit;
         end if;
      end loop;
   end Load;

   procedure Callback (D : in out Component.Block_Server_Device)
   is
   begin
      loop
         declare
            CR : Block_Client.Request := Block_Client.Next (D.Client);
            SR : Request;
         begin
            case CR.Kind is
               when Cai.Block.Write =>
                  Load (CR.Start, CR.Length, SR);
                  if SR.Kind = Cai.Block.Write then
                     SR.Status := Ok;
                     Acknowledge (D, SR, D.Context);
                  end if;
               when Cai.Block.None =>
                  exit;
               when others =>
                  null;
            end case;
         end;
      end loop;
   end Callback;

   procedure Initialize (D : out Component.Block_Server_Device; L : String; C : Context)
   is
   begin
      D.Context := C;
      Block_Client.Initialize_Device (D.Client, L, D);
   end Initialize;

   procedure Finalize (D : in out Component.Block_Server_Device)
   is
   begin
      Block_Client.Finalize_Device (D.Client);
   end Finalize;

   function Block_Count (D : Component.Block_Server_Device) return Count
   is
   begin
      return Block_Client.Block_Count (D.Client);
   end Block_Count;

   function Block_Size (D : Component.Block_Server_Device) return Size
   is
   begin
      return Block_Client.Block_Size (D.Client);
   end Block_Size;

   function Writable (D : Component.Block_Server_Device) return Boolean
   is
   begin
      return Block_Client.Writable (D.Client);
   end Writable;

   function Maximal_Transfer_Size (D : Component.Block_Server_Device) return Unsigned_Long
   is
   begin
      return Block_Client.Maximal_Transfer_Size (D.Client);
   end Maximal_Transfer_Size;

   procedure Read (D : in out Component.Block_Server_Device; B : System.Address; L : Unsigned_Long; R : in out Request)
   is
   begin
      null;
   end Read;

   procedure Write (D : in out Component.Block_Server_Device; B : Buffer; R : in out Request)
   is
      Req : Block_Client.Request := (
         Kind => Cai.Block.Write,
         Priv => Null_Data,
         Start => R.Start,
         Length => R.Length,
         Status => Cai.Block.Raw);
      Success : Boolean;
   begin
      Store (R, Success);
      if Success then
         Block_Client.Submit_Write (D.Client, Req, B);
         R.Status := Cai.Block.Ok;
      else
         R.Status := Cai.Block.Error;
      end if;
   end Write;

   procedure Sync (D : in out Component.Block_Server_Device)
   is
   begin
      Block_Client.Sync (D.Client);
   end Sync;

end Cai.Block.Server;
