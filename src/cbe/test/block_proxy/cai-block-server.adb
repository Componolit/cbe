with Cai.Block.Client;
with Gnat.Io;

package body Cai.Block.Server is

   procedure Callback (D : in out Component.Block_Server_Device);

   package Block_Client is new Cai.Block.Client (Component.Block_Server_Device, Callback);

   procedure Callback (D : in out Component.Block_Server_Device)
   is
   begin
      null;
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
   begin
      null;
   end Write;

   procedure Sync (D : in out Component.Block_Server_Device)
   is
   begin
      Block_Client.Sync (D.Client);
   end Sync;

end Cai.Block.Server;
