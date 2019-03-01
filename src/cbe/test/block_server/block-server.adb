
package body Block.Server is

   procedure Initialize (D : in out Device; L : String)
   is
   begin
      null;
   end Initialize;

   procedure Finalize (D : in out Device)
   is
   begin
      null;
   end Finalize;

   function Block_Count (D : in out Device) return Count
   is
   begin
      return 0;
   end Block_Count;

   function Block_Size (D : in out Device) return Size
   is
   begin
      return 0;
   end Block_Size;

   function Writable (D : in out Device) return Boolean
   is
   begin
      return False;
   end Writable;

   function Maximal_Transfer_Size (D : in out Device) return Unsigned_Long
   is
   begin
      return 0;
   end Maximal_Transfer_Size;

   procedure Read (D : in out Device; B : Buffer; R : Request)
   is
   begin
      null;
   end Read;

   procedure Sync (D : in out Device; R : Request)
   is
   begin
      null;
   end Sync;

   procedure Write (D : in out Device; B : Buffer; R : Request)
   is
   begin
      null;
   end Write;

end Block.Server;
