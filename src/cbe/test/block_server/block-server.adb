
with Gnat.Io;

package body Block.Server is

   subtype Disk_Block is Buffer (1 .. 512);
   type Disk is array (Id range <>) of Disk_Block;
   Ram_Disk : Disk (0 .. 1023);

   procedure Initialize (D : in out Component.Block_Device; L : String; C : Context)
   is
   begin
      Gnat.Io.Put_Line ("Initializing Ada RAM Disk with label " & L);
      D.Context := C;
      D.Block_Count := Ram_Disk'Length;
      D.Block_Size := Disk_Block'Length;
   end Initialize;

   procedure Finalize (D : in out Component.Block_Device)
   is
   begin
      null;
   end Finalize;

   function Block_Count (D : in out Component.Block_Device) return Count
   is
   begin
      return D.Block_Count;
   end Block_Count;

   function Block_Size (D : in out Component.Block_Device) return Size
   is
   begin
      return D.Block_Size;
   end Block_Size;

   function Writable (D : in out Component.Block_Device) return Boolean
   is
   begin
      return True;
   end Writable;

   function Maximal_Transfer_Size (D : in out Component.Block_Device) return Unsigned_Long
   is
   begin
      return 16#FFFFFFFF#;
   end Maximal_Transfer_Size;

   procedure Read (D : in out Component.Block_Device; B : out Buffer; R : in out Request)
   is
   begin
      if B'Length mod Disk_Block'Length = 0 and then
         R.Start in Ram_Disk'Range and then
         R.Start + Id (R.Length) - 1 in Ram_Disk'Range
      then
         for I in Id range R.Start .. R.Start + Id (R.Length) - 1 loop
            B (B'First + Unsigned_Long (I - R.Start) * Disk_Block'Length ..
               B'First + Unsigned_Long (I - R.Start + 1) * Disk_Block'Length - 1) := Ram_Disk (I);
         end loop;
         R.Status := Ok;
      else
         R.Status := Error;
      end if;
      Acknowledge (D, R, D.Context);
   end Read;

   procedure Sync (D : in out Component.Block_Device; R : in out Request)
   is
   begin
      R.Status := Ok;
      Acknowledge (D, R, D.Context);
   end Sync;

   procedure Write (D : in out Component.Block_Device; B : Buffer; R : in out Request)
   is
   begin
      if
         B'Length mod Disk_Block'Length = 0 and then
         R.Start in Ram_Disk'Range and then
         R.Start + Id (R.Length) - 1 in Ram_Disk'Range
      then
         for I in Id range R.Start .. R.Start + Id (R.Length) - 1 loop
            Ram_Disk (I) :=
               B (B'First + Unsigned_Long (I - R.Start) * Disk_Block'Length ..
                  B'First + Unsigned_Long (I - R.Start + 1) * Disk_Block'Length - 1);
         end loop;
         R.Status := Ok;
      else
         R.Status := Error;
      end if;
      Acknowledge (D, R, D.Context);
   end Write;

end Block.Server;
