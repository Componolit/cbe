generic
   type Device is limited private;
   with procedure Acknowledge (D : in out Device; R : Request);
package Block.Server is

   procedure Initialize (D : in out Device; L : String);

   procedure Finalize (D : in out Device);

   function Block_Count (D : in out Device) return Count;

   functioN Block_Size (D : in out Device) return Size;

   function Writable (D : in out Device) return Boolean;

   function Maximal_Transfer_Size (D : in out Device) return Unsigned_Long;

   procedure Read (D : in out Device; B : Buffer; R : Request);

   procedure Sync (D : in out Device; R : Request);

   procedure Write (D : in out Device; B : Buffer; R : Request);

end Block.Server;
