
with Cai.Block;

generic
   type Index is mod <>;
   type Buffer is private;
package Ringbuffer is

   type Item is record
      Block : Cai.Block.Id;
      Set : Boolean;
      Data : Buffer;
   end record;

   type Ring is array (Index) of Item;

   type Ringbuffer is record
      Read : Index;
      Write : Index;
      Data : Ring;
   end record;

   function Free (R : Ringbuffer) return Boolean;

   function Has_Block (R : Ringbuffer; B : Cai.Block.Id) return Boolean;

   function Block_Ready (R : Ringbuffer) return Boolean;

   procedure Initialize (R : out Ringbuffer);

   procedure Add (R : in out Ringbuffer; B : Cai.Block.Id) with
      Pre => Free (R) and not Has_Block (R, B);

   procedure Set_Data (R : in out Ringbuffer; B : Cai.Block.Id; Buf : Buffer) with
      Pre => Has_Block (R, B);

   procedure Get_Block (R : in out Ringbuffer; B : out Cai.Block.Id; Buf : out Buffer) with
      Pre => Block_Ready (R);

end Ringbuffer;

