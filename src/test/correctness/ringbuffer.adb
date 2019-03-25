
with Cai.Block;
with Cai.Log;
with Cai.Log.Client;

use all type Cai.Block.Id;

package body Ringbuffer is

   function Free (R : Ringbuffer) return Boolean
   is
   begin
      return R.Data (R.Write).Block = Cai.Block.Id'Last;
   end Free;

   function Has_Block (R : Ringbuffer; B : Cai.Block.Id) return Boolean
   is
   begin
      for I of R.Data loop
         if I.Block = B then
            return True;
         end if;
      end loop;
      return False;
   end Has_Block;

   function Block_Ready (R : Ringbuffer) return Boolean
   is
   begin
      return R.Data (R.Read).Set;
   end Block_Ready;

   procedure Initialize (R : out Ringbuffer)
   is
   begin
      R.Read := Index'First;
      R.Write := Index'First;
      for I in R.Data'Range loop
         R.Data (I).Block := Cai.Block.Id'Last;
         R.Data (I).Set := False;
      end loop;
   end Initialize;

   procedure Add (R : in out Ringbuffer; B : Cai.Block.Id)
   is
   begin
      R.Data (R.Write).Block := B;
      R.Data (R.Write).Set := False;
      R.Write := R.Write + 1;
   end Add;

   procedure Set_Data (R : in out Ringbuffer; B : Cai.Block.Id; Buf : Buffer)
   is
   begin
      for I in R.Data'Range loop
         if R.Data (I).Block = B then
            R.Data (I).Set := True;
            R.Data (I).Data := Buf;
            return;
         end if;
      end loop;
   end Set_Data;

   procedure Get_Block (R : in out Ringbuffer; B : out Cai.Block.Id; Buf : out Buffer)
   is
   begin
      B := R.Data (R.Read).Block;
      Buf := R.Data (R.Read).Data;
      R.Data (R.Read).Block := Cai.Block.Id'Last;
      R.Data (R.Read).Set := False;
      R.Read := R.Read + 1;
   end Get_Block;

end Ringbuffer;
