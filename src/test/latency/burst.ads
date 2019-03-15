
with Ada.Real_Time;
with Cai.Block;

package Burst is

   use Cai.Block;

   type Request is record
      Block : Cai.Block.Id;
      Start : Ada.Real_Time.Time;
      Finish : Ada.Real_Time.Time;
   end record;

   type Burst is array (Cai.Block.Id range <>) of Request;

   procedure Start (Item : Cai.Block.Id; Offset : Cai.Block.Count; Data : in out Burst) with
      Pre => Item - Offset in Data'Range;

   procedure Finish (Item : Cai.Block.Id; Offset : Cai.Block.Count; Data : in out Burst) with
      Pre => Item - Offset in Data'Range;

end Burst;
