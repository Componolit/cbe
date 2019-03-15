
package body Burst is

   procedure Start (Item : Cai.Block.Id; Offset : Cai.Block.Count; Data : in out Burst)
   is
   begin
      Data (Item - Offset).Start := Ada.Real_Time.Clock;
   end Start;

   procedure Finish (Item : Cai.Block.Id; Offset : Cai.Block.Count; Data : in out Burst)
   is
   begin
      Data (Item - Offset).Finish := Ada.Real_Time.Clock;
   end Finish;

end Burst;
