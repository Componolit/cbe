with Ada.Real_Time;
with Cai.Block;
with Cai.Block.Client;

use all type Cai.Block.Id;
use all type Cai.Block.Count;

generic
   with package Client is new Cai.Block.Client (<>);
   Request_Offset : Cai.Block.Count;
   Request_Count : Cai.Block.Count;
package Test is

   type Request is record
      Block : Cai.Block.Id;
      Start : Ada.Real_Time.Time;
      Finish : Ada.Real_Time.Time;
   end record;

   type Burst is array (Long_Integer range <>) of Request;

   procedure Start (Item : Client.Request; Offset : Cai.Block.Count; Data : in out Burst) with
      Pre => Long_Integer (Item.Start - Offset) in Data'Range;

   procedure Finish (Item : Client.Request; Offset : Cai.Block.Count; Data : in out Burst) with
      Pre => Long_Integer (Item.Start - Offset) in Data'Range;

   procedure Send (C : Cai.Block.Client_Session);

   procedure Receive (C : Cai.Block.Client_Session);

   procedure Evaluate;

end Test;
