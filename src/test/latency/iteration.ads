with Ada.Real_Time;
with Cai.Block;
with Cai.Block.Client;

use all type Cai.Block.Id;
use all type Cai.Block.Count;

generic
   with package Client is new Cai.Block.Client (<>);
package Iteration is

   type Request is record
      Block : Cai.Block.Id;
      Start : Ada.Real_Time.Time;
      Finish : Ada.Real_Time.Time;
   end record;

   type Burst is array (Long_Integer range <>) of Request;

   type Test is limited record
      Sent      : Long_Integer;
      Received  : Long_Integer;
      Count     : Cai.Block.Count;
      Offset    : Cai.Block.Count;
      Operation : Cai.Block.Request_Kind;
      Finished  : Boolean;
      Buffer    : Cai.Block.Buffer (1 .. 4096);
      Data      : Burst (1 .. Long_Integer (Request_Count - 1));
   end record;

   function Create (Count : Cai.Block.Count; Offset : Cai.Block.Count) return Test;

   procedure Send (C : Cai.Block.Client_Session; T : Test);

   procedure Receive (C : Cai.Block.Client_Session; T : Test);

end Iteration;
