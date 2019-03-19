with Ada.Real_Time;
with Cai.Block;
with Cai.Block.Client;

use all type Cai.Block.Id;
use all type Cai.Block.Count;

generic
   with package Client is new Cai.Block.Client (<>);
   Request_Count : Cai.Block.Count;
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
      Offset    : Cai.Block.Count;
      Operation : Cai.Block.Request_Kind;
      Finished  : Boolean;
      Buffer    : Cai.Block.Buffer (1 .. 4096);
      Data      : Burst (0 .. Long_Integer (Request_Count - 1));
   end record;

   function Create (Offset    : Cai.Block.Count;
                    Operation : Cai.Block.Request_Kind) return Test;

   procedure Send (C : Cai.Block.Client_Session; T : in out Test);

   procedure Receive (C : Cai.Block.Client_Session; T : in out Test);

end Iteration;
