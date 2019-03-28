with Ada.Real_Time;
with Cai.Block;
with Cai.Block.Client;
with Cai.Log;

use all type Cai.Block.Id;
use all type Cai.Block.Count;

generic
   with package Client is new Cai.Block.Client (<>);
   Request_Count : Cai.Block.Count;
   Operation     : Cai.Block.Request_Kind;
package Iteration is

   type Request is record
      Start : Ada.Real_Time.Time;
      Finish : Ada.Real_Time.Time;
      Success : Boolean;
   end record;

   type Burst is array (Long_Integer range <>) of Request;

   type Test is record
      Sent      : Long_Integer;
      Received  : Long_Integer;
      Offset    : Cai.Block.Count;
      Finished  : Boolean;
      Sync      : Boolean;
      Buffer    : Cai.Block.Buffer (1 .. 4096);
      Data      : Burst (0 .. Long_Integer (Request_Count - 1));
   end record;

   procedure Initialize (T : out Test; Offset : Cai.Block.Count; Sync : Boolean);

   procedure Send (C : in out Cai.Block.Client_Session; T : in out Test; Log : in out Cai.Log.Client_Session);

   procedure Receive (C : in out Cai.Block.Client_Session; T : in out Test; Log : in out Cai.Log.Client_Session);

   procedure Xml (Xml_Log : in out Cai.Log.Client_Session; B : Burst; Offset : Cai.Block.Count);

end Iteration;
