
with Cai.Block;
with Cai.Block.Client;
with Iteration;

generic
   with package Client is new Cai.Block.Client (<>);
   Request_Count : Cai.Block.Count;
   Run_Count : Positive;
   Operation : Cai.Block.Request_Kind;
package Run is

   package Iter is new Iteration (Client, Request_Count, Operation);

   type Run_Type is array (1 .. Run_Count) of Iter.Test;

   function Create return Run_Type;

   procedure Initialize (R : in out Run_Type);

   procedure Run (C : in out Cai.Block.Client_Session; R : in out Run_Type);

   function Finished (R : Run_Type) return Boolean;

end Run;
