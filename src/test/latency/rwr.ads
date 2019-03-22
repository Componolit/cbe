
with Cai.Block;
with Cai.Block.Client;
with Cai.Log;
with Run;

generic
   with Package Client is new Cai.Block.Client (<>);
   Request_Count : Cai.Block.Count;
   Iterations : Positive;
package Rwr is

   package RR1 is new Run (Client, Request_Count, Iterations, Cai.Block.Read);
   package WR is new Run (Client, Request_Count, Iterations, Cai.Block.Write);
   package RR2 is new Run (Client, Request_Count, Iterations, Cai.Block.Read);

   type Rwr_Run is limited record
      R1 : RR1.Run_Type;
      W : WR.Run_Type;
      R2 : RR2.Run_Type;
   end record;

   procedure Initialize (R : out Rwr_Run);

   procedure Run (C : in out Cai.Block.Client_Session; R : in out Rwr_Run; Log : in out Cai.Log.Client_Session);

   function Finished (R : Rwr_Run) return Boolean;

   procedure Xml (Xml_Log : in out Cai.Log.Client_Session; R : Rwr_Run; Log : in out Cai.Log.Client_Session);

end Rwr;
