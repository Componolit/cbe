
with Cai.Block;
with Cai.Block.Server;
with Cai.Block.Dispatcher;

package Component is

   type State is record
      Dispatcher : Cai.Block.Dispatcher_Session;
      Server : Cai.Block.Server_Session;
   end record;

   procedure Construct
      with
      Export,
      Convention => C,
      External_Name => "ada_component_construct";

   procedure Event (S : in out State);
   function Block_Count (S : State) return Cai.Block.Count;
   function Block_Size (S : State) return Cai.Block.Size;
   function Writable (S : State) return Boolean;
   function Maximal_Transfer_Size (S : State) return Cai.Block.Unsigned_Long;
   procedure Initialize (S : in out Cai.Block.Server_Session; L : String; C : in out State);
   procedure Finalize (S : in out Cai.Block.Server_Session);

   procedure Request (S : in out State);

   package Block_Server is new Cai.Block.Server (State,
                                                 Event,
                                                 Block_Count,
                                                 Block_Size,
                                                 Writable,
                                                 Maximal_Transfer_Size,
                                                 Initialize,
                                                 Finalize);
   package Block_Dispatcher is new Cai.Block.Dispatcher (Block_Server, Request);

end Component;
