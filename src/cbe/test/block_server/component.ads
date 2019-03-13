
with Cai.Block;
with Cai.Block.Server;
with Cai.Block.Dispatcher;

package Component is

   procedure Construct
      with
      Export,
      Convention => C,
      External_Name => "ada_component_construct";

   procedure Event;
   function Block_Count (S : Cai.Block.Server_Instance) return Cai.Block.Count;
   function Block_Size (S : Cai.Block.Server_Instance) return Cai.Block.Size;
   function Writable (S : Cai.Block.Server_Instance) return Boolean;
   function Maximal_Transfer_Size (S : Cai.Block.Server_Instance) return Cai.Block.Unsigned_Long;
   procedure Initialize (S : Cai.Block.Server_Instance; L : String);
   procedure Finalize (S : Cai.Block.Server_Instance);

   procedure Request;

   package Block_Server is new Cai.Block.Server (Event,
                                                 Block_Count,
                                                 Block_Size,
                                                 Writable,
                                                 Maximal_Transfer_Size,
                                                 Initialize,
                                                 Finalize);
   package Block_Dispatcher is new Cai.Block.Dispatcher (Block_Server, Request);

end Component;
