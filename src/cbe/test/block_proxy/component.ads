
with Cai.Block;
with Cai.Block.Client;
with Cai.Block.Dispatcher;
with Cai.Block.Server;

package Component is

   procedure Construct with
      Export,
      Convention => C,
      External_Name => "ada_component_construct";

   procedure Event;
   procedure Dispatch;
   procedure Initialize_Server (S : Cai.Block.Server_Instance; L : String);
   procedure Finalize_Server (S : Cai.Block.Server_Instance);
   function Block_Count (S : Cai.Block.Server_Instance) return Cai.Block.Count;
   function Block_Size (S : Cai.Block.Server_Instance) return Cai.Block.Size;
   function Writable (S : Cai.Block.Server_Instance) return Boolean;
   function Maximal_Transfer_Size (S : Cai.Block.Server_Instance) return Cai.Block.Unsigned_Long;

   package Block_Client is new Cai.Block.Client (Event);
   package Block_Server is new Cai.Block.Server (Event,
                                                 Block_Count,
                                                 Block_Size,
                                                 Writable,
                                                 Maximal_Transfer_Size,
                                                 Initialize_Server,
                                                 Finalize_Server);
   package Block_Dispatcher is new Cai.Block.Dispatcher (Block_Server, Dispatch);

end Component;
