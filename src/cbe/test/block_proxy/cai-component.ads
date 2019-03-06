with Cai.Block.Client;

package Cai.Component is

   type Block_Server_Device is record
      Context : Cai.Block.Context;
      Client : Cai.Block.Client.Device;
   end record;

end Cai.Component;
