with Cai.Block;

package Cai.Component is

   type Block_Server_Device is record
      Context : Cai.Block.Context;
      Client : Block.Device;
   end record;

end Cai.Component;
