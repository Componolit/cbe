with Cai.Block;

package Cai.Component is

   type Block_Server_Device is record
      Context : Cai.Block.Context;
      Block_Count : Cai.Block.Count;
      Block_Size : Cai.Block.Size;
   end record;

end Cai.Component;
