with Block;

package Component is

   type Block_Device is record
      Context : Block.Context;
      Block_Count : Block.Count;
      Block_Size : Block.Size;
   end record;

end Component;
