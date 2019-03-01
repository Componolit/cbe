with Block;

package Component is

   type Block_Device is limited record
      Block_Count : Block.Count;
      Block_Size : Block.Size;
   end record;

end Component;
