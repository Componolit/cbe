
with Cbe;
with Cbe.Block;

package Internals.Block is

   type Private_Data is new Cbe.Genode_Uint8_T_Array (1 .. 16);
   type Device is limited record
      Instance : Cbe.Block.Client.Class;
   end record;

end Internals.Block;
