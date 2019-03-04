with Cxx.Genode;

package Cxx.Block
   with SPARK_Mode => On
is
   type Kind is (None, Read, Write, Sync)
   with Size => Cxx.Unsigned_Int'Size;
   for Kind use (None => 0, Read => 1, Write => 2, Sync => 3);
   type Status is (Raw, Ok, Error, Ack)
   with Size => Cxx.Unsigned_Int'Size;

   package Request
      with SPARK_Mode => On
   is
      type Class is
      record
         Kind : Cxx.Block.Kind;
         Uid : Cxx.Genode.Uint8_T_Array (1 .. 16);
         Start : Cxx.Genode.Uint64_T;
         Length : Cxx.Genode.Uint64_T;
         Status : Cxx.Block.Status;
      end record;
      pragma Convention (C_Pass_By_Copy, Class);

   end Request;

   package Server
      with SPARK_Mode => On
   is
      type Class is
      limited record
         Session : Cxx.Void_Address;
         State : Cxx.Void_Address;
      end record
      with Import, Convention => CPP;

      function Constructor return Class
      with Global => null;
      pragma Cpp_Constructor (Constructor, "_ZN3Cai5Block6ServerC1Ev");

      procedure Initialize (This : in out Class; Label : Cxx.Char_Array; Length : Cxx.Genode.Uint64_T; Session : Cxx.Void_Address)
      with Global => null, Export, Convention => CPP, External_Name => "_ZN3Cai5Block6Server10initializeEPKcyPv";

      procedure Finalize (This : in out Class)
      with Global => null, Export, Convention => CPP, External_Name => "_ZN3Cai5Block6Server8finalizeEv";

      function Block_Count (This : Class) return Cxx.Genode.Uint64_T
      with Global => null, Export, Convention => CPP, External_Name => "_ZN3Cai5Block6Server11block_countEv";

      function Block_Size (This : Class) return Cxx.Genode.Uint64_T
      with Global => null, Export, Convention => CPP, External_Name => "_ZN3Cai5Block6Server10block_sizeEv";

      function Writable (This : Class) return Cxx.Bool
      with Global => null, Export, Convention => CPP, External_Name => "_ZN3Cai5Block6Server8writableEv";

      function Maximal_Transfer_Size (This : Class) return Cxx.Genode.Uint64_T
      with Global => null, Export, Convention => CPP, External_Name => "_ZN3Cai5Block6Server21maximal_transfer_sizeEv";

      procedure Read (This : Class; Buffer : Cxx.Genode.Uint8_T_Array; Size : Cxx.Genode.Uint64_T; Req : in out Cxx.Block.Request.Class)
      with Global => null, Export, Convention => CPP, External_Name => "_ZN3Cai5Block6Server4readEPhyRNS0_7RequestE";

      procedure Sync (This : Class; Req : in out Cxx.Block.Request.Class)
      with Global => null, Export, Convention => CPP, External_Name => "_ZN3Cai5Block6Server4syncERNS0_7RequestE";

      procedure Write (This : Class; Buffer : Cxx.Genode.Uint8_T_Array; Size : Cxx.Genode.Uint64_T; Req : in out Cxx.Block.Request.Class)
      with Global => null, Export, Convention => CPP, External_Name => "_ZN3Cai5Block6Server5writeEPhyRNS0_7RequestE";

      procedure Acknowledge (This : Class; Req : in out Cxx.Block.Request.Class)
      with Global => null, Import, Convention => CPP, External_Name => "_ZN3Cai5Block6Server11acknowledgeERNS0_7RequestE";

      procedure Malloc_State (This : Class; State : in out Cxx.Void_Address; Size : Cxx.Genode.Uint64_T)
      with Global => null, Import, Convention => CPP, External_Name => "_ZN3Cai5Block6Server12malloc_stateEPPvy";

      procedure Free_State (This : Class; State : Cxx.Void_Address; Size : Cxx.Genode.Uint64_T)
      with Global => null, Import, Convention => CPP, External_Name => "_ZN3Cai5Block6Server10free_stateEPvy";

   end Server;

end Cxx.Block;
