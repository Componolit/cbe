with Cbe.Genode;

package Cbe.Block
   with SPARK_Mode => On
is
   package Client
      with SPARK_Mode => On
   is
      type Kind is (None, Read, Write, Sync)
      with Size => Cbe.Unsigned_Int'Size;
      for Kind use (None => 0, Read => 1, Write => 2, Sync => 3);
      package Request
         with SPARK_Mode => On
      is
         type Class is
         record
            Kind : Cbe.Block.Client.Kind;
            Uid : Cbe.Genode.Uint8_T_Array (1 .. 16);
            Start : Cbe.Genode.Uint64_T;
            Length : Cbe.Genode.Uint64_T;
            Success : Cbe.Bool;
         end record;
         pragma Convention (C_Pass_By_Copy, Class);

         type Class_Address is private;
         type Class_Array is array (Natural range <>) of Class;
         type Class_Address_Array is array (Natural range <>) of Class_Address;

--         function Constructor return Class
--         with Global => null;
--         pragma Cpp_Constructor (Constructor, "_ZN5Block6Client7RequestC1Ev");

      private
         pragma SPARK_Mode (Off);

         type Class_Address is access Class;

      end Request;
      Block_Size : constant := 512;
      type Private_Uint64_T is limited private;
      type Private_Uint64_T_Address is limited private;
      type Private_Uint64_T_Array is array (Natural range <>) of Private_Uint64_T;
      type Private_Uint64_T_Address_Array is array (Natural range <>) of Private_Uint64_T_Address;

      type Class is
      limited record
         Private_X_Device : Private_Uint64_T;
      end record
      with Import, Convention => CPP;

      type Class_Address is private;
      type Class_Array is array (Natural range <>) of Class;
      type Class_Address_Array is array (Natural range <>) of Class_Address;

      function Constructor return Class
      with Global => null;
      pragma Cpp_Constructor (Constructor, "_ZN5Block6ClientC1Ev");

      procedure Initialize (This : Class; Device : Cbe.Char_Array)
      with Global => null, Import, Convention => CPP, External_Name => "_ZN5Block6Client10initializeEPKc";

      procedure Finalize (This : Class)
      with Global => null, Import, Convention => CPP, External_Name => "_ZN5Block6Client8finalizeEv";

      procedure Submit_Read (This : Class; Req : Cbe.Block.Client.Request.Class)
      with Global => null, Import, Convention => CPP, External_Name => "_ZN5Block6Client11submit_readENS0_7RequestE";

      procedure Submit_Sync (This : Class; Req : Cbe.Block.Client.Request.Class)
      with Global => null, Import, Convention => CPP, External_Name => "_ZN5Block6Client11submit_syncENS0_7RequestE";

      procedure Submit_Write (This : Class; Req : Cbe.Block.Client.Request.Class; Data : in out Cbe.Genode.Uint8_T_Array; Length : Cbe.Genode.Uint64_T)
      with Global => null, Import, Convention => CPP, External_Name => "_ZN5Block6Client12submit_writeENS0_7RequestEPhy";

      function Next (This : Class) return Cbe.Block.Client.Request.Class
      with Global => null, Import, Convention => CPP, External_Name => "_ZN5Block6Client4nextEv";

      procedure Acknowledge_Read (This : Class; Req : Cbe.Block.Client.Request.Class; Data : in out Cbe.Genode.Uint8_T_Array; Length : Cbe.Genode.Uint64_T)
      with Global => null, Import, Convention => CPP, External_Name => "_ZN5Block6Client16acknowledge_readENS0_7RequestEPhy";

      procedure Acknowledge_Sync (This : Class; Req : Cbe.Block.Client.Request.Class)
      with Global => null, Import, Convention => CPP, External_Name => "_ZN5Block6Client16acknowledge_syncENS0_7RequestE";

      procedure Acknowledge_Write (This : Class; Req : Cbe.Block.Client.Request.Class)
      with Global => null, Import, Convention => CPP, External_Name => "_ZN5Block6Client17acknowledge_writeENS0_7RequestE";

   private
      pragma SPARK_Mode (Off);

      type Class_Address is access Class;
      type Private_Uint64_T is new Cbe.Genode.Uint64_T;
      type Private_Uint64_T_Address is access Private_Uint64_T;
   end Client;

end Cbe.Block;
