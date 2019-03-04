with Ada.Unchecked_Conversion;
with Block;
with Block.Server;
with Cxx.Genode;
with Component;
use all type Cxx.Genode.Uint64_T;

package body Cxx.Block is

   package body Server is

      procedure Ack (D : in out Component.Block_Device; R : Standard.Block.Request)
      is
      begin
         null;
      end Ack;

      package Server_Component is new Standard.Block.Server (Component.Block_Device, Ack);

      procedure Initialize (This : in out Class; Label : Cxx.Char_Array; Length : Cxx.Genode.Uint64_T; Session : Cxx.Void_Address)
      is
         subtype C_Str is Cxx.Char_Array (1 .. Integer (Length));
         subtype L_Str is String (1 .. Integer (Length));
         function Convert_String is new Ada.Unchecked_Conversion (C_Str, L_Str);
      begin
         This.Session := Session;
         Malloc_State (This, This.State, Component.Block_Device'Size / 8);
         declare
            Dev : Component.Block_Device
            with Address => This.State;
         begin
            Server_Component.Initialize (Dev, Convert_String (Label (1 .. Integer (Length))));
         end;
      end Initialize;

      procedure Finalize (This : in out Class) is
      begin
         null;
      end Finalize;

      function Block_Count (This : Class) return Cxx.Genode.Uint64_T is
      begin
         return 0;
      end Block_Count;

      function Block_Size (This : Class) return Cxx.Genode.Uint64_T is
      begin
         return 0;
      end Block_Size;

      function Writable (This : Class) return Cxx.Bool is
      begin
         return 0;
      end Writable;

      function Maximal_Transfer_Size (This : Class) return Cxx.Genode.Uint64_T is
      begin
         return 0;
      end Maximal_Transfer_Size;

      procedure Read (This : Class;
                      Buffer : Cxx.Genode.Uint8_T_Array;
                      Size : Cxx.Genode.Uint64_T;
                      Req : in out Cxx.Block.Request.Class) is
      begin
         null;
      end Read;

      procedure Sync (This : Class; Req : in out Cxx.Block.Request.Class) is
      begin
         null;
      end Sync;

      procedure Write (This : Class;
                       Buffer : Cxx.Genode.Uint8_T_Array;
                       Size : Cxx.Genode.Uint64_T;
                       Req : in out Cxx.Block.Request.Class) is
      begin
         null;
      end Write;

   end Server;

end Cxx.Block;
