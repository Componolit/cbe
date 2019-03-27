
with Cai.Log;
with Cai.Block;
with Cai.Block.Client;
with LSC.Internal.SHA256;
with Ringbuffer;

generic
   with package Client is new Cai.Block.Client (<>);
   with function Next (Current : Cai.Block.Id) return Cai.Block.Id;
   with procedure PR_Block (B : in out Cai.Block.Buffer; Id : Cai.Block.Id);
package Test is

   type Buffer_Index is mod 256;
   subtype Block is Cai.Block.Buffer (1 .. 4096);

   package Ring is new Ringbuffer (Buffer_Index, Block);

   type Test_State is record
      Last : Cai.Block.Id;
      Sent : Cai.Block.Count;
      Written : Cai.Block.Count;
      Read : Cai.Block.Count;
      Count : Cai.Block.Count;
      Bounds_Checked : Boolean;
      Compared : Boolean;
      Write_Context : LSC.Internal.SHA256.Context_Type;
      Read_Context : LSC.Internal.SHA256.Context_Type;
      Data : Ring.Ringbuffer;
   end record;

   function Byte_Image (Bytes : Long_Integer) return String;

   procedure Initialize (C : in out Cai.Block.Client_Session;
                         T : out Test_State;
                         L : in out Cai.Log.Client_Session);

   procedure Bounds_Check (C : in out Cai.Block.Client_Session;
                           T : in out Test_State;
                           Success : out Boolean;
                           L : in out Cai.Log.Client_Session);

   function Bounds_Check_Finished (T : Test_State) return Boolean;

   procedure Write (C : in out Cai.Block.Client_Session;
                    T : in out Test_State;
                    Success : out Boolean;
                    L : in out Cai.Log.Client_Session);

   function Write_Finished (T : Test_State) return Boolean;

   procedure Read (C : in out Cai.Block.Client_Session;
                   T : in out Test_State;
                   Success : out Boolean;
                   L : in out Cai.Log.Client_Session);

   function Read_Finished (T : Test_State) return Boolean;

   procedure Compare (T : in out Test_State; Equal : out Boolean);

   function Compare_Finished (T : Test_State) return Boolean;

end Test;
