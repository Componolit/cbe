
with Cai.Log;
with Cai.Block;
with Cai.Block.Client;
with Ringbuffer;

generic
   with package Client is new Cai.Block.Client (<>);
   with function Next (Current : Cai.Block.Id) return Cai.Block.Id;
   with procedure PR_Block (B : in out Cai.Block.Buffer);
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
      Data : Ring.Ringbuffer;
   end record;

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

end Test;
