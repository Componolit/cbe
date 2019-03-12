
with Gnat.Io;

with Cai.Block;
use all type Cai.Block.Id;
use all type Cai.Block.Count;
use all type Cai.Block.Unsigned_Long;
use all type Cai.Block.Request_Kind;
use all type Cai.Block.Request_Status;

package body Component is

   Component_State : State := (Dispatcher => Block_Dispatcher.Create,
                               Server => Block_Server.Create);

   subtype Block is Cai.Block.Buffer (1 .. 512);
   type Disk is array (Cai.Block.Id range 0 .. 1023) of Block;

   Ram_Disk : Disk;

   procedure Construct
   is
   begin
      Block_Dispatcher.Initialize (Component_State.Dispatcher, Component_State);
      Block_Dispatcher.Register (Component_State.Dispatcher);
   end Construct;

   procedure Read (S : in out State; R : in out Block_Server.Request)
   is
      Buf : Cai.Block.Buffer (1 .. Cai.Block.Unsigned_Long (R.Length * Cai.Block.Count (Block_Size (S))));
      Success : Boolean;
   begin
      if Buf'Length mod Block'Length = 0 and then
         R.Start in Ram_Disk'Range and then
         R.Start + Cai.Block.Id (R.Length) - 1 in Ram_Disk'Range
      then
         for I in Cai.Block.Id range R.Start .. R.Start + Cai.Block.Id (R.Length) - 1 loop
            Buf (Buf'First + Cai.Block.Unsigned_Long (I - R.Start) * Block'Length ..
               Buf'First + Cai.Block.Unsigned_Long (I - R.Start + 1) * Block'Length - 1) := Ram_Disk (I);
         end loop;
         Block_Server.Read (S.Server, R, Buf, Success);
         R.Status := (if Success then Cai.Block.Ok else Cai.Block.Error);
      else
         R.Status := Cai.Block.Error;
      end if;
   end Read;

   procedure Write (S : in out State; R : in out Block_Server.Request)
   is
      B : Cai.Block.Buffer (1 .. Cai.Block.Unsigned_Long (R.Length * Cai.Block.Count (Block_Size (S))));
      Success : Boolean;
   begin
      R.Status := Cai.Block.Error;
      if
         B'Length mod Block'Length = 0 and then
         R.Start in Ram_Disk'Range and then
         R.Start + Cai.Block.Id (R.Length) - 1 in Ram_Disk'Range
      then
         Block_Server.Write (S.Server, R, B, Success);
         if Success then
            for I in Cai.Block.Id range R.Start .. R.Start + Cai.Block.Id (R.Length) - 1 loop
               Ram_Disk (I) :=
                  B (B'First + Cai.Block.Unsigned_Long (I - R.Start) * Block'Length ..
                     B'First + Cai.Block.Unsigned_Long (I - R.Start + 1) * Block'Length - 1);
            end loop;
            R.Status := Cai.Block.Ok;
         end if;
      end if;
   end Write;

   procedure Event (S : in out State)
   is
      R : Block_Server.Request;
   begin
      if Block_Server.Initialized (S.Server) then
         loop
            Block_Server.Next_Request (S.Server, R);
            case R.Kind is
               when Cai.Block.Read =>
                  Read (S, R);
                  while R.Status /= Cai.Block.Acknowledged loop
                     Block_Server.Acknowledge (S.Server, R);
                  end loop;
               when Cai.Block.Write =>
                  Write (S, R);
                  while R.Status /= Cai.Block.Acknowledged loop
                     Block_Server.Acknowledge (S.Server, R);
                  end loop;
               when others => null;
            end case;
            exit when R.Kind = Cai.Block.None;
         end loop;
      end if;
   end Event;

   function Block_Count (S : State) return Cai.Block.Count
   is
   begin
      return Cai.Block.Count (Ram_Disk'Length);
   end Block_Count;

   function Block_Size (S : State) return Cai.Block.Size
   is
   begin
      return Cai.Block.Size (Block'Length);
   end Block_Size;

   function Writable (S : State) return Boolean
   is
   begin
      return True;
   end Writable;

   function Maximal_Transfer_Size (S : State) return Cai.Block.Unsigned_Long
   is
   begin
      return 16#ffffffff#;
   end Maximal_Transfer_Size;

   procedure Initialize (S : in out Cai.Block.Server_Session; L : String; C : in out State)
   is
   begin
      Gnat.Io.Put_Line ("Server initialize with label: " & L);
      Ram_Disk := (others => (others => 0));
   end Initialize;

   procedure Finalize (S : in out Cai.Block.Server_Session)
   is
   begin
      null;
   end Finalize;

   procedure Request (S : in out State)
   is
      Label : String (1 .. 160);
      Last : Natural;
      Valid : Boolean;
   begin
      Block_Dispatcher.Session_Request (S.Dispatcher, Valid, Label, Last);
      if Valid and not Block_Server.Initialized (S.Server) then
         Gnat.Io.Put_Line ("Received request with label " & Label);
         Block_Dispatcher.Session_Accept (S.Dispatcher, S.Server, Label, S);
      end if;
      Block_Dispatcher.Session_Cleanup (S.Dispatcher, S.Server);
   end Request;

end Component;
