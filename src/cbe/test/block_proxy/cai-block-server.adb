with Cai.Block.Client;
with Gnat.Io;

package body Cai.Block.Server is

   type Read_Entry is record
      Free : Boolean;
      Block : Id;
      Length : Count;
      Address : System.Address;
   end record;

   type Read_List is array (Integer range <>) of Read_Entry;

   Queue : Read_List (1 .. 5) := (others => (True, 0, 0, System.Null_Address));

   procedure Store (R : in out Request; A : System.Address)
   is
   begin
      R.Status := Cai.Block.Error;
      for I in Queue'Range loop
         if Queue (I).Free then
            R.Status := Cai.Block.Ok;
            Queue (I).Free := False;
            Queue (I).Block := R.Start;
            Queue (I).Length := R.Length;
            Queue (I).Address := A;
            exit;
         end if;
      end loop;
   end Store;

   procedure Load (R : in out Request; A : out System.Address)
   is
   begin
      R.Status := Cai.Block.Error;
      for I in Queue'Range loop
         if not Queue (I).Free and Queue (I).Block = R.Start and Queue (I).Length = R.Length then
            A := Queue (I).Address;
            R.Status := Ok;
            Queue (I).Free := True;
            exit;
         end if;
      end loop;
   end Load;

   procedure Callback (D : in out Component.Block_Server_Device);

   package Block_Client is new Cai.Block.Client (Component.Block_Server_Device, Callback);

   procedure Callback (D : in out Component.Block_Server_Device)
   is
   begin
      loop
         declare
            R : Request := Block_Client.Next (D.Client);
         begin
            case R.Kind is
               when None =>
                  exit;
               when Write =>
                  Acknowledge (D, R, D.Context);
               when others =>
                  null;
            end case;
         end;
      end loop;
   end Callback;

   procedure Initialize (D : out Component.Block_Server_Device; L : String; C : Context)
   is
   begin
      D.Context := C;
      Block_Client.Initialize_Device (D.Client, L, D);
   end Initialize;

   procedure Finalize (D : in out Component.Block_Server_Device)
   is
   begin
      Block_Client.Finalize_Device (D.Client);
   end Finalize;

   function Block_Count (D : Component.Block_Server_Device) return Count
   is
   begin
      return Block_Client.Block_Count (D.Client);
   end Block_Count;

   function Block_Size (D : Component.Block_Server_Device) return Size
   is
   begin
      return Block_Client.Block_Size (D.Client);
   end Block_Size;

   function Writable (D : Component.Block_Server_Device) return Boolean
   is
   begin
      return Block_Client.Writable (D.Client);
   end Writable;

   function Maximal_Transfer_Size (D : Component.Block_Server_Device) return Unsigned_Long
   is
   begin
      return Block_Client.Maximal_Transfer_Size (D.Client);
   end Maximal_Transfer_Size;

   procedure Read (D : in out Component.Block_Server_Device; B : System.Address; L : Unsigned_Long; R : in out Request)
   is
      Acked : Boolean := False;
      Load_Addr : System.Address;
   begin
      Store (R, B);
      Block_Client.Submit_Read (D.Client, R);
      while not Acked loop
         declare
            N : Request := Block_Client.Next (D.Client);
         begin
            if N.Kind = Cai.Block.Read then
               if N.Status = Cai.Block.Ok then
                  Load (N, Load_Addr);
                  declare
                     Buf : Buffer (1 .. L)
                     with Address => Load_Addr;
                  begin
                     Block_Client.Read (D.Client, N, Buf);
                  end;
               end if;
               R.Status := N.Status;
               Acknowledge (D, R, D.Context);
               Acked := True;
            end if;
         end;
      end loop;
   end Read;

   procedure Write (D : in out Component.Block_Server_Device; B : Buffer; R : in out Request)
   is
      Acked : Boolean := False;
   begin
      Block_Client.Submit_Write (D.Client, R, B);
      R.Status := Cai.Block.Ok;
   end Write;

   procedure Sync (D : in out Component.Block_Server_Device)
   is
   begin
      Block_Client.Sync (D.Client);
   end Sync;

end Cai.Block.Server;
