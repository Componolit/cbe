with Cai.Block.Client;

package body Cai.Block.Server is

   procedure Initialize (D : out Component.Block_Server_Device; L : String; C : Context)
   is
   begin
      D.Context := C;
      Cai.Block.Client.Initialize_Device (D.Client, L);
   end Initialize;

   procedure Finalize (D : in out Component.Block_Server_Device)
   is
   begin
      Cai.Block.Client.Finalize_Device (D.Client);
   end Finalize;

   function Block_Count (D : Component.Block_Server_Device) return Count
   is
   begin
      return Cai.Block.Client.Block_Count (D.Client);
   end Block_Count;

   function Block_Size (D : Component.Block_Server_Device) return Size
   is
   begin
      return Cai.Block.Client.Block_Size (D.Client);
   end Block_Size;

   function Writable (D : Component.Block_Server_Device) return Boolean
   is
   begin
      return Cai.Block.Client.Writable (D.Client);
   end Writable;

   function Maximal_Transfer_Size (D : Component.Block_Server_Device) return Unsigned_Long
   is
   begin
      return Cai.Block.Client.Maximal_Transfer_Size (D.Client);
   end Maximal_Transfer_Size;

   procedure Read (D : in out Component.Block_Server_Device; B : out Buffer; R : in out Request)
   is
      Req : constant Request := R;
      Acked : Boolean := False;
   begin
      Cai.Block.Client.Submit_Read (D.Client, Req);
      while not Acked loop
         declare
            N : Request := Cai.Block.Client.Next (D.Client);
         begin
            if N.Kind = Cai.Block.Read then
               if N.Status = Cai.Block.Ok then
                  Cai.Block.Client.Read (D.Client, N, B);
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
      Req : constant Request := R;
      Acked : Boolean := False;
   begin
      Cai.Block.Client.Submit_Write (D.Client, Req, B);
      while not Acked loop
         declare
            N : constant Request := Cai.Block.Client.Next (D.Client);
         begin
            if N.Kind = Cai.Block.Write then
               R.Status := N.Status;
               Acknowledge (D, R, D.Context);
               Acked := True;
            end if;
         end;
      end loop;
   end Write;

   procedure Sync (D : in out Component.Block_Server_Device)
   is
   begin
      Cai.Block.Client.Sync (D.Client);
   end Sync;

end Cai.Block.Server;
