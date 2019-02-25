
with Ada.Unchecked_Conversion;
with Cbe.Genode;
use all type Cbe.Bool;

package body Block.Client is

   function Create_Device return Device
   is
   begin
      return Device' (Instance => Cbe.Block.Client.Constructor);
   end Create_Device;

   procedure Initialize_Device (D : in out Device; Path : String)
   is
      C_Path : constant String := Path & Character'Val(0);
      subtype C_Path_String is String (1 .. C_Path'Length);
      subtype C_String is Cbe.Char_Array (1 .. C_Path'Length);
      function To_C_String is new Ada.Unchecked_Conversion (C_Path_String, C_String);
   begin

      Cbe.Block.Client.Initialize (D.Instance, To_C_String (C_Path));
   end Initialize_Device;

   procedure Finalize_Device (D : in out Device)
   is
   begin
      Cbe.Block.Client.Finalize (D.Instance);
   end Finalize_Device;

   function Convert_Request (R : Request) return Cbe.Block.Client.Request.Class
   is
   begin
      return Cbe.Block.Client.Request.Class'(
         Kind => (case R.Kind is
                  when None => Cbe.Block.Client.None,
                  when Read => Cbe.Block.Client.Read,
                  when Write => Cbe.Block.Client.Write,
                  when Sync => Cbe.Block.Client.Sync),
         Uid => Cbe.Unsigned_Char_Array (R.Priv),
         Start => (case R.Kind is
                  when None | Sync => 0,
                  when Read | Write => Cbe.Genode.Uint64_T (R.Start)),
         Length => (case R.Kind is
                  when None | Sync => 0,
                  when Read | Write => Cbe.Genode.Uint64_T (R.Length)),
         Success => (case R.Kind is
                  when None | Sync => 0,
                  when Read | Write => Cbe.Bool (if R.Success then 1 else 0)));
   end Convert_Request;

   function Convert_Request (CR : Cbe.Block.Client.Request.Class) return Request
   is
      R : Request ((case CR.Kind is
                     when Cbe.Block.Client.None => None,
                     when Cbe.Block.Client.Read => Read,
                     when Cbe.Block.Client.Write => Write,
                     when Cbe.Block.Client.Sync => Sync));
   begin
      R.Priv := Private_Data (CR.Uid);
      case R.Kind is
         when None | Sync =>
            null;
         when Read | Write =>
            R.Start := Block_Id (CR.Start);
            R.Length := Block_Count (CR.Length);
            R.Success := (if CR.Success = 0 then False else True);
      end case;
      return R;
   end Convert_Request;

   procedure Submit_Read (D : Device; R : Request)
   is
   begin
      Cbe.Block.Client.Submit_Read (D.Instance, Convert_Request (R));
   end Submit_Read;

   procedure Submit_Sync (D : Device; R : Request)
   is
   begin
      Cbe.Block.Client.Submit_Sync (D.Instance, Convert_Request (R));
   end Submit_Sync;

   procedure Submit_Write (D : Device; R : Request; B : Buffer)
   is
      subtype Local_Buffer is Buffer (1 .. B'Length);
      subtype Local_U8_Array is Cbe.Genode.Uint8_T_Array (1 .. B'Length);
      function Convert_Buffer is new Ada.Unchecked_Conversion (Local_Buffer, Local_U8_Array);
      Data : Local_U8_Array := Convert_Buffer (B);
   begin
      Cbe.Block.Client.Submit_Write (
         D.Instance,
         Convert_Request (R),
         Data,
         Cbe.Genode.Uint64_T (B'Length));
   end Submit_Write;

   function Next (D : Device) return Request
   is
   begin
      return Convert_Request (Cbe.Block.Client.Next (D.Instance));
   end Next;

   procedure Acknowledge_Read (D : Device; R : Request; B : out Buffer)
   is
      subtype Local_Buffer is Buffer (1 .. B'Length);
      subtype Local_U8_Array is Cbe.Genode.Uint8_T_Array (1 .. B'Length);
      function Convert_Buffer is new Ada.Unchecked_Conversion (Local_U8_Array, Local_Buffer);
      Data : Local_U8_Array := (others => 0);
   begin
      Cbe.Block.Client.Acknowledge_Read (
         D.Instance,
         Convert_Request (R),
         Data,
         Cbe.Genode.Uint64_T (B'Length));
      B := Convert_Buffer (Data);
   end Acknowledge_Read;

   procedure Acknowledge_Sync (D : Device; R : Request)
   is
   begin
      Cbe.Block.Client.Acknowledge_Sync (D.Instance, Convert_Request (R));
   end Acknowledge_Sync;

   procedure Acknowledge_Write (D : Device; R : Request)
   is
   begin
      Cbe.Block.Client.Acknowledge_Write (D.Instance, Convert_Request (R));
   end Acknowledge_Write;

end Block.Client;
