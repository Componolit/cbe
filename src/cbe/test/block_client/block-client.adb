with Ada.Unchecked_Conversion;
with Cbe;
with Cbe.Block;
with Cbe.Genode;
use all type Cbe.Bool;

package body Block.Client is

   function Convert_Class is new Ada.Unchecked_Conversion (Cbe.Block.Client.Class, Device_Instance);
   function Convert_Class is new Ada.Unchecked_Conversion (Device_Instance, Cbe.Block.Client.Class);

   function Create_Device return Device
   is
   begin
      return Device'(Instance => Convert_Class (Cbe.Block.Client.Constructor));
   end Create_Device;

   procedure Initialize_Device (D : in out Device; Path : String)
   is
      C_Path : constant String := Path & Character'Val(0);
      subtype C_Path_String is String (1 .. C_Path'Length);
      subtype C_String is Cbe.Char_Array (1 .. C_Path'Length);
      function To_C_String is new Ada.Unchecked_Conversion (C_Path_String, C_String);
   begin

      Cbe.Block.Client.Initialize (Convert_Class (D.Instance), To_C_String (C_Path));
   end Initialize_Device;

   procedure Finalize_Device (D : in out Device)
   is
   begin
      Cbe.Block.Client.Finalize (Convert_Class (D.Instance));
   end Finalize_Device;

   function Convert_Request (R : Request) return Cbe.Block.Client.Request.Class
   is
      subtype Genode_Uid is Cbe.Genode_Uint8_T_Array (1 .. 16);
      function Convert_Uid is new Ada.Unchecked_Conversion (Private_Data, Genode_Uid);
   begin
      return Cbe.Block.Client.Request.Class'(
         Kind => (case R.Kind is
                  when None => Cbe.Block.Client.None,
                  when Read => Cbe.Block.Client.Read,
                  when Write => Cbe.Block.Client.Write,
                  when Sync => Cbe.Block.Client.Sync),
         Uid => Convert_Uid (R.Priv),
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
      subtype Genode_Uid is Cbe.Genode_Uint8_T_Array (1 .. 16);
      function Convert_Uid is new Ada.Unchecked_Conversion (Genode_Uid, Private_Data);
      R : Request ((case CR.Kind is
                     when Cbe.Block.Client.None => None,
                     when Cbe.Block.Client.Read => Read,
                     when Cbe.Block.Client.Write => Write,
                     when Cbe.Block.Client.Sync => Sync));
   begin
      R.Priv := Convert_Uid (CR.Uid);
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
      Cbe.Block.Client.Submit_Read (Convert_Class (D.Instance), Convert_Request (R));
   end Submit_Read;

   procedure Submit_Sync (D : Device; R : Request)
   is
   begin
      Cbe.Block.Client.Submit_Sync (Convert_Class (D.Instance), Convert_Request (R));
   end Submit_Sync;

   procedure Submit_Write (D : Device; R : Request; B : Buffer)
   is
      subtype Local_Buffer is Buffer (1 .. B'Length);
      subtype Local_U8_Array is Cbe.Genode.Uint8_T_Array (1 .. B'Length);
      function Convert_Buffer is new Ada.Unchecked_Conversion (Local_Buffer, Local_U8_Array);
      Data : Local_U8_Array := Convert_Buffer (B);
   begin
      Cbe.Block.Client.Submit_Write (
         Convert_Class (D.Instance),
         Convert_Request (R),
         Data,
         Cbe.Genode.Uint64_T (B'Length));
   end Submit_Write;

   function Next (D : Device) return Request
   is
   begin
      return Convert_Request (Cbe.Block.Client.Next (Convert_Class (D.Instance)));
   end Next;

   procedure Acknowledge_Read (D : Device; R : Request; B : out Buffer)
   is
      subtype Local_Buffer is Buffer (1 .. B'Length);
      subtype Local_U8_Array is Cbe.Genode.Uint8_T_Array (1 .. B'Length);
      function Convert_Buffer is new Ada.Unchecked_Conversion (Local_U8_Array, Local_Buffer);
      Data : Local_U8_Array := (others => 0);
   begin
      Cbe.Block.Client.Acknowledge_Read (
         Convert_Class (D.Instance),
         Convert_Request (R),
         Data,
         Cbe.Genode.Uint64_T (B'Length));
      B := Convert_Buffer (Data);
   end Acknowledge_Read;

   procedure Acknowledge_Sync (D : Device; R : Request)
   is
   begin
      Cbe.Block.Client.Acknowledge_Sync (Convert_Class (D.Instance), Convert_Request (R));
   end Acknowledge_Sync;

   procedure Acknowledge_Write (D : Device; R : Request)
   is
   begin
      Cbe.Block.Client.Acknowledge_Write (Convert_Class (D.Instance), Convert_Request (R));
   end Acknowledge_Write;

end Block.Client;
