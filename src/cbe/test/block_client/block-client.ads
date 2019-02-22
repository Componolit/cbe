
package Block.Client
   with SPARK_Mode
is

   type Request_Kind is (None, Read, Write, Sync);
   type Private_Data is private;

   type Request (Kind : Request_Kind) is record
      Priv : Private_Data;
      case Kind is
         when None | Sync =>
            null;
         when Read | Write =>
            Start : Block_Id;
            Length : Block_Count;
            Success : Boolean;
      end case;
   end record;

   type Device is limited private;

   function Create_Device return Device;

   procedure Initialize_Device (D : in out Device; Path : String);

   procedure Finalize_Device (D : in out Device);

   procedure Submit_Read (D : Device; R : Request)
      with Pre => R.Kind = Read;

   procedure Submit_Sync (D : Device; R : Request)
      with Pre => R.Kind = Sync;

   procedure Submit_Write (D : Device; R : Request; B : Buffer)
      with Pre => R.Kind = Write;

   function Next (D : Device) return Request;

   procedure Acknowledge_Read (D : Device; R : Request; B : out Buffer)
      with Pre => R.Kind = Read;

   procedure Acknowledge_Sync (D : Device; R : Request)
      with Pre => R.Kind = Sync;

   procedure Acknowledge_Write (D : Device; R : Request)
      with Pre => R.Kind = Write;

private

   type Private_Data is array (Long_Integer range 1 .. 16) of Byte;
   type Device_Instance is mod 2 ** 64
      with Size => 64;
   type Device is record
      Instance : Device_Instance;
   end record;


end Block.Client;
