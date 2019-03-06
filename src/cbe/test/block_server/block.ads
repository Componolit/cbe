
with System;

package Block
   with SPARK_Mode
is
   type Unsigned_Long is range 0 .. 2 ** 63 - 1
      with Size => 64;
   type Id is mod 2 ** 64
      with Size => 64;
   type Count is range 0 .. 2 ** 63 - 1
      with Size => 64;
   type Size is range 0 .. 2 ** 32 - 1
      with Size => 64;
   type Byte is mod 2 ** 8
      with Size => 8;
   type Buffer is array (Unsigned_Long range <>) of Byte;

   type Request_Kind is (None, Read, Write, Sync);
   type Request_Status is (Raw, Ok, Error, Acknowledged);

   type Private_Data is private;

   type Request (Kind : Request_Kind) is record
      Priv : Private_Data;
      case Kind is
         when None | Sync =>
            null;
         when Read | Write =>
            Start : Id;
            Length : Count;
            Status : Request_Status;
      end case;
   end record;

   subtype Context is System.Address;

private

   type Private_Data is array (Unsigned_Long range 1 .. 16) of Byte;

end Block;
