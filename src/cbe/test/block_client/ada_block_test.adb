with Ada.Unchecked_Conversion;
with Block;
with Block.Client;
with Gnat.Io;
use all type Block.Client.Request_Kind;

package body Ada_Block_Test is

   procedure Run is
      Client : Block.Client.Device := Block.Client.Create_Device;
      subtype Block_Buffer is Block.Buffer (1 .. 512);
      subtype Block_String is String (1 .. 512);
      function Convert_Block is new Ada.Unchecked_Conversion (Block_Buffer, Block_String);
      Buf : Block_Buffer;
      Write_Req : Block.Client.Request (Kind => Block.Client.Write);
      Read_Req : Block.Client.Request (Kind => Block.Client.Read);
      Started : Boolean := False;
      Handled : Boolean := False;
   begin
      Block.Client.Initialize_Device (Client, "");
      Write_Req.Start := 1;
      Write_Req.Length := 1;
      Buf := (others => Block.Byte (Character'Pos('a')));
      Block.Client.Submit_Write (Client, Write_Req, Buf);
      Write_Req.Start := 3;
      Block.Client.Submit_Write (Client, Write_Req, Buf);
      Buf := (others => Block.Byte (Character'Pos('d')));
      Write_Req.Start := 2;
      Block.Client.Submit_Write (Client, Write_Req, Buf);
      loop
         declare
            Req : constant Block.Client.Request := Block.Client.Next (Client);
         begin
            started := started or Req.Kind /= Block.Client.None;
            handled := started and Req.Kind = Block.Client.None;
            if handled then
               Gnat.Io.Put_Line ("writing finished");
            end if;
            if started and not handled and Req.Kind = Block.Client.Write then
               Gnat.Io.Put_Line ("Write to block " &
               (if Req.Success then " succeeded" else " failed"));
            end if;
         end;
         exit when handled;
      end loop;
      Read_Req.Start := 1;
      Read_Req.Length := 1;
      Block.Client.Submit_Read (Client, Read_Req);
      Read_Req.Start := 2;
      Block.Client.Submit_Read (Client, Read_Req);
      Read_Req.Start := 3;
      Block.Client.Submit_Read (Client, Read_Req);
      started := False;
      handled := false;
      loop
         declare
            Req : constant Block.Client.Request := Block.Client.Next (Client);
         begin
            started := started or Req.Kind /= Block.Client.None;
            handled := started and Req.Kind = Block.Client.None;
            if handled then
               Gnat.Io.Put_Line ("reading finished");
            end if;
            if started and not handled and Req.Kind = Block.Client.Read then
               Gnat.Io.Put_Line ("Reading from block " &
               (if Req.Success then " succeeded" else " failed"));
               Block.Client.Acknowledge_Read (Client, Req, Buf);
               Gnat.Io.Put_Line (Convert_Block (Buf));
            end if;
         end;
         exit when handled;
      end loop;
      Block.Client.Finalize_Device (Client);
   end Run;

end Ada_Block_Test;
