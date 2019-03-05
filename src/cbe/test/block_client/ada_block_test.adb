with Ada.Unchecked_Conversion;
with Block;
with Block.Client;
with Gnat.Io;
use all type Block.Size;
use all type Block.Request_Kind;
use all type Block.Request_Status;

package body Ada_Block_Test is

   procedure Run is
      Client : Block.Client.Device := Block.Client.Create_Device;
      subtype Block_Buffer is Block.Buffer (1 .. 4096);
      subtype Block_String is String (1 .. Block_Buffer'Length);
      function Convert_Block is new Ada.Unchecked_Conversion (Block_Buffer, Block_String);
      Buf : Block_Buffer := (others => 0);
      Write_Req : Block.Request (Kind => Block.Write);
      Read_Req : Block.Request (Kind => Block.Read);
      Acknowledged_Blocks : Integer;
      Block_Size : Block.Size;
   begin
      Block.Client.Initialize_Device (Client, "");
      Block_Size := Block.Client.Block_Size (Client);
      if Block_Size > Block_Buffer'Length then
         Gnat.Io.Put_Line ("ERROR: Block size too big");
      end if;
      Gnat.Io.Put_Line ("Writing...");
      Write_Req.Start := 1;
      Write_Req.Length := 1;
      Buf (1 .. Block.Unsigned_Long (Block_Size)) := (others => Block.Byte (Character'Pos('a')));
      Block.Client.Submit_Write (Client, Write_Req, Buf (1 .. Block.Unsigned_Long (Block_Size)));
      Write_Req.Start := 3;
      Block.Client.Submit_Write (Client, Write_Req, Buf (1 .. Block.Unsigned_Long (Block_Size)));
      Buf (1 .. Block.Unsigned_Long (Block_Size)) := (others => Block.Byte (Character'Pos('d')));
      Write_Req.Start := 2;
      Block.Client.Submit_Write (Client, Write_Req, Buf (1 .. Block.Unsigned_Long (Block_Size)));
      Acknowledged_Blocks := 0;
      while Acknowledged_Blocks < 3 loop
         declare
            Req : Block.Request := Block.Client.Next (Client);
         begin
            if Req.Kind = Block.Write then
               Acknowledged_Blocks := Acknowledged_Blocks + 1;
               Gnat.Io.Put_Line ("Write to block " &
               (if Req.Status = Block.Ok then " succeeded" else " failed"));
               Block.Client.Acknowledge (Client, Req);
            end if;
         end;
      end loop;
      Gnat.Io.Put_Line ("Writing finished.");
      Gnat.Io.Put_Line ("Reading...");
      Read_Req.Start := 1;
      Read_Req.Length := 1;
      Block.Client.Submit_Read (Client, Read_Req);
      Read_Req.Start := 2;
      Block.Client.Submit_Read (Client, Read_Req);
      Read_Req.Start := 3;
      Block.Client.Submit_Read (Client, Read_Req);
      Acknowledged_Blocks := 0;
      while Acknowledged_Blocks < 3 loop
         declare
            Req : Block.Request := Block.Client.Next (Client);
         begin
            if Req.Kind = Block.Read then
               Acknowledged_Blocks := Acknowledged_Blocks + 1;
               if Req.Status = Block.Ok then
                  Block.Client.Read (Client, Req, Buf);
               end if;
               Gnat.Io.Put_Line ("Reading from block " &
               (if Req.Status = Block.Ok then " succeeded" else " failed"));
               if Req.Status = Block.Ok then
                  Gnat.Io.Put_Line (Convert_Block (Buf) (1 .. Standard.Integer (Block_Size)));
               end if;
               Block.Client.Acknowledge (Client, Req);
            end if;
         end;
      end loop;
      Gnat.Io.Put_Line ("Reading finished.");
      Block.Client.Finalize_Device (Client);
   end Run;

end Ada_Block_Test;
