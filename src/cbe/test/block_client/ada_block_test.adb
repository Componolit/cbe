with Ada.Unchecked_Conversion;
with Cai.Block;
with Cai.Block.Client;
with Gnat.Io;
use all type Cai.Block.Size;
use all type Cai.Block.Request_Kind;
use all type Cai.Block.Request_Status;
use all type Cai.Block.Unsigned_Long;

package body Ada_Block_Test is

   procedure Run is
      Client : Cai.Block.Client.Device := Cai.Block.Client.Create_Device;
      subtype Block_Buffer is Cai.Block.Buffer (1 .. 4096);
      subtype Block_String is String (1 .. Block_Buffer'Length);
      function Convert_Block is new Ada.Unchecked_Conversion (Block_Buffer, Block_String);
      Buf : Block_Buffer := (others => 0);
      Write_Req : Cai.Block.Request (Kind => Cai.Block.Write);
      Read_Req : Cai.Block.Request (Kind => Cai.Block.Read);
      Acknowledged_Blocks : Integer;
      Block_Size : Cai.Block.Size;
   begin
      Cai.Block.Client.Initialize_Device (Client, "");
      Block_Size := Cai.Block.Client.Block_Size (Client);
      if Block_Size > Block_Buffer'Length then
         Gnat.Io.Put_Line ("ERROR: Block size too big");
      end if;
      Gnat.Io.Put_Line ("Writing...");
      Write_Req.Start := 1;
      Write_Req.Length := 1;
      Buf (1 .. Cai.Block.Unsigned_Long (Block_Size)) := (others => Cai.Block.Byte (Character'Pos('a')));
      Cai.Block.Client.Submit_Write (Client, Write_Req, Buf (1 .. Cai.Block.Unsigned_Long (Block_Size)));
      Write_Req.Start := 3;
      Cai.Block.Client.Submit_Write (Client, Write_Req, Buf (1 .. Cai.Block.Unsigned_Long (Block_Size)));
      Buf (1 .. Cai.Block.Unsigned_Long (Block_Size)) := (others => Cai.Block.Byte (Character'Pos('d')));
      Write_Req.Start := 2;
      Cai.Block.Client.Submit_Write (Client, Write_Req, Buf (1 .. Cai.Block.Unsigned_Long (Block_Size)));
      Acknowledged_Blocks := 0;
      while Acknowledged_Blocks < 3 loop
         declare
            Req : Cai.Block.Request := Cai.Block.Client.Next (Client);
         begin
            if Req.Kind = Cai.Block.Write then
               Acknowledged_Blocks := Acknowledged_Blocks + 1;
               Gnat.Io.Put_Line ("Write to block " &
               (if Req.Status = Cai.Block.Ok then " succeeded" else " failed"));
               Cai.Block.Client.Acknowledge (Client, Req);
            end if;
         end;
      end loop;
      Gnat.Io.Put_Line ("Writing finished.");
      Cai.Block.Client.Sync (Client);
      Gnat.Io.Put_Line ("Reading...");
      Read_Req.Start := 1;
      Read_Req.Length := 1;
      Cai.Block.Client.Submit_Read (Client, Read_Req);
      Read_Req.Start := 2;
      Cai.Block.Client.Submit_Read (Client, Read_Req);
      Read_Req.Start := 3;
      Cai.Block.Client.Submit_Read (Client, Read_Req);
      Acknowledged_Blocks := 0;
      while Acknowledged_Blocks < 3 loop
         declare
            Req : Cai.Block.Request := Cai.Block.Client.Next (Client);
         begin
            if Req.Kind = Cai.Block.Read then
               Acknowledged_Blocks := Acknowledged_Blocks + 1;
               if Req.Status = Cai.Block.Ok then
                  Cai.Block.Client.Read (Client, Req, Buf);
               end if;
               Gnat.Io.Put_Line ("Reading from block " &
               (if Req.Status = Cai.Block.Ok then " succeeded" else " failed"));
               if Req.Status = Cai.Block.Ok then
                  Gnat.Io.Put_Line (Convert_Block (Buf) (1 .. Standard.Integer (Block_Size)));
               end if;
               Cai.Block.Client.Acknowledge (Client, Req);
            end if;
         end;
      end loop;
      Gnat.Io.Put_Line ("Reading finished.");
      Gnat.Io.Put_Line ("Writing 2 block request...");
      Write_Req.Start := 4;
      Write_Req.Length := 2;
      Buf (1 .. Cai.Block.Unsigned_Long (Block_Size) * Cai.Block.Unsigned_Long (Write_Req.Length)) := (others => Cai.Block.Byte (Character'Pos ('x')));
      Cai.Block.Client.Submit_Write (Client, Write_Req, Buf (1 .. Cai.Block.Unsigned_Long (Block_Size) * Cai.Block.Unsigned_Long (Write_Req.Length)));
      Acknowledged_Blocks := 0;
      while Acknowledged_Blocks < 1 loop
         declare
            Req : Cai.Block.Request := Cai.Block.Client.Next (Client);
         begin
            if Req.Kind = Cai.Block.Write then
               Gnat.Io.Put_Line ("Writing 2 blocks " &
               (if Req.Status = Cai.Block.Ok then "succeeded" else "failed"));
               Cai.Block.Client.Acknowledge (Client, Req);
               Acknowledged_Blocks := 1;
            end if;
         end;
      end loop;
      Gnat.Io.Put_Line ("Writing finished.");
      Cai.Block.Client.Sync (Client);
      Gnat.Io.Put_Line ("Reading 2 block request...");
      Read_Req.Start := 4;
      Read_Req.Length := 2;
      Cai.Block.Client.Submit_Read (Client, Read_Req);
      Acknowledged_Blocks := 0;
      while Acknowledged_Blocks < 1 loop
         declare
            Req : Cai.Block.Request := Cai.Block.Client.Next (Client);
         begin
            if Req.Kind = Cai.Block.Read then
               if Req.Status = Cai.Block.Ok then
                  Cai.Block.Client.Read (Client, Req, Buf);
               end if;
               Gnat.Io.Put_Line ("Reading 2 blocks " &
               (if Req.Status = Cai.Block.Ok then "succeeded" else "failed"));
               if Req.Status = Cai.Block.Ok then
                  Gnat.Io.Put_Line (Convert_Block (Buf) (1 .. Standard.Integer (Block_Size * Cai.Block.Size (Req.Length))));
               end if;
               Cai.Block.Client.Acknowledge (Client, Req);
               Acknowledged_Blocks := Acknowledged_Blocks + 1;
            end if;
         end;
      end loop;
      Gnat.Io.Put_Line ("Reading finished.");
      Cai.Block.Client.Finalize_Device (Client);
   end Run;

end Ada_Block_Test;
