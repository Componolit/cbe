with Ada.Unchecked_Conversion;
with Cai.Block;
with Block_Client;
with Gnat.Io;
use all type Cai.Block.Size;
use all type Cai.Block.Request_Kind;
use all type Cai.Block.Request_Status;
use all type Cai.Block.Unsigned_Long;

package body Ada_Block_Test is

   procedure Run is
      Client : Cai.Block.Client_Session := Block_Client.Block_Client.Create;
      subtype Block_Buffer is Cai.Block.Buffer (1 .. 4096);
      subtype Block_String is String (1 .. Block_Buffer'Length);
      function Convert_Block is new Ada.Unchecked_Conversion (Block_Buffer, Block_String);
      Buf : Block_Buffer := (others => 0);
      Write_Req : Block_Client.Block_Client.Request (Kind => Cai.Block.Write);
      Read_Req : Block_Client.Block_Client.Request (Kind => Cai.Block.Read);
      Sync_Req : constant Block_Client.Block_Client.Request (Kind => Cai.Block.Sync) := (Kind => Cai.Block.Sync, Priv => Cai.Block.Null_Data);
      Acknowledged_Blocks : Integer;
      Block_Size : Cai.Block.Size;

      procedure Wait_For_Ready (R : Block_Client.Block_Client.Request) with
         Pre => Block_Client.Block_Client.Initialized (Client),
         Post => Block_Client.Block_Client.Ready (Client, R)
      is
      begin
         while not Block_Client.Block_Client.Ready (Client, R) loop
            null;
         end loop;
      end Wait_For_Ready;

   begin
      Block_Client.Block_Client.Initialize (Client, "ada test client");
      Block_Size := Block_Client.Block_Client.Block_Size (Client);
      if Block_Size > Block_Buffer'Length then
         Gnat.Io.Put_Line ("ERROR: Block size too big");
      end if;
      Gnat.Io.Put_Line ("Writing...");
      Write_Req.Start := 1;
      Write_Req.Length := 1;
      Buf (1 .. Cai.Block.Unsigned_Long (Block_Size)) := (others => Cai.Block.Byte (Character'Pos('a')));
      Wait_For_Ready (Write_Req);
      Block_Client.Block_Client.Enqueue_Write (Client, Write_Req, Buf (1 .. Cai.Block.Unsigned_Long (Block_Size)));
      Write_Req.Start := 3;
      Wait_For_Ready (Write_Req);
      Block_Client.Block_Client.Enqueue_Write (Client, Write_Req, Buf (1 .. Cai.Block.Unsigned_Long (Block_Size)));
      Buf (1 .. Cai.Block.Unsigned_Long (Block_Size)) := (others => Cai.Block.Byte (Character'Pos('d')));
      Write_Req.Start := 2;
      Wait_For_Ready (Write_Req);
      Block_Client.Block_Client.Enqueue_Write (Client, Write_Req, Buf (1 .. Cai.Block.Unsigned_Long (Block_Size)));
      Block_Client.Block_Client.Enqueue_Sync (Client, Sync_Req);
      Block_Client.Block_Client.Submit (Client);
      Acknowledged_Blocks := 0;
      while Acknowledged_Blocks < 3 loop
         declare
            Req : Block_Client.Block_Client.Request := Block_Client.Block_Client.Next (Client);
         begin
            if Req.Kind = Cai.Block.Write then
               Acknowledged_Blocks := Acknowledged_Blocks + 1;
               Gnat.Io.Put_Line ("Write to block " &
               (if Req.Status = Cai.Block.Ok then " succeeded" else " failed"));
               Block_Client.Block_Client.Release (Client, Req);
            end if;
         end;
      end loop;
      Gnat.Io.Put_Line ("Writing finished.");
      Gnat.Io.Put_Line ("Reading...");
      Read_Req.Start := 1;
      Read_Req.Length := 1;
      Wait_For_Ready (Read_Req);
      Block_Client.Block_Client.Enqueue_Read (Client, Read_Req);
      Read_Req.Start := 2;
      Wait_For_Ready (Read_Req);
      Block_Client.Block_Client.Enqueue_Read (Client, Read_Req);
      Read_Req.Start := 3;
      Wait_For_Ready (Read_Req);
      Block_Client.Block_Client.Enqueue_Read (Client, Read_Req);
      Block_Client.Block_Client.Submit (Client);
      Acknowledged_Blocks := 0;
      while Acknowledged_Blocks < 3 loop
         declare
            Req : Block_Client.Block_Client.Request := Block_Client.Block_Client.Next (Client);
         begin
            if Req.Kind = Cai.Block.Read then
               Acknowledged_Blocks := Acknowledged_Blocks + 1;
               if Req.Status = Cai.Block.Ok then
                  Block_Client.Block_Client.Read (Client, Req, Buf);
               end if;
               Gnat.Io.Put_Line ("Reading from block " &
               (if Req.Status = Cai.Block.Ok then " succeeded" else " failed"));
               if Req.Status = Cai.Block.Ok then
                  Gnat.Io.Put_Line (Convert_Block (Buf) (1 .. Standard.Integer (Block_Size)));
               end if;
               Block_Client.Block_Client.Release (Client, Req);
            end if;
         end;
      end loop;
      Gnat.Io.Put_Line ("Reading finished.");
      Gnat.Io.Put_Line ("Writing 2 block request...");
      Write_Req.Start := 4;
      Write_Req.Length := 2;
      Buf (1 .. Cai.Block.Unsigned_Long (Block_Size) * Cai.Block.Unsigned_Long (Write_Req.Length)) := (others => Cai.Block.Byte (Character'Pos ('x')));
      Wait_For_Ready (Write_Req);
      Block_Client.Block_Client.Enqueue_Write (Client, Write_Req, Buf (1 .. Cai.Block.Unsigned_Long (Block_Size) * Cai.Block.Unsigned_Long (Write_Req.Length)));
      Block_Client.Block_Client.Enqueue_Sync (Client, Sync_Req);
      Block_Client.Block_Client.Submit (Client);
      Acknowledged_Blocks := 0;
      while Acknowledged_Blocks < 1 loop
         declare
            Req : Block_Client.Block_Client.Request := Block_Client.Block_Client.Next (Client);
         begin
            if Req.Kind = Cai.Block.Write then
               Gnat.Io.Put_Line ("Writing 2 blocks " &
               (if Req.Status = Cai.Block.Ok then "succeeded" else "failed"));
               Block_Client.Block_Client.Release (Client, Req);
               Acknowledged_Blocks := 1;
            end if;
         end;
      end loop;
      Gnat.Io.Put_Line ("Writing finished.");
      Gnat.Io.Put_Line ("Reading 2 block request...");
      Read_Req.Start := 4;
      Read_Req.Length := 2;
      Wait_For_Ready (Read_Req);
      Block_Client.Block_Client.Enqueue_Read (Client, Read_Req);
      Block_Client.Block_Client.Submit (Client);
      Acknowledged_Blocks := 0;
      while Acknowledged_Blocks < 1 loop
         declare
            Req : Block_Client.Block_Client.Request := Block_Client.Block_Client.Next (Client);
         begin
            if Req.Kind = Cai.Block.Read then
               if Req.Status = Cai.Block.Ok then
                  Block_Client.Block_Client.Read (Client, Req, Buf);
               end if;
               Gnat.Io.Put_Line ("Reading 2 blocks " &
               (if Req.Status = Cai.Block.Ok then "succeeded" else "failed"));
               if Req.Status = Cai.Block.Ok then
                  Gnat.Io.Put_Line (Convert_Block (Buf) (1 .. Standard.Integer (Block_Size * Cai.Block.Size (Req.Length))));
               end if;
               Block_Client.Block_Client.Release (Client, Req);
               Acknowledged_Blocks := Acknowledged_Blocks + 1;
            end if;
         end;
      end loop;
      Gnat.Io.Put_Line ("Reading finished.");
      Block_Client.Block_Client.Finalize (Client);
   end Run;

end Ada_Block_Test;
