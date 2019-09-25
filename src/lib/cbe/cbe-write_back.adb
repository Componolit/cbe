--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;
with Cbe.Request;
with Sha256_4k;

package body Cbe.Write_Back with
   Spark_Mode
is
   --
   --  Private
   --

   procedure Fail_If_Pending_Primitive_Not_Complete (Obj  : Object_Type;
                                                     Prim : Primitive.Object_Type)
   is
   begin
      if
         Obj.State /= Complete
         or Primitive.Block_Number (Prim) /= Primitive.Block_Number (Obj.Pending_Primitive) then
         raise Program_Error;
      end if;
   end Fail_If_Pending_Primitive_Not_Complete;

   --
   --  The method is called for all entries by the specific
   --  'Peek_Generated_*_Primitve' method, which contains the
   --  state guard.
   --
   function Peek_Generated_Leaf_Primitive (Obj : Object_Type) return Primitive.Object_Type
   is
   begin
      if Obj.Entries (0).State /= Pending then
         return Primitive.Invalid_Object;
      end if;
      return Primitive.Valid_Object
            (Op     => Write,
             Succ   => Request.Success_Type (False),
             Tg     => Obj.Entries (0).Tag,
             Blk_Nr => Block_Number_Type (Obj.Entries (0).Update_Pba),
             Idx    => 0);
   end Peek_Generated_Leaf_Primitive;

   --
   --  The method is called for all entries by the specific
   --  'Peek_Generated_*_Data' method, which contains the
   --  state guard.
   --
   function Peek_Generated_Leaf_Data (Obj  : Object_Type;
                                      Prim : Primitive.Object_Type) return Data_Index_Type
   is
   begin
      if Primitive.Block_Number (Prim) /= Block_Number_Type (Obj.Entries (0).Update_Pba) then
         raise Program_Error;
      end if;
      return Data_Index_Type (0);
   end Peek_Generated_Leaf_Data;

   --
   --  The method is called for all entries by the specific
   --  'drop_generated_*_primitive' method, which contains the
   --  state guard.
   --
   procedure Drop_Generated_Leaf_Primitive (Obj  : in out Object_Type;
                                            Prim :        Primitive.Object_Type)
   is
      procedure Mark_Entry_As_In_Progress (E : in out Entry_Type) is
      begin
         --  XXX condition differs from 'Peek_Generated_Leaf_Data'
         if E.State /= Pending or Primitive.Block_Number (Prim) /= Block_Number_Type (E.Update_Pba) then
            raise Program_Error;
         end if;
         E.State := In_Progress;
      end Mark_Entry_As_In_Progress;
   begin
      Mark_Entry_As_In_Progress (Obj.Entries (0));
   end Drop_Generated_Leaf_Primitive;

   function Invalid_Entry return Entry_Type is (Pba => 0, Update_Pba => 0, State => Invalid, Tag => Tag_Invalid);

   --
   --  Public
   --

   procedure Initialize_Object (Obj : out Object_Type) is
   begin
      Obj := Initialized_Object;
   end Initialize_Object;

   --
   --  Initialized_Object
   --
   function Initialized_Object return Object_Type is
      (Entries           => (others => Invalid_Entry),
       Hashes            => (others => (others => 0)),
       Levels            => 0,
       State             => Invalid,
       Pending_Primitive => Primitive.Invalid_Object,
       Pending_Failure   => False,
       Vba               => Virtual_Block_Address_Type'Last,
       New_Generation    => 0);

   procedure Update (Obj         : in out Object_Type;
                     Pba         :        Physical_Block_Address_Type;
                     Tree        :        Tree_Helper.Object_Type;
                     Data        :        Block_Data_Type;
                     Update_Data : in out Block_Data_Type)
   is
   begin
      --
      --  Start at 1 since we only care about the inner nodes. The
      --  leaf node was already handled and the hash is stored, which
      --  makes accessing _entry[i-1] in the first round safe. Currently
      --  the way the arbiter operates and the Write_back module generates
      --  primitives should lead to updating the tree level by level.
      --
      --  (And if that's not the case, updating will corrupt the tree
      --  for obvious reasons.)
      --
      For_Each_Entry :
      for I in 1 .. Obj.Levels - 1 loop
         if Obj.Entries (I).Tag = Tag_Cache and Obj.Entries (I).Pba = Pba then
            --  CoW action incoming
            if Obj.Entries (I).Pba /= Obj.Entries (I).Update_Pba then
               Update_Data := Data;
            end if;
            declare
               --  save as long as only inner nodes in cache
               Child_Update_Pba : constant Physical_Block_Address_Type := Obj.Entries (I - 1).Update_Pba;
               Child_Hash       : constant Hash_Type                   := Obj.Hashes (I - 1);
               --  get index from VBA in inner node
               Index      : constant Tree_Child_Index_Type := Tree_Helper.Index (Tree, Obj.Vba, Tree_Level_Type (I));
               Node_Block : Type_I_Node_Block_Type with
                  Address => Update_Data'Address;
            begin
               Node_Block (Natural (Index)).Pba  := Child_Update_Pba;
               Node_Block (Natural (Index)).Gen  := Obj.New_Generation;
               Node_Block (Natural (Index)).Hash := Child_Hash;
            end;
            --  calculate hash
            Declare_Sha_Args :
            declare
               Sha_Data : Sha256_4k.Data_Type with
                  Address => Update_Data'Address;
               Sha_Hash : Sha256_4k.Hash_Type with
                  Address => Obj.Hashes (I)'Address;
            begin
               Sha256_4k.Hash (Sha_Data, Sha_Hash);
            end Declare_Sha_Args;
            Obj.Entries (I).State := Complete;
            exit For_Each_Entry;
         end if;
      end loop For_Each_Entry;
         --  for now always check all entries after update
      declare
         All_Entries_Complete : Boolean := True;
      begin
         for I in 1 .. Obj.Levels - 1 loop
            if Obj.Entries (I).State /= Complete then
               All_Entries_Complete := False;
               exit;
            end if;
         end loop;
         if All_Entries_Complete then
            Obj.State := Complete;
         end if;
      end;
   end Update;

   function Primitive_Acceptable (Obj : Object_Type) return Boolean is
      (not Primitive.Valid (Obj.Pending_Primitive));

   procedure Submit_Primitive (Obj      : in out Object_Type;
                               Prim     :        Primitive.Object_Type;
                               Gen      :        Generation_Type;
                               Vba      :        Virtual_Block_Address_Type;
                               New_Pbas :        New_Pbas_Type;
                               Old_Pbas :        Type_1_Node_Infos_Type;
                               N        :        Tree_Level_Index_Type;
                               Data     :        Block_Data_Type;
                               Wb_Data  :    out Data_Type)
   is
   begin
      --  set internal state
      Obj.Pending_Primitive := Prim;
      Obj.New_Generation    := Gen;
      Obj.Vba               := Vba;
      Obj.Levels            := N;
      --
      --  Currently only the leaf node is written to disk directly,
      --  the other entries are all inner nodes and are stored in
      --  the cache and will be written to disk on a cache flush.
      --
      --  always start by encrypting the new leaf data
      Obj.State := Crypto;
         --  handle common members of an entry...
         --  XXX iterate from 0?
      for I in 0 .. Obj.Levels - 1 loop
         Obj.Entries (I).Pba        := Old_Pbas (Natural (I)).Pba;
         Obj.Entries (I).Update_Pba := New_Pbas (I);
         Obj.Entries (I).State      := Pending;
         Obj.Entries (I).Tag        := Tag_Cache;
      end loop;
      --  ... but the data or rather leaf node is special
      Obj.Entries (0).Tag := Tag_Encrypt;
      Wb_Data (0)         := Data;
   end Submit_Primitive;

   function Peek_Completed_Primitive (Obj : Object_Type) return Primitive.Object_Type
   is
      Prim : Primitive.Object_Type := Obj.Pending_Primitive;
   begin
      if not Primitive.Valid (Obj.Pending_Primitive) then
         return Primitive.Invalid_Object;
      end if;
      if Obj.State /= Complete then
         return Primitive.Invalid_Object;
      end if;
      Primitive.Success (Prim, Request.Success_Type (not Obj.Pending_Failure));
      return Prim;
   end Peek_Completed_Primitive;

   function Peek_Completed_Root (Obj  : Object_Type;
                                 Prim : Primitive.Object_Type)
      return Physical_Block_Address_Type
   is
   begin
      Fail_If_Pending_Primitive_Not_Complete (Obj, Prim);
      return Obj.Entries (Obj.Levels - 1).Update_Pba;
   end Peek_Completed_Root;

   procedure Peek_Completed_Root_Hash (Obj  :     Object_Type;
                                       Prim :     Primitive.Object_Type;
                                       Hash : out Hash_Type)
   is
   begin
      Fail_If_Pending_Primitive_Not_Complete (Obj, Prim);
      Hash := Obj.Hashes (Obj.Levels - 1);
   end Peek_Completed_Root_Hash;

   procedure Drop_Completed_Primitive (Obj  : in out Object_Type;
                                       Prim :        Primitive.Object_Type)
   is
   begin
      Fail_If_Pending_Primitive_Not_Complete (Obj, Prim);
      Obj.Pending_Primitive := Primitive.Invalid_Object;
   end Drop_Completed_Primitive;

   function Peek_Generated_Crypto_Primitive (Obj : Object_Type) return Primitive.Object_Type
   is
   begin
      if Obj.State = Crypto then
         return Peek_Generated_Leaf_Primitive (Obj);
      end if;
      return Primitive.Invalid_Object;
   end Peek_Generated_Crypto_Primitive;

   function Peek_Generated_Crypto_Data (Obj  : Object_Type;
                                        Prim : Primitive.Object_Type) return Data_Index_Type
   is
   begin
      if Obj.State /= Crypto then
         raise Program_Error;
      end if;
      return Peek_Generated_Leaf_Data (Obj, Prim);
   end Peek_Generated_Crypto_Data;

   procedure Drop_Generated_Crypto_Primitive (Obj  : in out Object_Type;
                                              Prim :        Primitive.Object_Type)
   is
   begin
      if Obj.State /= Crypto then
         raise Program_Error;
      end if;
      Drop_Generated_Leaf_Primitive (Obj, Prim);
   end Drop_Generated_Crypto_Primitive;

   procedure Mark_Completed_Crypto_Primitive (Obj         : in out Object_Type;
                                              Prim        :        Primitive.Object_Type;
                                              Crypto_Data :        Block_Data_Type)
   is
   begin
      if Obj.State /= Crypto then
         raise Program_Error;
      end if;
      if Obj.Entries (0).State /= In_Progress
         or Primitive.Block_Number (Prim) /= Block_Number_Type (Obj.Entries (0).Update_Pba)
      then
         raise Program_Error;
      end if;
      Declare_Sha_Args :
      declare
         Sha_Data : Sha256_4k.Data_Type with
            Address => Crypto_Data'Address;
         Sha_Hash : Sha256_4k.Hash_Type with
            Address => Obj.Hashes (0)'Address;
      begin
         Sha256_4k.Hash (Sha_Data, Sha_Hash);
      end Declare_Sha_Args;
      if not Primitive.Success (Prim) then
         Obj.Pending_Failure := True;
         Obj.State           := Complete;
         return;
      end if;
      Obj.Entries (0).State := Pending;
      Obj.Entries (0).Tag   := Tag_Io;
      Obj.State             := Io;
   end Mark_Completed_Crypto_Primitive;

   function Peek_Generated_Io_Primitive (Obj : Object_Type) return Primitive.Object_Type
   is
   begin
      if Obj.State /= Io then
         return Primitive.Invalid_Object;
      end if;
      return Peek_Generated_Leaf_Primitive (Obj);
   end Peek_Generated_Io_Primitive;

   function Peek_Generated_Io_Data (Obj  : Object_Type;
                                    Prim : Primitive.Object_Type) return Data_Index_Type
   is
   begin
      if Obj.State /= Io then
         raise Program_Error;
      end if;
      return Peek_Generated_Leaf_Data (Obj, Prim);
   end Peek_Generated_Io_Data;

   procedure Drop_Generated_Io_Primitive (Obj  : in out Object_Type;
                                          Prim :        Primitive.Object_Type)
   is
   begin
      if Obj.State /= Io then
         raise Program_Error;
      end if;
      Drop_Generated_Leaf_Primitive (Obj, Prim);
   end Drop_Generated_Io_Primitive;

   procedure Mark_Completed_Io_Primitive (Obj  : in out Object_Type;
                                          Prim :        Primitive.Object_Type)
   is
   begin
      if Obj.State /= Io then
         raise Program_Error;
      end if;
      if Obj.Entries (0).State /= In_Progress
         or Primitive.Block_Number (Prim) /= Block_Number_Type (Obj.Entries (0).Update_Pba)
      then
         raise Program_Error;
      end if;
      if not Primitive.Success (Prim) then
         Obj.Pending_Failure := True;
         Obj.State           := Complete;
         return;
      end if;
      Obj.Entries (0).State := Complete;
      Obj.State             := Cache;
   end Mark_Completed_Io_Primitive;

   function Peek_Generated_Cache_Primitive (Obj : Object_Type) return Primitive.Object_Type
   is
   begin
      if Obj.State /= Cache then
         return Primitive.Invalid_Object;
      end if;
      For_Each_Entry :
      for I in 1 .. Obj.Levels - 1 loop
         if Obj.Entries (I).State = Pending then
            return Primitive.Valid_Object
                  (Op     => Write,
                   Succ   => Request.Success_Type (False),
                   Tg     => Obj.Entries (I).Tag,
                   Blk_Nr => Block_Number_Type (Obj.Entries (I).Pba),
                   Idx    => 0);
         end if;
      end loop For_Each_Entry;
      return Primitive.Invalid_Object;
   end Peek_Generated_Cache_Primitive;

   function Peek_Generated_Cache_Update_Pba (Obj  : in out Object_Type;
                                             Prim :        Primitive.Object_Type) return Physical_Block_Address_Type
   is
      Pba : constant Physical_Block_Address_Type := Physical_Block_Address_Type (Primitive.Block_Number (Prim));
   begin
      if Obj.State /= Cache then
         raise Program_Error;
      end if;
      For_Each_Entry :
      for I in 1 .. Obj.Levels - 1 loop
         if Obj.Entries (I).Pba = Pba then
            return Obj.Entries (I).Update_Pba;
         end if;
      end loop For_Each_Entry;
      raise Program_Error;
   end Peek_Generated_Cache_Update_Pba;

   procedure Drop_Generated_Cache_Primitive (Obj  : in out Object_Type;
                                             Prim :        Primitive.Object_Type)
   is
      Pba : constant Physical_Block_Address_Type := Physical_Block_Address_Type (Primitive.Block_Number (Prim));
   begin
      if Obj.State /= Cache then
         raise Program_Error;
      end if;
      For_Each_Entry :
      for I in 1 .. Obj.Levels - 1 loop
         if Obj.Entries (I).Pba = Pba then
            Obj.Entries (I).State := In_Progress;
            return;
         end if;
      end loop For_Each_Entry;
      raise Program_Error;
   end Drop_Generated_Cache_Primitive;

end Cbe.Write_Back;
