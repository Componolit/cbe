--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;
with Cbe.Tree_Helper;

package body Cbe.Library with
   Spark_Mode
is

   function Discard_Snapshot (Active_Snaps : in out Snapshots_Type;
                              Curr_Snap_Id :        Snapshot_Id_Type) return Boolean
   is
      Lowest_Snap_Id : Snapshot_Id_Type := Snapshot_Id_Invalid;
   begin
      for Snap of Active_Snaps loop
         if
            Snapshot_Valid (Snap)
            and not Snapshot_Keep (Snap)
            and Snap.Id /= Curr_Snap_Id
            and Snap.Id < Lowest_Snap_Id
         then
            Lowest_Snap_Id := Snap.Id;
         end if;
      end loop;
      if Lowest_Snap_Id = Snapshot_Id_Invalid then
         return False;
      end if;
      Active_Snaps (Snapshots_Index_Type (Lowest_Snap_Id)).Id := Snapshot_Id_Invalid;
      --  DBG("discard snapshot: ", Snap);
      return True;
   end Discard_Snapshot;

--
--  Not translated as only for debugging
--
--      procedure Dump_Cur_Sb_Info () const
--      is begin
--              Cbe::Super_Block const &sb := Obj.Super_Blocks (Obj.Cur_SB);
--              Snapshot_Type    const &Snap := sb.Snapshots (Obj.Cur_SB);
--
--              Cbe::Physical_Block_Address const root_Number := Snap.PBA;
--              Cbe::Height                 const height      := Snap.Height;
--              Cbe::Number_Of_Leafs       const leafs      := Snap.Leafs;
--
--              Cbe::Degree                 const degree      := sb.Degree;
--              Cbe::Physical_Block_Address const free_Number := sb.Free_Number;
--              Cbe::Number_Of_Leafs       const free_Leafs := sb.Free_Leafs;
--              Cbe::Height                 const free_Height := sb.Free_Height;
--
--              Genode::log ("Virtual block-device info in SB (", Obj.Cur_SB, "): ",
--                          " SNAP (", Obj.Cur_SB, "): ",
--                          "tree height: ", height, " ",
--                          "edges per node: ", degree, " ",
--                          "leafs: ", leafs, " ",
--                          "root block address: ", root_Number, " ",
--                          "free block address: ", free_Number, " ",
--                          "free leafs: (", free_Leafs, "/", free_Height, ")"
--              );
--      end Dump_Cur_Sb_Info;

   function Cache_Dirty (Obj : Object_Type) return Boolean
   is
      Result : Boolean := False;
   begin
      For_Cache_Data :
      for Cache_Index in Cache.Cache_Index_Type loop
         if Cache.Dirty (Obj.Cache_Obj, Cache_Index) then
            Result := True;
            exit For_Cache_Data;
         end if;
      end loop For_Cache_Data;
      return Result;
   end Cache_Dirty;

   function Superblock_Dirty (Obj : Object_Type) return Boolean is (Obj.Superblock_Dirty);
   function Is_Securing_Superblock (Obj : Object_Type) return Boolean is (Obj.Secure_Superblock);

   procedure Start_Securing_Superblock (Obj : in out Object_Type) is
   begin
      Obj.Secure_Superblock := True;
   end Start_Securing_Superblock;

   function Is_Sealing_Generation (Obj : Object_Type) return Boolean is (Obj.Seal_Generation);

   procedure Start_Sealing_Generation (Obj : in out Object_Type) is
   begin
      Obj.Seal_Generation := True;
   end Start_Sealing_Generation;

   function Super_Block_Snapshot_Slot (Sb : Super_Block_Type) return Snapshot_Id_Type
   is
      Snap_Slot : Snapshot_Id_Type := Snapshot_Id_Invalid_Slot;
   begin
      For_Snapshots :
      for Snap_Index in Snapshots_Index_Type loop
         if Snapshot_Valid (Sb.Snapshots (Snap_Index)) and Sb.Snapshots (Snap_Index).Id = Sb.Snapshot_Id then
            Snap_Slot := Snapshot_Id_Type (Snap_Index);
            exit For_Snapshots;
         end if;
      end loop For_Snapshots;
      return Snap_Slot;
   end Super_Block_Snapshot_Slot;

   procedure Initialize_Object (Obj     : out Object_Type;
                                Sbs     :     Super_Blocks_Type;
                                Curr_Sb :     Super_Blocks_Index_Type)
   is
      Snap_Slot : constant Snapshot_Id_Type          := Super_Block_Snapshot_Slot (Sbs (Curr_Sb));
      Degree    : constant Tree_Degree_Type          := Sbs (Curr_Sb).Degree;
      Height    : constant Tree_Level_Type := Sbs (Curr_Sb).Snapshots (Snapshots_Index_Type (Snap_Slot)).Height;
      Leafs     : constant Tree_Number_Of_Leafs_Type :=
         Sbs (Curr_Sb).Snapshots (Snapshots_Index_Type (Snap_Slot)).Nr_Of_Leafs;
   begin
      if Snap_Slot = Snapshot_Id_Invalid_Slot then
         --  Genode::error ("snapshot slot not found");
         raise Program_Error; --  throw Invalid_Snapshot_Slot;
      end if;

      --
      --  The Current implementation is limited with regard to the
      --  tree topology. Make sure it fits.
      --
      if Height > Tree_Max_Height or Height < Tree_Min_Height then
         --  Genode::error ("tree height of ", height, " not supported");
         raise Program_Error; --  throw Invalid_Tree;
      end if;

      if Degree < Tree_Min_Degree then
         --  Genode::error ("tree outer-degree of ", degree, " not supported");
         raise Program_Error; --  throw Invalid_Tree;
      end if;
      Obj.Execute_Progress := False;
      Obj.Request_Pool_Obj := Pool.Initialized_Object;
      Obj.Splitter_Obj     := Splitter.Initialized_Object;
      Obj.Crypto_Obj       :=
         Crypto.Initialized_Object
            (Key =>
                (65, 108, 108, 32, 121, 111, 117, 114, 32, 98, 97, 115, 101, 32, 97, 114, 101, 32, 98, 101, 108, 111,
                 110, 103, 32, 116, 111, 32, 117, 115, 32, 32)); --  "All your base are belong to us  "
      Obj.Crypto_Data :=
         (others => 0);
      Obj.Io_Obj  := Block_Io.Initialized_Object;
      Obj.Io_Data :=
         (others =>
             (others => 0));
      Obj.Cache_Obj  := Cache.Initialized_Object;
      Obj.Cache_Data :=
         (others =>
             (others => 0));
      Obj.Cache_Job_Data :=
         (others =>
             (others => 0));
      Obj.Cache_Flusher_Obj := Cache_Flusher.Initialized_Object;
      Obj.Trans_Data        :=
         (others =>
             (others => 0));
      Obj.Vbd             := Virtual_Block_Device.Initialized_Object (Height, Degree, Leafs);
      Obj.Write_Back_Obj  := Write_Back.Initialized_Object;
      Obj.Write_Back_Data :=
         (others =>
             (others => 0));
      Obj.Sync_Sb_Obj   := Sync_Superblock.Initialized_Object;
      Obj.Free_Tree_Obj :=
         Free_Tree.Initialized_Object
            (Sbs (Curr_Sb).Free_Number, Sbs (Curr_Sb).Free_Gen, Sbs (Curr_Sb).Free_Hash, Sbs (Curr_Sb).Free_Height,
             Sbs (Curr_Sb).Free_Degree, Sbs (Curr_Sb).Free_Leafs);
      Obj.Free_Tree_Retry_Count := 0;
      Obj.Free_Tree_Trans_Data  :=
         (others =>
             (others => 0));
      Obj.Free_Tree_Query_Data :=
         (others =>
             (others => 0));
      Obj.Super_Blocks            := Sbs;
      Obj.Cur_Sb                  := Superblock_Index_Type (Curr_Sb);
      Obj.Cur_Gen                 := Sbs (Curr_Sb).Last_Secured_Generation + 1;
      Obj.Last_Secured_Generation := Sbs (Curr_Sb).Last_Secured_Generation;
      Obj.Cur_Snap                := Snap_Slot;
      Obj.Last_Snapshot_Id        := Sbs (Curr_Sb).Snapshots (Snapshots_Index_Type (Snap_Slot)).Id;
      Obj.Seal_Generation         := False;
      Obj.Secure_Superblock       := False;
      Obj.Superblock_Dirty        := False;
      Obj.Front_End_Req_Prim      := Request_Primitive_Invalid;
      Obj.Back_End_Req_Prim       := Request_Primitive_Invalid;
   end Initialize_Object;

--
--  Not translated as only for debugging
--
--      procedure Dump_Cur_SB_Info () const
--      is begin
--              _Dump_Cur_SB_Info ();
--      end Dump_Cur_SB_Info;

   function Curr_Sb (Obj : Object_Type) return Super_Blocks_Index_Type
   is
   begin
      if Obj.Cur_Sb > Superblock_Index_Type (Super_Blocks_Index_Type'Last) then
         raise Program_Error;
      end if;
      return Super_Blocks_Index_Type (Obj.Cur_Sb);
   end Curr_Sb;

   function Curr_Snap (Obj : Object_Type) return Snapshots_Index_Type
   is
   begin
      if Obj.Cur_Snap > Snapshot_Id_Type (Snapshots_Index_Type'Last) then
         raise Program_Error;
      end if;
      return Snapshots_Index_Type (Obj.Cur_Snap);
   end Curr_Snap;

   function Max_Vba (Obj : Object_Type) return Virtual_Block_Address_Type
   is
   begin
      return Virtual_Block_Address_Type (Obj.Super_Blocks (Curr_Sb (Obj)).Snapshots (Curr_Snap (Obj)).Nr_Of_Leafs - 1);
   end Max_Vba;

   procedure Execute (Obj : in out Object_Type;
                      Now :        Timestamp_Type)
                  --  Show_Progress    :        Boolean;
                  --  Show_If_Progress :        Boolean)
   is
      procedure Create_New_Snapshot (Obj  :        Object_Type;
                                     Snap : in out Snapshot_Type;
                                     Prim :        Primitive.Object_Type;
                                     Id   :    out Snapshot_Id_Type)
      is
         Snap_Id : constant Snapshot_Id_Type := Obj.Last_Snapshot_Id + 1;
      begin
         Snap.Pba := Write_Back.Peek_Completed_Root (Obj.Write_Back_Obj, Prim);
         Write_Back.Peek_Completed_Root_Hash (Obj.Write_Back_Obj, Prim, Snap.Hash);
         Declare_Tree :
         declare
            Tree : constant Tree_Helper.Object_Type := Virtual_Block_Device.Get_Tree_Helper (Obj.Vbd);
         begin
            Snap.Height      := Tree_Helper.Height (Tree);
            Snap.Nr_Of_Leafs := Tree_Helper.Leafs (Tree);
            Snap.Gen         := Obj.Cur_Gen;
            Snap.Id          := Snap_Id;
         end Declare_Tree;
         Id := Snap_Id;
      end Create_New_Snapshot;

      procedure Update_Snapshot_Hash
         (Obj  :        Object_Type;
          Snap : in out Snapshot_Type;
          Prim :        Primitive.Object_Type)
      is
         Pba : constant Physical_Block_Address_Type := Write_Back.Peek_Completed_Root (Obj.Write_Back_Obj, Prim);
      begin
         --  FIXME why do we need that again?
         if Snap.Pba /= Pba then
            Snap.Gen := Obj.Cur_Gen;
            Snap.Pba := Pba;
         end if;
         Write_Back.Peek_Completed_Root_Hash (Obj.Write_Back_Obj, Prim, Snap.Hash);
      end Update_Snapshot_Hash;
      Progress : Boolean := False;

   begin
      Print_String (To_String (Obj));
      --------------------------
      --  Free-tree handling  --
      --------------------------

      --
      --  The FT meta-module uses the Translation module internally and
      --  needs access to the Cache since it wants to use its data.
      --  Because accessing a Cache entry will update its LRU value, the
      --  Cache must be mutable (that is also the reason we need the
      --  time object).
      --
      --  Since it might need to reuse reserved blocks, we have to hand
      --  over all active snapshots as well as the last secured generation.
      --  Both are needed for doing the reuse check.
      --
      --
      --  (Rather than passing the Cache module itself to the FT it might
      --  be better to use a different interface for that purpose as I
      --  do not know how well the current solution works with SPARK...)
      --
      declare
      begin
         Free_Tree.Execute
            (Obj.Free_Tree_Obj, Obj.Super_Blocks (Curr_Sb (Obj)).Snapshots, Obj.Last_Secured_Generation,
             Obj.Free_Tree_Trans_Data, Obj.Cache_Obj, Obj.Cache_Data, Obj.Free_Tree_Query_Data, Now);
         if Free_Tree.Execute_Progress (Obj.Free_Tree_Obj) then
            Progress := True;
         end if;
         --  LOG_PROGRESS(FT_Progress);
      end;

      --
      --  A complete primitive was either successful or has failed.
      --
      --  In the former case we will instruct the Write_Back module to
      --  write all changed nodes of the VBD back to the block device
      --  and eventually will leadt to ACKing the block Request.
      --
      --  In the later case we will attempt to free reserved blocks in
      --  the FT by discarding snapshots. Briefly speaking all snapshots
      --  that were not specifically marked (see FLAG_KEEP) will be
      --  discarded. A finit number of retries will be performed. If we
      --  cannot free enough blocks, the write operation is marked as
      --  failed and will result in an I/O error at the Block session.
      --
      --
      Loop_Free_Tree_Completed_Prims :
      loop
         Declare_Prim_1 :
         declare
            Prim : constant Primitive.Object_Type := Free_Tree.Peek_Completed_Primitive (Obj.Free_Tree_Obj);
         begin
            exit Loop_Free_Tree_Completed_Prims when not Primitive.Valid (Prim) or Primitive.Success (Prim);
               --  DBG("allocating new blocks failed: ", Obj.Free_Tree_Retry_Count);
            if Obj.Free_Tree_Retry_Count < Free_Tree_Retry_Limit then
               Declare_Curr :
               declare
                  Curr : constant Snapshot_Id_Type := Obj.Super_Blocks (Curr_Sb (Obj)).Snapshots (Curr_Snap (Obj)).Id;
               begin
                  if Discard_Snapshot (Obj.Super_Blocks (Curr_Sb (Obj)).Snapshots, Curr) then
                     Obj.Free_Tree_Retry_Count := Obj.Free_Tree_Retry_Count + 1;
                     --
                     --  Instructing the FT to retry the allocation will
                     --  lead to clearing its internal 'query branches'
                     --  state and executing the previously submitted
                     --  Request again.
                     --
                     --  (This retry attempt is a shortcut as we do not have
                     --  all information available at this point to call
                     --  'submit_Request' again - so we must not call
                     --  'drop_Completed_Primitive' as this will clear the
                     --  Request.)
                     --
                     Free_Tree.Retry_Allocation (Obj.Free_Tree_Obj);
                  end if;
               end Declare_Curr;
               exit Loop_Free_Tree_Completed_Prims;
            end if;
            --  Genode::error ("could not find enough useable blocks");
            Pool.Mark_Completed_Primitive (Obj.Request_Pool_Obj, Prim);
            --  DBG("-----------------------> current primitive: ", current_Primitive, " FINISHED");
            --  current_Primitive :=  : Primitive.Object_Type{ };
            --  FIXME
            Virtual_Block_Device.Trans_Resume_Translation (Obj.Vbd);
            Free_Tree.Drop_Completed_Primitive (Obj.Free_Tree_Obj, Prim);
         end Declare_Prim_1;
         Progress := True;
      end loop Loop_Free_Tree_Completed_Prims;
      --
      --  There are two types of generated primitives by FT module,
      --  the traversing of the tree is done by the internal Translation
      --  module, which will access the nodes through the Cache - I/O
      --  primitives will therefor be generated as a side-effect of the
      --  querying attempt by the Cache module.
      --
      --  - IO_TAG primitives are only used for querying type 2 nodes, i.E.,
      --   inner nodes of the free-tree containg free or reserved blocks.
      --
      --  - WRITE_BACK_TAG primitve are only used for writing one changed
      --   branch back to the block device. Having the branch written
      --   will lead to a complete primitve.
      --
      Loop_Free_Tree_Generated_Prims :
      loop
         Declare_Prim_2 :
         declare
            Prim : constant Primitive.Object_Type := Free_Tree.Peek_Generated_Primitive (Obj.Free_Tree_Obj);
         begin
            exit Loop_Free_Tree_Generated_Prims when not Primitive.Valid (Prim)
               or not Block_Io.Primitive_Acceptable (Obj.Io_Obj);
            Declare_Index_1 :
            declare
               Index : constant Index_Type := Free_Tree.Peek_Generated_Data_Index (Obj.Free_Tree_Obj, Prim);
            begin
               if Primitive.Tag (Prim) = Tag_Write_Back then
                  --
                  --  FIXME Accessing the Cache in this way could be dangerous because
                  --  the Cache is shared by the VBD as well as the FT. If we would
                  --  not suspend the VBD while doing the write-back, another Request
                  --  could evict the entry belonging to the Index value and replace it.
                  --
                  --  (Since the Prim contains the PBA we could check the validity of
                  --  the index beforehand - but not storing the index in the first
                  --  place would be the preferable solution.)
                  --
                  Block_Io.Submit_Primitive
                     (Obj.Io_Obj, Tag_Free_Tree_Wb, Prim, Obj.Io_Data, Obj.Cache_Data (Cache.Cache_Index_Type (Index)));

               elsif Primitive.Tag (Prim) = Tag_Io then
                  Block_Io.Submit_Primitive
                     (Obj.Io_Obj, Tag_Free_Tree_Io, Prim, Obj.Io_Data, Obj.Free_Tree_Query_Data (Natural (Index)));
               end if;
            end Declare_Index_1;
            Free_Tree.Drop_Generated_Primitive (Obj.Free_Tree_Obj, Prim);
         end Declare_Prim_2;
         Progress := True;
      end loop Loop_Free_Tree_Generated_Prims;
      ---------------------------------
      --  Put Request into splitter  --
      ---------------------------------
      --
      --  An arbitrary sized Block Request will be cut into 4096 byte
      --  sized primitves by the Splitter module.
      --
      Loop_Pool_Pending_Requests :
      loop
         Declare_Req :
         declare
            Req : constant Request.Object_Type := Pool.Peek_Pending_Request (Obj.Request_Pool_Obj);
         begin
            exit Loop_Pool_Pending_Requests when not Request.Valid (Req)
               or not Splitter.Request_Acceptable (Obj.Splitter_Obj);
            Pool.Drop_Pending_Request (Obj.Request_Pool_Obj, Req);
            Splitter.Submit_Request (Obj.Splitter_Obj, Req);
         end Declare_Req;
         Progress := True;
      end loop Loop_Pool_Pending_Requests;
      --
      --  Give primitive to the translation module
      --
      Loop_Splitter_Generated_Prims :
      loop
         Declare_Prim_3 :
         declare
            Prim : constant Primitive.Object_Type := Splitter.Peek_Generated_Primitive (Obj.Splitter_Obj);
         begin
            exit Loop_Splitter_Generated_Prims when not Primitive.Valid (Prim)
               or not Virtual_Block_Device.Primitive_Acceptable (Obj.Vbd);
               --
               --  FIXME why is Obj.Seal_Generation check not necessary?
               --  that mainly is intended to block write primitives--
               --
            if Obj.Secure_Superblock then
               --  DBG("prevent processing new primitives while securing super-block");
               exit Loop_Splitter_Generated_Prims;
            end if;
            Splitter.Drop_Generated_Primitive (Obj.Splitter_Obj, Prim);
            --  current_Primitive := Prim;
            --
            --  For every new Request, we have to use the currlently active
            --  snapshot as a previous Request may have changed the tree.
            --
            Virtual_Block_Device.Submit_Primitive
               (Obj.Vbd, Obj.Super_Blocks (Curr_Sb (Obj)).Snapshots (Curr_Snap (Obj)).Pba,
                Obj.Super_Blocks (Curr_Sb (Obj)).Snapshots (Curr_Snap (Obj)).Gen,
                Obj.Super_Blocks (Curr_Sb (Obj)).Snapshots (Curr_Snap (Obj)).Hash, Prim);
         end Declare_Prim_3;
         Progress := True;
      end loop Loop_Splitter_Generated_Prims;
      --  if current_Primitive.Valid () then
      --      DBG("-----------------------> current primitive: ", current_Primitive);
      --  end if;

      --------------------
      --  VBD handling  --
      --------------------

      --
      --  The VBD meta-module uses the Translation module internally and
      --  needs access to the Cache since it wants to use its Data.
      --  Because accessing a Cache entry will update its LRU value, the
      --  Cache must be mutable (that is also the reason we need the
      --  time object).
      --
      --
      --  (Basically the same issue regarding SPARK as the FT module...)
      --
      Virtual_Block_Device.Execute (Obj.Vbd, Obj.Trans_Data, Obj.Cache_Obj, Obj.Cache_Data, Now);

      if Virtual_Block_Device.Execute_Progress (Obj.Vbd) then
         Progress := True;
      end if;
      --  LOG_PROGRESS(vbd_Progress);

      ------------------------------
      --  Cache_Flusher handling  --
      ------------------------------

      --
      --  The Cache_Flusher module is used to flush all dirty Cache entries
      --  to the block device and mark them as clean again. While the flusher
      --  is doing its work, all Cache entries should be locked, i.E., do not
      --  Cache an entry while its flushed - otherwise the change might not
      --  end up on the block device. Should be guarded by 'Obj.Seal_Generation'.
      --
      --  (For better or worse it is just a glorified I/O manager. At some
      --  point it should be better merged into the Cache module later on.)
      --

      --
      --  Mark the corresponding Cache entry as clean. If it was
      --  evicted in the meantime it will be ignored.
      --
      Loop_Cache_Flusher_Completed_Prims :
      loop
         Declare_Prim_4 :
         declare
            Prim : constant Primitive.Object_Type := Cache_Flusher.Peek_Completed_Primitive (Obj.Cache_Flusher_Obj);
         begin
            exit Loop_Cache_Flusher_Completed_Prims when not Primitive.Valid (Prim);
            if not Primitive.Success (Prim) then
               --  DBG(Prim);
               raise Program_Error; --  throw Primitive_Failed;
            end if;
            Cache.Mark_Clean (Obj.Cache_Obj, Physical_Block_Address_Type (Primitive.Block_Number (Prim)));
            --  DBG("mark_Clean: ", PBA);
            Cache_Flusher.Drop_Completed_Primitive (Obj.Cache_Flusher_Obj, Prim);
         end Declare_Prim_4;
         Progress := True;
      end loop Loop_Cache_Flusher_Completed_Prims;
      --
      --  Just pass the primitive on to the I/O module.
      --
      Loop_Cache_Flusher_Generated_Prims :
      loop
         Declare_Prim_5 :
         declare
            Prim : constant Primitive.Object_Type := Cache_Flusher.Peek_Generated_Primitive (Obj.Cache_Flusher_Obj);
         begin
            exit Loop_Cache_Flusher_Generated_Prims when not Primitive.Valid (Prim)
               or not Block_Io.Primitive_Acceptable (Obj.Io_Obj);
            Block_Io.Submit_Primitive
               (Obj.Io_Obj, Tag_Cache_Flush, Prim, Obj.Io_Data,
                Obj.Cache_Data (Cache_Flusher.Peek_Generated_Data_Index (Obj.Cache_Flusher_Obj, Prim)));
            Cache_Flusher.Drop_Generated_Primitive (Obj.Cache_Flusher_Obj, Prim);
         end Declare_Prim_5;
         Progress := True;
      end loop Loop_Cache_Flusher_Generated_Prims;
      ---------------------------
      --  Write-back handling  --
      ---------------------------

      --
      --  The Write_Back module will store a changed branch including its leaf
      --  node on the block device.
      --
      --  The way it currently operates is as follows:
      --    1. (CRYPTO)   it hands the leaf Data to the Crypto module for encryption
      --    2. (IO)       it hands the encrypted leaf Data to I/O module to write it
      --                  to the block device
      --    3. (CACHE)    starting by the lowest inner node it will update the node
      --                  entry (PBA and Hash)
      --    4. (COMPLETE) it returns the new root PBA and root Hash
      --
      --  When 'Obj.Seal_Generation' is set, it will first instruct the Cache_Flusher
      --  module to clean the Cache. Afterwards it will store the current snapshot
      --  and increment the 'Obj.Cur_SB' as well as '_Cur_Gen' (-> there is only one
      --  snapshot per generation and there are currently only 48 snapshot slots per
      --  super-block) and set the sync trigger.
      --
      --  Otherwise it will just update the root Hash in place.
      --

      Loop_Wb_Completed_Prims :
      loop
         Declare_Prim_6 :
         declare
            Prim : constant Primitive.Object_Type := Write_Back.Peek_Completed_Primitive (Obj.Write_Back_Obj);
         begin
            exit Loop_Wb_Completed_Prims when not Primitive.Valid (Prim);
            if not Primitive.Success (Prim) then
               --  DBG(Prim);
               raise Program_Error; --  throw Primitive_Failed;
            end if;
            if Obj.Seal_Generation then
               --  FIXME only check flusher when the Cache is dirty
               --  FIXME and track if flusher is already active, e.G. by adding
               --     a 'active' function that returns True whenever is doing
               --     its job. I fear it currently only works by chance
               exit Loop_Wb_Completed_Prims when not Cache_Flusher.Request_Acceptable (Obj.Cache_Flusher_Obj);
               Declare_Cache_Dirty_2 :
               declare
                  Cache_Dirty : Boolean := False;
               begin
                  For_Cache_Data_2 :
                  for Cache_Index in Cache.Cache_Index_Type loop
                     if Cache.Dirty (Obj.Cache_Obj, Cache_Index) then
                        Cache_Dirty := True;
                        --  DBG(" i: ", Idx.Value, " PBA: ", PBA, " needs flushing");
                        Cache_Flusher.Submit_Request
                           (Obj.Cache_Flusher_Obj, Cache.Flush (Obj.Cache_Obj, Cache_Index), Cache_Index);
                     end if;
                  end loop For_Cache_Data_2;
                     --
                     --  In case we have to flush the Cache, wait until we have finished
                     --  doing that.
                     --
                  if Cache_Dirty then
                     --  DBG("CACHE FLUSH NEEDED: Progress: ", Progress);
                     Progress := True;
                     exit Loop_Wb_Completed_Prims;
                  end if;
               end Declare_Cache_Dirty_2;
               --
               --  Look for a new snapshot slot. If we cannot find one
               --  we manual intervention b/c there are too many snapshots
               --  flagged as keep
               --
               Declare_Next_Snap :
               declare
                  Next_Snap : Snapshot_Id_Type := Obj.Cur_Snap;
               begin
                  For_Snapshots :
                  for Snap_Id in Snapshots_Index_Type loop
                     Next_Snap := (Next_Snap + 1) mod Snapshot_Id_Type (Snapshots_Index_Type'Last + 1);
                     if not Snapshot_Valid
                           (Obj.Super_Blocks (Curr_Sb (Obj)).Snapshots (Snapshots_Index_Type (Next_Snap)))
                     then
                        exit For_Snapshots;
                     else
                        exit For_Snapshots when not Snapshot_Keep
                              (Obj.Super_Blocks (Curr_Sb (Obj)).Snapshots (Snapshots_Index_Type (Next_Snap)));
                     end if;
                  end loop For_Snapshots;
                  if Next_Snap = Obj.Cur_Snap then
                     --  Genode::error ("could not find free snapshot slot");
                     --  proper handling pending--
                     raise Program_Error; --  throw Invalid_Snapshot_Slot;
                  end if;
                  --
                  --  Creating a new snapshot only involves storing its
                  --  meta-Data in a new slot and afterwards setting the
                  --  seal timeout again.
                  --
                  Create_New_Snapshot
                     (Obj, Obj.Super_Blocks (Curr_Sb (Obj)).Snapshots (Snapshots_Index_Type (Next_Snap)), Prim,
                      Obj.Last_Snapshot_Id);
                  --  DBG("new snapshot for generation: ", Obj.Cur_Gen, " Snap: ", Snap);
               end Declare_Next_Snap;
               Obj.Cur_Gen         := Obj.Cur_Gen + 1;
               Obj.Cur_Snap        := Obj.Cur_Snap + 1;
               Obj.Seal_Generation := False;
            else
               --
               --  No need to create a new snapshot, just update the Hash in place
               --  and move on.
               --
               Update_Snapshot_Hash (Obj, Obj.Super_Blocks (Curr_Sb (Obj)).Snapshots (Curr_Snap (Obj)), Prim);
            end if;
            --
            --  We touched the super-block, either by updating a snapshot or by
            --  creating a new one - make sure it gets secured within the next
            --  interval.
            --
            Obj.Superblock_Dirty := True;
            Write_Back.Drop_Completed_Primitive (Obj.Write_Back_Obj, Prim);
            --
            --  Since the write Request is finally finished, all nodes stored
            --  at some place "save" (leafs on the block device, inner nodes within
            --  the Cache, acknowledge the primitive.
            --
            Pool.Mark_Completed_Primitive (Obj.Request_Pool_Obj, Prim);
         end Declare_Prim_6;
         --  DBG("-----------------------> current primitive: ", current_Primitive, " FINISHED");
         --  current_Primitive :=  : Primitive.Object_Type{ };
         Progress := True;
         --
         --  FIXME stalling translation as long as the write-back takes places
         --     is not a good idea
         --
         Virtual_Block_Device.Trans_Resume_Translation (Obj.Vbd);
      end loop Loop_Wb_Completed_Prims;
      --
      --  Give the leaf Data to the Crypto module.
      --
      Loop_Wb_Generated_Crypto_Prims :
      loop
         Declare_Prim_7 :
         declare
            Prim : constant Primitive.Object_Type := Write_Back.Peek_Generated_Crypto_Primitive (Obj.Write_Back_Obj);
         begin
            exit Loop_Wb_Generated_Crypto_Prims when not Primitive.Valid (Prim)
               or not Crypto.Primitive_Acceptable (Obj.Crypto_Obj);
            --  the Data will be copied into the Crypto module's internal buffer--
            Declare_Crypto_Data :
            declare
               Plain_Data_Index : constant Write_Back.Data_Index_Type :=
                  Write_Back.Peek_Generated_Crypto_Data (Obj.Write_Back_Obj, Prim);
               Plain_Data : Crypto.Plain_Data_Type with
                  Address => Obj.Write_Back_Data (Plain_Data_Index)'Address;
            begin
               Crypto.Submit_Encryption_Primitive (Obj.Crypto_Obj, Prim, Plain_Data);
            end Declare_Crypto_Data;
            Write_Back.Drop_Generated_Crypto_Primitive (Obj.Write_Back_Obj, Prim);
         end Declare_Prim_7;
         Progress := True;
      end loop Loop_Wb_Generated_Crypto_Prims;
      --
      --  Pass the encrypted leaf Data to the I/O module.
      --
      Loop_Wb_Generated_Io_Prims :
      loop
         Declare_Prim_8 :
         declare
            Prim : constant Primitive.Object_Type := Write_Back.Peek_Generated_Io_Primitive (Obj.Write_Back_Obj);
         begin
            exit Loop_Wb_Generated_Io_Prims when not Primitive.Valid (Prim)
               or not Block_Io.Primitive_Acceptable (Obj.Io_Obj);
            Block_Io.Submit_Primitive
               (Obj.Io_Obj, Tag_Write_Back, Prim, Obj.Io_Data,
                Obj.Write_Back_Data (Write_Back.Peek_Generated_Io_Data (Obj.Write_Back_Obj, Prim)));
            Write_Back.Drop_Generated_Io_Primitive (Obj.Write_Back_Obj, Prim);
         end Declare_Prim_8;
         Progress := True;
      end loop Loop_Wb_Generated_Io_Prims;
      --
      --  Update the inner nodes of the tree. This is always done after the
      --  encrypted leaf node was stored by the I/O module.
      --
      Loop_Wb_Generated_Cache_Prims :
      loop
         Declare_Prim_9 :
         declare
            Prim : constant Primitive.Object_Type := Write_Back.Peek_Generated_Cache_Primitive (Obj.Write_Back_Obj);
         begin
            --  DBG(Prim);
            exit Loop_Wb_Generated_Cache_Prims when not Primitive.Valid (Prim);
            Declare_Pbas :
            declare
               Pba : constant Physical_Block_Address_Type :=
                  Physical_Block_Address_Type (Primitive.Block_Number (Prim));
               Update_Pba : constant Physical_Block_Address_Type :=
                  Write_Back.Peek_Generated_Cache_Update_Pba (Obj.Write_Back_Obj, Prim);
               Cache_Miss : Boolean := False;
            begin
               --
               --  Check if the Cache contains the needed entries. In case of the
               --  of the old node's block that is most likely. The new one, if
               --  there is one (that happens when the inner nodes are Obj.Not_ updated
               --  in place, might not be in the Cache - check and Request both.
               --
               if not Cache.Data_Available (Obj.Cache_Obj, Pba) then
                  --  DBG("Cache miss PBA: ", PBA);
                  if Cache.Request_Acceptable (Obj.Cache_Obj, Pba) then
                     Cache.Submit_Request (Obj.Cache_Obj, Pba);
                  end if;
                  Cache_Miss := True;
               end if;
               if Pba /= Update_Pba then
                  if not Cache.Data_Available (Obj.Cache_Obj, Update_Pba) then
                     --  DBG("Cache miss Update_PBA: ", Update_PBA);
                     if Cache.Request_Acceptable (Obj.Cache_Obj, Update_Pba) then
                        Cache.Submit_Request (Obj.Cache_Obj, Update_Pba);
                     end if;
                     Cache_Miss := True;
                  end if;
               end if;
                  --  read the needed blocks first--
               if Cache_Miss then
                  --  DBG("Cache_Miss");
                  exit Loop_Wb_Generated_Cache_Prims;
               end if;
               Write_Back.Drop_Generated_Cache_Primitive (Obj.Write_Back_Obj, Prim);
               --  DBG("Cache hot PBA: ", PBA, " Update_PBA: ", Update_PBA);
               --
               --  To keep it simply, always set both properly - even if
               --  the old and new node are the same.
               --
               Declare_Indices :
               declare
                  Index        : constant Cache.Cache_Index_Type := Cache.Data_Index (Obj.Cache_Obj, Pba, Now);
                  Update_Index : constant Cache.Cache_Index_Type := Cache.Data_Index (Obj.Cache_Obj, Update_Pba, Now);
               begin
                  --
                  --  (Later on we can remove the tree_Helper here as the outer degree,
                  --  which is used to calculate the entry in the inner node from the
                  --  VBA is set at compile-time.)
                  --
                  Write_Back.Update
                     (Obj.Write_Back_Obj, Pba, Virtual_Block_Device.Get_Tree_Helper (Obj.Vbd), Obj.Cache_Data (Index),
                      Obj.Cache_Data (Update_Index));
                  --  make the potentially new entry as dirty so it gets flushed next time--
                  Cache.Mark_Dirty (Obj.Cache_Obj, Update_Pba);
               end Declare_Indices;
            end Declare_Pbas;
         end Declare_Prim_9;
         Progress := True;
      end loop Loop_Wb_Generated_Cache_Prims;
         ----------------------------
         --  Super-block handling  --
         ----------------------------

         --
         --  Store the current generation and snapshot id in the current
         --  super-block before it gets secured.
         --

      if Obj.Secure_Superblock and Sync_Superblock.Request_Acceptable (Obj.Sync_Sb_Obj) then
         Obj.Super_Blocks (Curr_Sb (Obj)).Last_Secured_Generation := Obj.Cur_Gen;
         Obj.Super_Blocks (Curr_Sb (Obj)).Snapshot_Id             := Snapshot_Id_Type (Obj.Cur_Sb);
         --  DBG("secure current super-block Gen: ", Obj.Cur_Gen,
         --     " Snap.ID: ", Obj.Cur_SB);
         Sync_Superblock.Submit_Request (Obj.Sync_Sb_Obj, Obj.Cur_Sb, Obj.Cur_Gen);
      end if;
      --
      --  When the current super-block was secured, select the next one.
      --
      Loop_Sync_Sb_Completed_Prims :
      loop
         Declare_Prim_10 :
         declare
            Prim : constant Primitive.Object_Type := Sync_Superblock.Peek_Completed_Primitive (Obj.Sync_Sb_Obj);
         begin
            exit Loop_Sync_Sb_Completed_Prims when not Primitive.Valid (Prim);
            if not Primitive.Success (Prim) then
               --  DBG(Prim);
               raise Program_Error; --  throw Primitive_Failed;
            end if;
            --  DBG("primitive: ", Prim);
            Declare_Next_Sb :
            declare
               Next_Sb : constant Superblock_Index_Type :=
                  Obj.Cur_Sb + 1 mod Superblock_Index_Type (Super_Blocks_Index_Type'Last) + 1;
            begin
               Obj.Super_Blocks (Super_Blocks_Index_Type (Next_Sb)) := Obj.Super_Blocks (Curr_Sb (Obj));
               --  handle state--
               Obj.Cur_Sb                  := Next_Sb;
               Obj.Last_Secured_Generation := Sync_Superblock.Peek_Completed_Generation (Obj.Sync_Sb_Obj, Prim);
               Obj.Superblock_Dirty        := False;
               Obj.Secure_Superblock       := False;
            end Declare_Next_Sb;
            Sync_Superblock.Drop_Completed_Primitive (Obj.Sync_Sb_Obj, Prim);
         end Declare_Prim_10;
         Progress := True;
      end loop Loop_Sync_Sb_Completed_Prims;
      --
      --  Use I/O module to write super-block to the block device.
      --
      Loop_Sync_Sb_Generated_Prims :
      loop
         Declare_Prim_11 :
         declare
            Prim : constant Primitive.Object_Type := Sync_Superblock.Peek_Generated_Primitive (Obj.Sync_Sb_Obj);
         begin
            exit Loop_Sync_Sb_Generated_Prims when not Primitive.Valid (Prim)
               or not Block_Io.Primitive_Acceptable (Obj.Io_Obj);
            Declare_Sb_Data :
            declare
               Sb_Index : constant Superblock_Index_Type :=
                  Sync_Superblock.Peek_Generated_Index (Obj.Sync_Sb_Obj, Prim);
               Sb_Data : Block_Data_Type with
                  Address => Obj.Super_Blocks (Super_Blocks_Index_Type (Sb_Index))'Address;
            begin
               Block_Io.Submit_Primitive (Obj.Io_Obj, Tag_Sync_Sb, Prim, Obj.Io_Data, Sb_Data);
            end Declare_Sb_Data;
            Sync_Superblock.Drop_Generated_Primitive (Obj.Sync_Sb_Obj, Prim);
         end Declare_Prim_11;
         Progress := True;
      end loop Loop_Sync_Sb_Generated_Prims;
      -----------------------
      --  Crypto handling  --
      -----------------------

      --
      --  The Crypto module has its own internal buffer, Data has to be
      --  copied in and copied out.
      --
      Crypto.Execute (Obj.Crypto_Obj);
      if Crypto.Execute_Progress (Obj.Crypto_Obj) then
         Progress := True;
      end if;
      --  LOG_PROGRESS(crypto_Progress);
      --
      --  Only writes primitives (encrypted Data) are handled here,
      --  read primitives (decrypred Data) are handled in 'give_Read_Data'.
      --
      Loop_Crypto_Completed_Prims :
      loop
         Declare_Prim_12 :
         declare
            Prim : constant Primitive.Object_Type := Crypto.Peek_Completed_Primitive (Obj.Crypto_Obj);
         begin
            exit Loop_Crypto_Completed_Prims when not Primitive.Valid (Prim) or Primitive.Operation (Prim) = Read;
            if not Primitive.Success (Prim) then
               --  DBG(Prim);
               raise Program_Error; --  throw Primitive_Failed;
            end if;
            Declare_Index_2 :
            declare
               Index : constant Write_Back.Data_Index_Type :=
                  Write_Back.Peek_Generated_Crypto_Data (Obj.Write_Back_Obj, Prim);
               Cipher_Data : Crypto.Cipher_Data_Type with
                  Address => Obj.Write_Back_Data (Index)'Address;
            begin
               --
               --  FIXME instead of copying the Data just ask the crypto module for the resulting
               --     Hash and omit further processing in case the operation failed
               --
               Crypto.Copy_Encrypted_Data (Obj.Crypto_Obj, Prim, Cipher_Data);
               Write_Back.Mark_Completed_Crypto_Primitive (Obj.Write_Back_Obj, Prim, Obj.Write_Back_Data (Index));
            end Declare_Index_2;
            Crypto.Drop_Completed_Primitive (Obj.Crypto_Obj, Prim);
         end Declare_Prim_12;
         Progress := True;
      end loop Loop_Crypto_Completed_Prims;
      --
      --  Since encryption is performed when calling 'execute' and decryption
      --  is handled differently, all we have to do here is to drop and mark
      --  complete.
      --
      Loop_Crypto_Generated_Prims :
      loop
         Declare_Prim_13 :
         declare
            Prim : constant Primitive.Object_Type := Crypto.Peek_Generated_Primitive (Obj.Crypto_Obj);
         begin
            exit Loop_Crypto_Generated_Prims when not Primitive.Valid (Prim);
            Crypto.Drop_Generated_Primitive (Obj.Crypto_Obj, Prim);
            Crypto.Mark_Completed_Primitive (Obj.Crypto_Obj, Prim);
         end Declare_Prim_13;
         Progress := True;
      end loop Loop_Crypto_Generated_Prims;
      ----------------------
      --  Cache handling  --
      ----------------------

      --
      --  Pass the Data used by the module in by reference so that it
      --  can be shared by the other modules. The method will internally
      --  copy read job Data into the chosen entry. In doing so it might
      --  evict an already populated entry.
      --
      Cache.Fill_Cache (Obj.Cache_Obj, Obj.Cache_Data, Obj.Cache_Job_Data, Now);
      if Cache.Execute_Progress (Obj.Cache_Obj) then
         Progress := True;
      end if;
      --  LOG_PROGRESS(Cache_Progress);
      --
      --  Read Data from the block device to fill the Cache.
      --
      --  (The Cache module has no 'peek_Completed_Primitive ()' method,
      --  all modules using the Cache have to poll and might be try to
      --  submit the same Request multiple times (see its acceptable
      --  method). It makes sense to change the Cache module so that it
      --  works the rest of modules. That would require restructing
      --  the modules, though.)
      --
      Loop_Cache_Generated_Prims :
      loop
         Declare_Prim_14 :
         declare
            Prim : constant Primitive.Object_Type := Cache.Peek_Generated_Primitive (Obj.Cache_Obj);
         begin
            exit Loop_Cache_Generated_Prims when not Primitive.Valid (Prim)
               or not Block_Io.Primitive_Acceptable (Obj.Io_Obj);
            Block_Io.Submit_Primitive
               (Obj.Io_Obj, Tag_Cache, Prim, Obj.Io_Data,
                Obj.Cache_Job_Data
                   (Cache.Cache_Job_Index_Type (Cache.Peek_Generated_Data_Index (Obj.Cache_Obj, Prim))));
            Cache.Drop_Generated_Primitive (Obj.Cache_Obj, Prim);
         end Declare_Prim_14;
         Progress := True;
      end loop Loop_Cache_Generated_Prims;
      --------------------
      --  I/O handling  --
      --------------------

      --
      --  This module handles all the block backend I/O and has to
      --  work with all most all modules. IT uses the 'Tag' field
      --  to differentiate the modules.
      --
      Loop_Io_Completed_Prims :
      loop
         Declare_Prim_15 :
         declare
            Prim : constant Primitive.Object_Type := Block_Io.Peek_Completed_Primitive (Obj.Io_Obj);
         begin
            exit Loop_Io_Completed_Prims when not Primitive.Valid (Prim);
            if not Primitive.Success (Prim) then
               --  DBG(Prim);
               raise Program_Error; --  throw Primitive_Failed;
            end if;
            Declare_Index_3 :
            declare
               Index : constant Block_Io.Data_Index_Type := Block_Io.Peek_Completed_Data_Index (Obj.Io_Obj);
               --
               --  Whenever we cannot hand a successful primitive over
               --  to the corresponding module, leave the loop but keep
               --  the completed primitive so that it might be processed
               --  next time.
               --
               Mod_Progress : Boolean := True;
            begin
               if Primitive.Tag (Prim) = Tag_Decrypt then
                  if not Crypto.Primitive_Acceptable (Obj.Crypto_Obj) then
                     Mod_Progress := False;
                  else
                     Declare_Data :
                     declare
                        Cipher_Data : Crypto.Cipher_Data_Type with
                           Address => Obj.Io_Data (Index)'Address;
                     begin
                        --
                        --  Having to override the Tag is needed because of the way
                        --  the Crypto module is hooked up in the overall Data flow.
                        --  Since it is the one that acknowledges the primitive to the
                        --  pool in the read case, we have to use the Tag the pool
                        --  module uses.
                        --
                        Crypto.Submit_Decryption_Primitive
                           (Obj.Crypto_Obj,
                            Primitive.Copy_Valid_Object_Change_Tag
                               (Prim, Block_Io.Peek_Completed_Tag (Obj.Io_Obj, Prim)),
                            Cipher_Data);
                     end Declare_Data;
                  end if;
               elsif Primitive.Tag (Prim) = Tag_Cache then
                  --
                  --  FIXME we need a proper method for getting the right Cache job
                  --       Data index, for now rely on the knowledge that there is
                  --       only one item
                  --
                  Obj.Cache_Job_Data (0) := Obj.Io_Data (Index);
                  Cache.Mark_Completed_Primitive (Obj.Cache_Obj, Prim);
               elsif Primitive.Tag (Prim) = Tag_Cache_Flush then
                  Cache_Flusher.Mark_Generated_Primitive_Complete (Obj.Cache_Flusher_Obj, Prim);
               elsif Primitive.Tag (Prim) = Tag_Write_Back then
                  Write_Back.Mark_Completed_Io_Primitive (Obj.Write_Back_Obj, Prim);
               elsif Primitive.Tag (Prim) = Tag_Sync_Sb then
                  Sync_Superblock.Mark_Generated_Primitive_Complete (Obj.Sync_Sb_Obj, Prim);
               elsif Primitive.Tag (Prim) = Tag_Free_Tree_Wb then
                  Free_Tree.Mark_Generated_Primitive_Complete
                     (Obj.Free_Tree_Obj, Primitive.Copy_Valid_Object_Change_Tag (Prim, Tag_Write_Back));
               elsif Primitive.Tag (Prim) = Tag_Free_Tree_Io then
                  --
                  --  FIXME we need a proper method for getting the right query
                  --       Data index, for now rely on the knowledge that there
                  --       is only one item
                  --
                  Obj.Free_Tree_Query_Data (0) := Obj.Io_Data (Index);
                  Free_Tree.Mark_Generated_Primitive_Complete
                     (Obj.Free_Tree_Obj, Primitive.Copy_Valid_Object_Change_Tag (Prim, Tag_Io));
               end if;
               exit Loop_Io_Completed_Prims when not Mod_Progress;
            end Declare_Index_3;
            Block_Io.Drop_Completed_Primitive (Obj.Io_Obj, Prim);
         end Declare_Prim_15;
         Progress := True;
      end loop Loop_Io_Completed_Prims;
      Obj.Execute_Progress := Progress;
   end Execute;

   function Request_Acceptable (Obj : Object_Type) return Boolean is
      (Pool.Request_Acceptable (Obj.Request_Pool_Obj));

   procedure Submit_Request (Obj : in out Object_Type;
                             Req :        Request.Object_Type)
   is
   begin
      Pool.Submit_Request (Obj.Request_Pool_Obj, Req, Splitter.Number_Of_Primitives (Req));
   end Submit_Request;

   function Peek_Completed_Request (Obj : Object_Type) return Request.Object_Type is
      (Pool.Peek_Completed_Request (Obj.Request_Pool_Obj));

   procedure Drop_Completed_Request (Obj : in out Object_Type;
                                     Req :        Request.Object_Type)
   is
   begin
      Pool.Drop_Completed_Request (Obj.Request_Pool_Obj, Req);
   end Drop_Completed_Request;

   procedure Io_Data_Required (Obj : in out Object_Type;
                               Req :    out Request.Object_Type)
   is
   begin
      if Primitive.Valid (Obj.Back_End_Req_Prim.Prim) then
         Req := Request.Invalid_Object;
         return;
      end if;
         --  I/O module--
      declare
         Prim : constant Primitive.Object_Type := Block_Io.Peek_Generated_Primitive (Obj.Io_Obj);
      begin
         if Primitive.Valid (Prim) and Primitive.Operation (Prim) = Read then
            Obj.Back_End_Req_Prim :=
               (Req => Request.Valid_Object
                      (Op     => Primitive.Operation (Prim),
                       Succ   => False,
                       Blk_Nr => Primitive.Block_Number (Prim),
                       Off    => 0,
                       Cnt    => 1,
                       Tg     => Tag_Invalid),
                Prim        => Prim,
                Tag         => Tag_Io,
                In_Progress => False);
            Req := Obj.Back_End_Req_Prim.Req;
         else
            Req := Request.Invalid_Object;
         end if;
      end;
   end Io_Data_Required;

   --
   --  For now there can be only one Request pending.
   --
   function Front_End_Busy_With_Other_Request (Obj : Object_Type;
                                               Req : Request.Object_Type) return Boolean is
      (not Request.Equal (Obj.Front_End_Req_Prim.Req, Req));

   procedure Io_Data_Read_In_Progress (Obj      : in out Object_Type;
                                       Req      :        Request.Object_Type;
                                       Progress :    out Boolean)
   is
   begin
      Progress := False;
      if not Request.Equal (Obj.Back_End_Req_Prim.Req, Req) or Obj.Back_End_Req_Prim.In_Progress
         or Obj.Back_End_Req_Prim.Tag /= Tag_Io
      then
         return;
      end if;
      Block_Io.Drop_Generated_Primitive (Obj.Io_Obj, Obj.Back_End_Req_Prim.Prim);
      Obj.Back_End_Req_Prim.In_Progress := True;
      Progress                          := True;
   end Io_Data_Read_In_Progress;

   --
   --  Return copy of Primitive with specified Success state
   --
   function Primitive_With_Success (Prim    : Primitive.Object_Type;
                                    Success : Request.Success_Type) return Primitive.Object_Type is
      (Primitive.Valid_Object
          (Op     => Primitive.Operation (Prim),
           Succ   => Success,
           Tg     => Primitive.Tag (Prim),
           Blk_Nr => Primitive.Block_Number (Prim),
           Idx    => Primitive.Index (Prim)));

   procedure Supply_Io_Data (Obj      : in out Object_Type;
                             Req      :        Request.Object_Type;
                             Data     :        Block_Data_Type;
                             Progress :    out Boolean)
   is
      Prim : constant Primitive.Object_Type := Obj.Back_End_Req_Prim.Prim;
   begin
      Progress := False;
      if not Request.Equal (Obj.Back_End_Req_Prim.Req, Req) or not Obj.Back_End_Req_Prim.In_Progress
         or Obj.Back_End_Req_Prim.Tag /= Tag_Io
      then
         return;
      end if;
      if Request.Success (Req) then
         Obj.Io_Data (Block_Io.Peek_Generated_Data_Index (Obj.Io_Obj, Prim)) := Data;
      end if;
      Block_Io.Mark_Generated_Primitive_Complete (Obj.Io_Obj, Primitive_With_Success (Prim, Request.Success (Req)));
      Obj.Back_End_Req_Prim := Request_Primitive_Invalid;
      Progress              := True;
   end Supply_Io_Data;

   procedure Has_Io_Data_To_Write (Obj : in out Object_Type;
                                   Req :    out Request.Object_Type)
   is
   begin
      if Primitive.Valid (Obj.Back_End_Req_Prim.Prim) then
         Req := Request.Invalid_Object;
         return;
      end if;
         --  I/O module--
      declare
         Prim : constant Primitive.Object_Type := Block_Io.Peek_Generated_Primitive (Obj.Io_Obj);
      begin
         if Primitive.Valid (Prim) and Primitive.Operation (Prim) = Write then
            Obj.Back_End_Req_Prim :=
               (Req => Request.Valid_Object
                      (Op     => Primitive.Operation (Prim),
                       Succ   => False,
                       Blk_Nr => Primitive.Block_Number (Prim),
                       Off    => 0,
                       Cnt    => 1,
                       Tg     => Tag_Invalid),
                Prim        => Prim,
                Tag         => Tag_Io,
                In_Progress => False);
            Req := Obj.Back_End_Req_Prim.Req;
         else
            Req := Request.Invalid_Object;
         end if;
      end;
   end Has_Io_Data_To_Write;

   procedure Obtain_Io_Data (Obj      : in out Object_Type;
                             Req      :        Request.Object_Type;
                             Data     :    out Block_Data_Type;
                             Progress :    out Boolean)
   is
      Prim : constant Primitive.Object_Type := Obj.Back_End_Req_Prim.Prim;
   begin
      Progress := False;

      if not Request.Equal (Obj.Back_End_Req_Prim.Req, Req) or Obj.Back_End_Req_Prim.In_Progress
         or Obj.Back_End_Req_Prim.Tag /= Tag_Io
      then
         return;
      end if;
      Data := Obj.Io_Data (Block_Io.Peek_Generated_Data_Index (Obj.Io_Obj, Prim));
      Block_Io.Drop_Generated_Primitive (Obj.Io_Obj, Prim);
      Obj.Back_End_Req_Prim.In_Progress := True;
      Progress                          := True;
   end Obtain_Io_Data;

   procedure Ack_Io_Data_To_Write (Obj      : in out Object_Type;
                                   Req      :        Request.Object_Type;
                                   Progress :    out Boolean)
   is
      Prim : constant Primitive.Object_Type := Obj.Back_End_Req_Prim.Prim;
   begin
      Progress := False;
      if not Request.Equal (Obj.Back_End_Req_Prim.Req, Req) or not Obj.Back_End_Req_Prim.In_Progress
         or Obj.Back_End_Req_Prim.Tag /= Tag_Io
      then
         return;
      end if;
      Block_Io.Mark_Generated_Primitive_Complete (Obj.Io_Obj, Primitive_With_Success (Prim, Request.Success (Req)));
      Obj.Back_End_Req_Prim := Request_Primitive_Invalid;
      Progress              := True;
   end Ack_Io_Data_To_Write;

   procedure Client_Data_Ready (Obj : in out Object_Type;
                                Req :    out Request.Object_Type)
   is
      procedure Assign_Front_End_Req_Prim (Prim : Primitive.Object_Type;
                                           Tag  : Tag_Type)
      is
      begin
         Obj.Front_End_Req_Prim :=
            (Req         => Pool.Request_For_Tag (Obj.Request_Pool_Obj, Primitive.Tag (Prim)),
             Prim        => Prim,
             Tag         => Tag,
             In_Progress => False);
      end Assign_Front_End_Req_Prim;
   begin
      Req := Request.Invalid_Object;
      if Primitive.Valid (Obj.Front_End_Req_Prim.Prim) then
         return;
      end if;
         --
         --  When it was a read Request, we need the location to
         --  where the Crypto should copy the decrypted data.
         --
      declare
         Prim : constant Primitive.Object_Type := Crypto.Peek_Completed_Primitive (Obj.Crypto_Obj);
      begin
         if Primitive.Valid (Prim) and Primitive.Operation (Prim) = Read then
            Assign_Front_End_Req_Prim (Prim, Tag_Crypto);
            Req := Obj.Front_End_Req_Prim.Req;
            return;
         end if;
      end;
         --
         --  When it was a read Request, we need access to the data the Crypto
         --  module should decrypt. --  XXX this should be handled in I/O backend
         --
      declare
         Prim : constant Primitive.Object_Type := Virtual_Block_Device.Peek_Completed_Primitive (Obj.Vbd);
      begin
         if Primitive.Valid (Prim) and Primitive.Operation (Prim) = Read then
            Assign_Front_End_Req_Prim (Prim, Tag_Vbd);
            Req := Obj.Front_End_Req_Prim.Req;
            return;
         end if;
      end;
   end Client_Data_Ready;

   function Give_Data_Index (Obj : Object_Type;
                             Req : Request.Object_Type) return Primitive.Index_Type
   is
   begin
      if Front_End_Busy_With_Other_Request (Obj, Req) then
         return Primitive.Invalid_Index;
      end if;
      return Primitive.Index (Obj.Front_End_Req_Prim.Prim);
   end Give_Data_Index;

   procedure Obtain_Client_Data (Obj      : in out Object_Type;
                                 Req      :        Request.Object_Type;
                                 Data     :    out Crypto.Plain_Data_Type;
                                 Progress :    out Boolean)
   is
      Prim : constant Primitive.Object_Type := Obj.Front_End_Req_Prim.Prim;
      Tag  : constant Tag_Type              := Obj.Front_End_Req_Prim.Tag;
   begin
      Progress := False;
      if Front_End_Busy_With_Other_Request (Obj, Req) then
         return;
      end if;
      if Tag = Tag_Crypto then
         Crypto.Copy_Decrypted_Data (Obj.Crypto_Obj, Prim, Data);
         Crypto.Drop_Completed_Primitive (Obj.Crypto_Obj, Prim);
         Pool.Mark_Completed_Primitive (Obj.Request_Pool_Obj, Prim);
         Obj.Front_End_Req_Prim := Request_Primitive_Invalid;
         Progress               := True;
      elsif Tag = Tag_Vbd then
         --
         --  We have to reset Front_End_Req_Prim before because in case there
         --  is current I/O pending, we have to make sure 'Client_Data_Ready' is
         --  called again.
         --
         Obj.Front_End_Req_Prim := Request_Primitive_Invalid;
         if Block_Io.Primitive_Acceptable (Obj.Io_Obj) then
            declare
               --  cast Crypto.Plain_Data_Type to Block_Data_Type
               Block_Data : Block_Data_Type with
                  Address => Data'Address;
            begin
               Block_Io.Submit_Primitive
                  (Obj     => Obj.Io_Obj,
                   Tag     => Tag_Decrypt,
                   Prim    => Prim,
                   Io_Data => Obj.Io_Data,
                   Data    => Block_Data);
            end;
            Virtual_Block_Device.Drop_Completed_Primitive (Obj.Vbd);
            Progress := True;
         end if;
      end if;
   end Obtain_Client_Data;

   --  FIXME move Front_End_Req_Prim allocation into execute,
   --       turn procedure into function
   procedure Client_Data_Required (Obj : in out Object_Type;
                                   Req :    out Request.Object_Type)
   is
      procedure Assign_Front_End_Req_Prim (Prim : Primitive.Object_Type;
                                           Tag  : Tag_Type)
      is
      begin
         Obj.Front_End_Req_Prim :=
            (Req         => Pool.Request_For_Tag (Obj.Request_Pool_Obj, Primitive.Tag (Prim)),
             Prim        => Prim,
             Tag         => Tag,
             In_Progress => False);
      end Assign_Front_End_Req_Prim;
   begin
      Req := Request.Invalid_Object;
      if Primitive.Valid (Obj.Front_End_Req_Prim.Prim) then
         return;
      end if;
         --
         --  A write Request, we need the location from where to read the new
         --  leaf data.
         --
      declare
         Prim : constant Primitive.Object_Type := Virtual_Block_Device.Peek_Completed_Primitive (Obj.Vbd);
      begin
         if Primitive.Valid (Prim) and Primitive.Operation (Prim) = Write then
            Assign_Front_End_Req_Prim (Prim, Tag_Vbd);
            Req := Obj.Front_End_Req_Prim.Req;
            return;
         end if;
      end;
         --
         --  The free-tree needs the data to give to the Write_Back module.
         --
      declare
         Prim : constant Primitive.Object_Type := Free_Tree.Peek_Completed_Primitive (Obj.Free_Tree_Obj);
      begin
         if Primitive.Valid (Prim) and Primitive.Success (Prim) then
            Assign_Front_End_Req_Prim (Prim, Tag_Free_Tree);
            Req := Obj.Front_End_Req_Prim.Req;
            return;
         end if;
      end;
   end Client_Data_Required;

   function Supply_Client_Data (Obj  : in out Object_Type;
                                Now  :        Timestamp_Type;
                                Req  :        Request.Object_Type;
                                Data :        Block_Data_Type) return Boolean
   is
      Prim : constant Primitive.Object_Type := Obj.Front_End_Req_Prim.Prim;
   begin
      --
      --  For now there is only one Request pending.
      --
      if not Request.Equal (Obj.Front_End_Req_Prim.Req, Req) then
         return False;
      end if;
      if Obj.Front_End_Req_Prim.Tag = Tag_Free_Tree then
         if not Write_Back.Primitive_Acceptable (Obj.Write_Back_Obj) then
            return False;
         end if;
         Obj.Free_Tree_Retry_Count := 0;
            --
            --  Accessing the write-back data in this manner is still a shortcut
            --  and probably will not work with SPARK - we have to get rid of
            --  the 'Block_Data' pointer.
            --
         declare
            Wb : constant Free_Tree.Write_Back_Data_Type := Free_Tree.Peek_Completed_Wb_Data (Obj.Free_Tree_Obj, Prim);
         begin
            Write_Back.Submit_Primitive
               (Obj.Write_Back_Obj, Wb.Prim, Wb.Gen, Wb.Vba, Wb.New_Pbas, Wb.Old_Pbas,
                Tree_Level_Index_Type (Wb.Tree_Height), Data, Obj.Write_Back_Data);
         end;
         Free_Tree.Drop_Completed_Primitive (Obj.Free_Tree_Obj, Prim);
--                      _Frontend_Req_Prim := Req_Prim { };
--  XXX check if default constructor produces invalid object
         Obj.Front_End_Req_Prim := Request_Primitive_Invalid;
         return True;
         --
         --  The VBD module translated a write Request, writing the data
         --  now to disk involves multiple steps:
         --
         --  1. Gathering of all nodes in the branch and looking up the
         --     volatile ones (those, which belong to theCurr generation
         --     and will be updated in place).
         --  2. Allocate new blocks if needed by consulting the FT
         --  3. Updating all entries in the nodes
         --  4. Writing the branch back to the block device.
         --
         --  Those steps are handled by different modules, depending on
         --  the allocation of new blocks.
         --
      elsif Obj.Front_End_Req_Prim.Tag = Tag_Vbd then
         --
         --  As usual check first we can submit new requests.
         --
         if not Free_Tree.Request_Acceptable (Obj.Free_Tree_Obj) then
            return False;
         end if;
         if not Virtual_Block_Device.Trans_Can_Get_Type_1_Info_Spark (Obj.Vbd, Prim) then
            return False;
         end if;
         --
         --  Then (ab-)use the Translation module and its still pending
         --  Request to get all old PBAs, whose generation we then check.
         --  The order of the array items corresponds to the level within
         --  the tree.
         --
         Declare_Old_Pbas :
         declare
            Old_Pbas : Type_1_Node_Infos_Type :=
               (others => Type_1_Node_Info_Invalid);
            Trans_Height : constant Tree_Level_Type := Virtual_Block_Device.Tree_Height (Obj.Vbd) + 1;
            --  XXX merge Super_Blocks_Index_Type and Superblock_Index_Type
            Snap : constant Snapshot_Type :=
               Obj.Super_Blocks (Super_Blocks_Index_Type (Obj.Cur_Sb)).Snapshots (Snapshots_Index_Type (Obj.Cur_Snap));
            --
            --  The array of new_PBA will either get populated from the Old_PBA
            --  content or from newly allocated blocks.
            --  The order of the array items corresponds to the level within
            --  the tree.
            --
            New_Pbas : Write_Back.New_Pbas_Type :=
               (others => 0);
            New_Blocks : Number_Of_Blocks_Type := 0;
            --
            --  This array contains all blocks that will get freed or rather
            --  marked as reserved in the FT as they are still referenced by
            --  an snapshot.
            --
            Free_Pbas : Free_Tree.Free_Pbas_Type :=
               (others => 0);
            Free_Blocks : Tree_Level_Index_Type := 0;
            --
            --  Get the corresponding VBA that we use to calculate the index
            --  for the edge in the node for a given level within the tree.
            --
            Vba : constant Virtual_Block_Address_Type :=
               Virtual_Block_Address_Type (Virtual_Block_Device.Trans_Get_Virtual_Block_Address (Obj.Vbd, Prim));
         begin
            Virtual_Block_Device.Trans_Get_Type_1_Info (Obj.Vbd, Old_Pbas);
               --
               --  Make sure we work with the proper snapshot.
               --
               --  (This check may be removed at some point.)
               --
            if Old_Pbas (Natural (Trans_Height - 1)).Pba /= Snap.Pba then
               raise Program_Error;
            end if;
               --
               --  Here only the inner nodes, i.E. all nodes excluding root and leaf,
               --  are considered. The root node is checked afterwards as we need the
               --  information of theCurr snapshot for that.
               --
            for I in 1 .. Trans_Height - 1 loop
               --
               --  Use the old PBA to get the node's data from the cache and
               --  use it check how we have to handle the node.
               --
               declare
                  Pba : constant Physical_Block_Address_Type := Old_Pbas (Natural (I)).Pba;
                  Idx : constant Cache.Cache_Index_Type      := Cache.Data_Index (Obj.Cache_Obj, Pba, Now);
                  Id  : constant Tree_Child_Index_Type       := Virtual_Block_Device.Index_For_Level (Obj.Vbd, Vba, I);
--                                              Cbe::Block_Data const &data := _Cache_Data.Item (idx.Value);
--                                              uint32_T const id := _VBD->index_For_Level (VBA, i);
--                                              Cbe::Type_I_Node const *n := reinterpret_Cast<Cbe::Type_I_Node const*>(&data);
                  Node : Type_I_Node_Block_Type with
                     Address => Obj.Cache_Data (Idx)'Address;
                  Gen : constant Generation_Type := Node (Natural (Id)).Gen;
               begin
                  --
                  --  In case the generation of the entry is the same as theCurr
                  --  generation OR if the generation is 0 (which means it was never
                  --  used before) the block is volatile and we change it in place
                  --  and store it directly in the new_PBA array.
                  --
                  if Gen = Obj.Cur_Gen or Gen = 0 then
                     New_Pbas (Tree_Level_Index_Type (I - 1)) := Old_Pbas (Natural (I - 1)).Pba;
                     --
                     --  Otherwise add the block to the free_PBA array so that the
                     --  FT will reserved it and note that we need another new block.
                     --
                  else
                     Free_Pbas (Free_Blocks) := Old_Pbas (Natural (I - 1)).Pba;
                     Free_Blocks             := Free_Blocks + 1;
                     New_Blocks              := New_Blocks + 1;
                  end if;
               end;
            end loop;
               --  check root node--
            if Snap.Gen = Obj.Cur_Gen or Snap.Gen = 0 then
               New_Pbas (Tree_Level_Index_Type (Trans_Height - 1)) := Old_Pbas (Natural (Trans_Height - 1)).Pba;
            else
               Free_Pbas (Free_Blocks) := Old_Pbas (Natural (Trans_Height - 1)).Pba;
               Free_Blocks             := Free_Blocks + 1;
               New_Blocks              := New_Blocks + 1;
            end if;
               --
               --  Since there are blocks we cannot change in place, use the
               --  FT module to allocate the blocks. As we have to reserve
               --  the blocks we implicitly will free (free_PBA items), pass
               --  on theCurr generation.
               --
            if New_Blocks > 0 then
               Free_Tree.Submit_Request
                  (Obj         => Obj.Free_Tree_Obj,
                   Curr_Gen    => Obj.Cur_Gen,
                   Nr_Of_Blks  => New_Blocks,
                   New_Pbas    => New_Pbas,
                   Old_Pbas    => Old_Pbas,
                   Tree_Height => Trans_Height,
                   Fr_Pbas     => Free_Pbas,
                   Req_Prim    => Prim,
                   Vba         => Vba);
            else
               --
               --  The complete branch is still part of theCurr generation,
               --  call the Write_Back module directly.
               --
               --  (We would have to check if the module can acutally accept
               --  the Request...)
               --
               Write_Back.Submit_Primitive
                  (Obj      => Obj.Write_Back_Obj,
                   Prim     => Prim,
                   Gen      => Obj.Cur_Gen,
                   Vba      => Vba,
                   New_Pbas => New_Pbas,
                   Old_Pbas => Old_Pbas,
                   N        => Tree_Level_Index_Type (Trans_Height),
                   Data     => Data,
                   Wb_Data  => Obj.Write_Back_Data);
            end if;
            Virtual_Block_Device.Drop_Completed_Primitive (Obj.Vbd);
            Obj.Front_End_Req_Prim := Request_Primitive_Invalid;
            --
            --  Inhibit translation which effectively will suspend the
            --  Translation modules operation and will stall all other
            --  pending requests to make sure all following Request will
            --  use the newest tree.
            --
            --  (It stands to reasons whether we can remove this check
            --  if we make sure that only the requests belonging to
            --  the same branch are serialized.)
            --
            Virtual_Block_Device.Trans_Inhibit_Translation (Obj.Vbd);
            return True;
         end Declare_Old_Pbas;
      end if;
      return False;
   end Supply_Client_Data;

   function Execute_Progress (Obj : Object_Type) return Boolean is (Obj.Execute_Progress);

   function To_String (Req_Prim : Request_Primitive_Type) return String
   is
   begin
      return "Req_Prim(Req=" & Request.To_String (Req_Prim.Req) & ", Prim=" & Primitive.To_String (Req_Prim.Prim)
         & ", Tag=" & To_String (Req_Prim.Tag) & ", In_Progress=" & To_String (Req_Prim.In_Progress) & ")";
   end To_String;

   function To_String (Obj : Object_Type) return String
   is
   begin
      return "Cbe=(" & ", Back_End_Req_Prim=" & To_String (Obj.Back_End_Req_Prim) & ", Front_End_Req_Prim="
         & To_String (Obj.Front_End_Req_Prim) & ", VBD=" & Virtual_Block_Device.To_String (Obj.Vbd)
         & ", Superblock_Dirty=" & Cbe.To_String (Obj.Superblock_Dirty) & ", Secure_Superblock="
         & Cbe.To_String (Obj.Secure_Superblock) & ")";
   end To_String;

end Cbe.Library;
