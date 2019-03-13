with Cai.Block;
with Cai.Block.Client;

package Block_Client is

   procedure Callback;

   package Block_Client is new Cai.Block.Client (Callback);

end Block_Client;
