with Cai.Block.Client;

package Block_Client is

   type State is null record;

   procedure Callback (S : in out State);

   package Block_Client is new Cai.Block.Client (State, Callback);

end Block_Client;
