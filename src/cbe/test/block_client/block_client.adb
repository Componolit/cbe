with Gnat.Io;

package body Block_Client is

   procedure Callback (S : in out State)
   is
   begin
      Gnat.Io.Put_Line ("Client Callback");
   end Callback;

end Block_Client;
