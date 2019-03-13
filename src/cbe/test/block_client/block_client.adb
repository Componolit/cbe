with Gnat.Io;

package body Block_Client is

   procedure Callback
   is
   begin
      Gnat.Io.Put_Line ("Client Callback");
   end Callback;

end Block_Client;
