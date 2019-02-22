
package Ada_Block_Test is

   procedure Run
      with
      Export,
      Convention => C,
      External_Name => "ada_block_test_run";

end Ada_Block_Test;
