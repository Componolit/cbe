
package Component with
   SPARK_Mode
is

   procedure Construct with
      Export,
      Convention => C,
      External_Name => "ada_component_construct";

   procedure Event;

end Component;
