with Boiler.RDF_Recursive_Descent; use Boiler.RDF_Recursive_Descent;

package body Boiler.RDF_Format is

   procedure Check_Node_Class (Global: Boiler.Global.Global_State_Type;
                               World: Redland_World_Type_Without_Finalize'Class;
                               Model: Model_Type_Without_Finalize'Class;
                               Node: Node_Type_Without_Finalize'Class;
                               Class: URI_Type_Without_Finalize'Class) is
      function Is_Subclass (Sub, Super: URI_Type_Without_Finalize'Class) return Boolean is
      begin
         return Boiler.Global.Is_Subclass (Global, Sub, Super);
      end;
   begin
      Check_Node_Class(Is_Subclass'Access, World, Model, Node, Class);
   end;

end Boiler.RDF_Format;
