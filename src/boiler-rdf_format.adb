package body Boiler.RDF_Format is

   procedure Check_Node_Class (Global: Boiler.Global.Global_State_Type;
                               Context: Parser_Context_Type;
                               Model: Model_Type_Without_Finalize'Class;
                               Node: Node_Type_Without_Finalize'Class;
                               Class: URI_Type_Without_Finalize'Class) is
      function Is_Subclass (Sub, Super: URI_Type_Without_Finalize'Class) return Boolean is
      begin
         return Boiler.Global.Is_Subclass (Global, Sub, Super);
      end;
   begin
      Check_Node_Class(Is_Subclass'Access, Context, Model, Node, Class);
   end;

end Boiler.RDF_Format;
