package body Boiler.RDF_Recursive_Descent.Literals is

   function Parse (World: Redland_World_Type_Without_Finalize'Class;
                   Parser: String_Literal_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class)
                   return String is
   begin
      if not Is_Literal(Node) or else As_String(Get_Datatype_URI(Node)) /= "http://www.w3.org/2001/XMLSchema#string" then
         raise Parse_Error;
      end if;
      return As_String(Node);
   end;

   function Parse (World: Redland_World_Type_Without_Finalize'Class;
                   Parser: Integer_Literal_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class)
                   return Integer is
   begin
      if not Is_Literal(Node) or else As_String(Get_Datatype_URI(Node)) /= "http://www.w3.org/2001/XMLSchema#integer" then
         raise Parse_Error;
      end if;
      declare
         pragma Unsuppress(Range_Check);
      begin
         return Integer'Value(As_String(Node));
      exception
         when Constraint_Error =>
            raise Parse_Error;
      end;
   end;

end Boiler.RDF_Recursive_Descent.Literals;
