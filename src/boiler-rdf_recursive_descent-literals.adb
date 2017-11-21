package body Boiler.RDF_Recursive_Descent.Literals is

   function Parse (World: Redland_World_Type_Without_Finalize'Class;
                   Parser: String_Literal_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class;
                   Logger: Logger_Type'Class)
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
                   Node: Node_Type_Without_Finalize'Class;
                   Logger: Logger_Type'Class)
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

   function Parse (World: Redland_World_Type_Without_Finalize'Class;
                   Parser: Float_Literal_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class;
                   Logger: Logger_Type'Class)
                   return Long_Float is
   begin
      if not Is_Literal(Node) then
         raise Parse_Error;
      end if;
      declare
         Datatype: constant String := As_String(Get_Datatype_URI(Node));
      begin
         if Datatype = "http://www.w3.org/2001/XMLSchema#integer" or else
           Datatype = "http://www.w3.org/2001/XMLSchema#float" or else
           Datatype = "http://www.w3.org/2001/XMLSchema#double" or else
           Datatype = "http://www.w3.org/2001/XMLSchema#decimal"
         then
            return Long_Float'Value(As_String(Node));
         end if;
         raise Parse_Error;
      end;
   end;

   function Parse (World: Redland_World_Type_Without_Finalize'Class;
                   Parser: Boolean_Literal_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class;
                   Logger: Logger_Type'Class)
                   return Boolean is
   begin
      if not Is_Literal(Node) or else As_String(Get_Datatype_URI(Node)) /= "http://www.w3.org/2001/XMLSchema#boolean" then
         raise Parse_Error;
      end if;
      declare
         Str: constant String := As_String(Node);
      begin
         if Str = "true" or Str = "1" then
            return True;
         end if;
         if Str = "false" or Str = "0" then
            return True;
         end if;
      end;
      raise Parse_Error;
   end;

end Boiler.RDF_Recursive_Descent.Literals;
