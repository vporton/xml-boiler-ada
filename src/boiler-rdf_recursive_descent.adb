with RDF.Redland.Node_Iterator; use RDF.Redland.Node_Iterator;

package body Boiler.RDF_Recursive_Descent is

   package body Zero_One_Predicate is

      function Parse (World: Redland_World_Type_Without_Finalize'Class;
                      Parser: Zero_One_Predicate_Parser;
                      Model: Model_Type_Without_Finalize'Class;
                      Node: Node_Type_Without_Finalize'Class)
                      return Holder_Type is
         Iterator: Node_Iterator_Type :=
           Get_Targets(Model, Node, From_URI(World, Parser.Predicate));
         Child_Nodes: constant Node_Array := To_Array(Iterator);
         use Predicate_Parser;
      begin
         if Child_Nodes'Length /= 1 then
            return Default_Value;
         end if;
         declare
            Child_Node: Node_Type renames Child_Nodes(Child_Nodes'First);
            use Node_Parser;
         begin
            return To_Holder(Parse(World,
                             Parser.Child_Parser.all,
                             Model,
                             Child_Node));
         exception
            when Parse_Error =>
               return Default_Value;
         end;
      end;

   end Zero_One_Predicate;

   package body Zero_Or_More_Predicate is

      function Parse (World: Redland_World_Type_Without_Finalize'Class;
                      Parser: Zero_Or_More_Predicate_Parser;
                      Model: Model_Type_Without_Finalize'Class;
                      Node: Node_Type_Without_Finalize'Class)
                      return Vectors.Vector is
         Iter: Node_Iterator_Type :=
           Get_Targets (Model, Node, From_URI(World, Parser.Predicate));
         use Vectors, Node_Parser;
      begin
         return Result: Vectors.Vector do
            while not Is_End(Iter) loop
               begin
                  Append(Result, Child_Converter(Parse(World,
                         Parser.Child_Parser.all,
                         Model,
                         Get_Node(Iter))));
               exception
                  when Parse_Error =>
                     null; -- ignore non-parsed childs
               end;
               Next(Iter);
            end loop;
         end return;
      end;

   end Zero_Or_More_Predicate;

   package body Choice is

      function Parse (World: Redland_World_Type_Without_Finalize'Class;
                      Parser: Choice_Parser;
                      Model: Model_Type_Without_Finalize'Class;
                      Node: Node_Type_Without_Finalize'Class)
                      return Base_Type is
         use Predicate_Parser;
      begin
         for C of Parser.Choices.all loop
            begin
               return Parse(World, C.all, Model, Node);
            exception
               when Parse_Error =>
                  null; -- do next loop iteration
            end;
         end loop;
         raise Parse_Error; -- no variant found
      end;

   end Choice;

   procedure Check_Node_Class (Is_Subclass: access function (Sub, Super: URI_Type_Without_Finalize'Class) return Boolean;
                               World: Redland_World_Type_Without_Finalize'Class;
                               Model: Model_Type_Without_Finalize'Class;
                               Node: Node_Type_Without_Finalize'Class;
                               Class: URI_Type_Without_Finalize'Class) is
      Iterator: Node_Iterator_Type :=
        Get_Targets(Model, Node, From_URI_String(World, "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"));
   begin
      while not Is_End(Iterator) loop
         declare
            Node: Node_Type_Without_Finalize renames Get_Node(Iterator);
         begin
            if Is_Resource(Node) and then Is_Subclass(Get_URI(Node), Class) then
               return;
            end if;
         end;
         Next(Iterator);
      end loop;
      raise Parse_Error;
   end;

end Boiler.RDF_Recursive_Descent;
