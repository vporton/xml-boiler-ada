with RDF.Redland.Statement; use RDF.Redland.Statement;
with RDF.Redland.Node_Iterator; use RDF.Redland.Node_Iterator;
with RDF.Redland.Stream; use RDF.Redland.Stream;

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
            Child_Node: constant Node_Type := Child_Nodes(Child_Nodes'First);
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
            Target: constant Node_Type_Without_Finalize := Get_Node(Iterator);
         begin
            if Is_Resource(Target) and then Is_Subclass(Get_URI(Target), Class) then
               return;
            end if;
         end;
         Next(Iterator);
      end loop;
      raise Parse_Error;
   end;

   package body Class_Forest is

      function Parse (World: Redland_World_Type_Without_Finalize'Class;
                      Parser: Class_Forest_Parser;
                      Model: Model_Type_Without_Finalize'Class)
                      return Vectors.Vector is
         Pattern: constant Statement_Type :=
           From_Nodes(World,
                      Node_Type_Without_Finalize'(From_Handle(null)),
                      From_URI_String(World, "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                      Node_Type_Without_Finalize'(From_Handle(null)));
         Stream: constant Stream_Type := Find(Model, Pattern);
      begin
         return V: Vectors.Vector do
            while not Is_End(Stream) loop -- slow algorithm
               declare
                  -- TODO: Unfortunate clash of two Get_Object functions. Change RDF.Redland?
                  Statement: constant Statement_Type_Without_Finalize := Get_Object(Stream);
                  Node: constant Node_Type_Without_Finalize := Get_Object(Statement);
                  Class_Node: constant Node_Type_Without_Finalize := Get_Subject(Statement);
               begin
                  if Is_Resource(Class_Node) then
                     declare
                        Class: constant URI_Type_Without_Finalize := Get_URI(Class_Node);
                        use Node_Parser;
                     begin
                        Check_Node_Class(Parser.Is_Subclass, World, Model, Node, Class);
                        V.Append(Parse(World, Parser.Parser.all, Model, Node));
                     exception
                        when Parse_Error =>
                           null;
                     end;
                  end if;
               end;
               Next(Stream);
            end loop;
         end return;
      end;

   end Class_Forest;

end Boiler.RDF_Recursive_Descent;
