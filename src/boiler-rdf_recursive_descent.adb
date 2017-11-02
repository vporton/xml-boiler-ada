with RDF.Redland.Node_Iterator; use RDF.Redland.Node_Iterator;

package body Boiler.RDF_Recursive_Descent is

   procedure Parse (Parser: Base_Node_Parser;
                    Model: Model_Type_Without_Finalize'Class;
                    Node: Node_Type_Without_Finalize'Class;
                    Data: access all Data_Type) is
   begin
      for I in Parser.Predicates loop
         Parse(World, Element(I), Model, Node, Data); -- FIXME: Where to get an array of Data?
      end loop;
   end;

   package body Zero_One_Predicate is
   end Zero_One_Predicate;

   package body Zero_Or_More_Predicate is

      procedure Parse(World: Redland_World_Type_Without_Finalize'Class;
                      Parser: Zero_Or_More_Predicate_Parser;
                      Model: Model_Type_Without_Finalize'Class;
                      Node: Node_Type_Without_Finalize'Class;
                      Data: access all Vectors.Vector) is
         Iter: Node_Iterator_Type :=
           Get_Targets (Model, Node, From_URI(World, Parser.Predicate));
         Elt: aliased Child_Type;
      begin
         if Data /= null then
            Clear(Vector);
         end if;
         while not Is_End(Iter) loop
            begin
               Parse(World,
                     Parser.Child_Parser,
                     Model,
                     Get_Node(Iter),
                     (if Data /= null then Elt'Access else null));
               if Data /= null then
                  Append(Data.all, Elt);
               end if;
            exception
               when Parse_Error =>
                  null; -- ignore non-parsed childs
            end;
            Next(Iter);
         end loop;
      end;

   end Zero_Or_More_Predicate;

end Boiler.RDF_Recursive_Descent;
