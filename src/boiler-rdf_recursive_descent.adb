with RDF.Redland.Node_Iterator; use RDF.Redland.Node_Iterator;

package body Boiler.RDF_Recursive_Descent is

--     package body Zero_One_Predicate is
--     end Zero_One_Predicate;

   package body Zero_Or_More_Predicate is

      procedure Parse(World: Redland_World_Type_Without_Finalize'Class;
                      Parser: Zero_Or_More_Predicate_Parser;
                      Model: Model_Type_Without_Finalize'Class;
                      Node: Node_Type_Without_Finalize'Class;
                      Data: Predicate_Parser.Data_Access) is
         Iter: Node_Iterator_Type :=
           Get_Targets (Model, Node, From_URI(World, Parser.Predicate));
         use type Predicate_Parser.Data_Access;
         use Vectors, Node_Parser;
      begin
         if Data /= null then
            Clear(Data.all);
         end if;
         while not Is_End(Iter) loop
            declare
               Elt: aliased Temporary_Child_Type;
            begin
               Parse(World,
                     Parser.Child_Parser.all,
                     Model,
                     Get_Node(Iter),
                     (if Data /= null then Elt'Unchecked_Access else null));
               if Data /= null then
                  Append(Data.all, Child_Converter(Elt)); -- TODO: Add "converter" to change element type (for example from indefinite holder)
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
