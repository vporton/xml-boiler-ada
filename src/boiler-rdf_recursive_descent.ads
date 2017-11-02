with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Model; use RDF.Redland.Model;

package Boiler.RDF_Recursive_Descent is

   Parse_Error: exception;

   generic
      type Data_Type is private; -- for indefinite types use indefinite holder
   package Base_Predicate is

      type Base_Predicate_Parser is tagged
         record
            Predicate: URI_Type;
            Data: access all Data_Type;
         end record;

      -- TODO: condition to restrict Node to only URI or Blank
      not overriding procedure Parse (World: World_Type_Without_Finalize'Class;
                                      Parser: Base_Predicate_Parser;
                                      Model: Model_Type_Without_Finalize'Class;
                                      Node: Node_Type_Without_Finalize'Class) is null;

   end Base_Predicate;

   package Predicate_List is new Ada.Containers.Indefinite_Vectors(Natural, Base_Predicate_Parser'Class);

   type Base_Node_Parser is limited
      record
         Predicates: Predicate_List.Vector;
      end record;

   not overriding procedure Parse (World: World_Type_Without_Finalize'Class;
                                   Parser: Base_Node_Parser;
                                   Model: Model_Type_Without_Finalize'Class;
                                   Node: Node_Type_Without_Finalize'Class);

   generic
      type Child_Type is private;
   package Zero_One_Predicate is
      -- FIXME: Can we use a variant record instead?
      package Holders is new Ada.Containers.Indefinite_Holders(Child_Type);
      type Zero_One_Predicate_Parser is new Base_Predicate_Parser with
         record
--              Convert_Child: access function ()
            Child: access Holders.Holder;
         end record;
      overriding procedure Parse(World: World_Type_Without_Finalize'Class;
                                 Parser: Zero_One_Predicate_Parser;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Node: Node_Type_Without_Finalize'Class);
   end Zero_One_Predicate;

   generic
      type Child_Type is private;
   package Zero_Or_More_Predicate is
      package Vectors is new Ada.Containers.Indefinite_Vectors(Child_Type);
      package Holders is new Ada.Containers.Indefinite_Holders(Child_Type); -- TODO: Move to body?
      package Parser is new Base_Predicate_Parser(Child_Type);
      type Zero_Or_More_Predicate_Parser is new Parser.Base_Predicate_Parser with
         record
            Child_Parser: Base_Node_Parser;
         end record;
      overriding procedure Parse(World: World_Type_Without_Finalize'Class;
                                 Parser: Zero_Or_More_Predicate_Parser;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Node: Node_Type_Without_Finalize'Class);
   end Zero_One_Predicate;

end Boiler.RDF_Recursive_Descent;
