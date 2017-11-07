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

      type Data_Access is access all Data_Type;

      type Base_Predicate_Parser is tagged
         record
            Predicate: URI_Type;
--              Data: Data_Access; -- also make a version where it is a part of a larger record?
         end record;

      -- TODO: condition to restrict Node to only URI or Blank
      not overriding procedure Parse (World: Redland_World_Type_Without_Finalize'Class;
                                      Parser: Base_Predicate_Parser;
                                      Model: Model_Type_Without_Finalize'Class;
                                      Node: Node_Type_Without_Finalize'Class;
                                      Data: Data_Access) is null;

   end Base_Predicate;

   generic
      type Data_Type is private; -- for indefinite types use indefinite holder
   package Base_Node is

      type Data_Access is access all Data_Type;

      type Base_Node_Parser is tagged limited null record;

      not overriding procedure Parse (World: Redland_World_Type_Without_Finalize'Class;
                                      Parser: Base_Node_Parser;
                                      Model: Model_Type_Without_Finalize'Class;
                                      Node: Node_Type_Without_Finalize'Class;
                                      Data: Data_Access) is null;

   end Base_Node;

   generic
      type Child_Type is private;
      type Holder_Type is private;
      with function To_Holder (From: Child_Type) return Holder_Type;
      Default_Value: Holder_Type;
   package Zero_One_Predicate is
      package Node_Parser is new Base_Node(Child_Type);
      package Predicate_Parser is new Base_Predicate(Holder_Type);
      type Zero_One_Predicate_Parser is new Predicate_Parser.Base_Predicate_Parser with
         record
            Child_Parser: access Node_Parser.Base_Node_Parser'Class;
         end record;
      overriding procedure Parse(World: Redland_World_Type_Without_Finalize'Class;
                                 Parser: Zero_One_Predicate_Parser;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Node: Node_Type_Without_Finalize'Class;
                                 Data: Predicate_Parser.Data_Access);
   end Zero_One_Predicate;

   generic
      type Child_Type is private;
   package Simple_Zero_One_Predicate is
      package Holders is new Ada.Containers.Indefinite_Holders(Child_Type);
      package Parent is new Zero_One_Predicate(Child_Type,
                                               Holders.Holder,
                                               Holders.To_Holder,
                                               Holders.Empty_Holder);
      type Zero_One_Predicate_Parser is new Parent.Zero_One_Predicate_Parser with null record;
   end Simple_Zero_One_Predicate;

   generic
      type Child_Type (<>) is private;
      type Temporary_Child_Type is private;
      with function Child_Converter (From: Temporary_Child_Type) return Child_Type;
   package Zero_Or_More_Predicate is
      package Vectors is new Ada.Containers.Indefinite_Vectors(Natural, Child_Type); -- It could work faster without "Indefinite"
      package Predicate_Parser is new Base_Predicate(Vectors.Vector);
      package Node_Parser is new Base_Node(Temporary_Child_Type);
      type Zero_Or_More_Predicate_Parser is new Predicate_Parser.Base_Predicate_Parser with
         record
            Child_Parser: access Node_Parser.Base_Node_Parser'Class;
         end record;
      overriding procedure Parse(World: Redland_World_Type_Without_Finalize'Class;
                                 Parser: Zero_Or_More_Predicate_Parser;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Node: Node_Type_Without_Finalize'Class;
                                 Data: Predicate_Parser.Data_Access);
   end Zero_Or_More_Predicate;

   generic
      type Child_Type is private;
   package Simple_Zero_Or_More_Predicate is
      function Identity (From: Child_Type) return Child_Type is (From);
      package Parent is new Zero_Or_More_Predicate(Child_Type, Child_Type, Identity);
      type Zero_Or_More_Predicate_Parser is new Parent.Zero_Or_More_Predicate_Parser with null record;
   end Simple_Zero_Or_More_Predicate;

end Boiler.RDF_Recursive_Descent;
