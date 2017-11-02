package Boiler.RDF_Recursive_Descent is

   type Base_Predicate_Parser is limited
      record
         Predicate: URI_Type;
      end record;

   -- TODO: condition to restrict Node to only URI or Blank
   not overriding procedure Parse(Parser: Base_Predicate_Parser; Node: Node_Type) is null;

   package Predicate_List is new Ada.Containers.Vectors(Base_Predicate_Parser'Class);

   type Base_Node_Parser is limited tagged
      record
         Predicates: Predicate_List.Vector;
      end record;

   not overriding procedure Parse(Parser: Base_Node_Parser; Node: Node_Type);

   generic
      type Child_Type is private;
   package Zero_One_Predicate is
      package Holders is Ada.Containers.Indefinite_Holders(Child_Type);
      type Zero_One_Predicate_Parser is new Base_Predicate_Parser with
         record
            Child: access Holders.Holder;
         end record;
      overriding procedure Parse(Parser: Zero_One_Predicate_Parser; Node: Node_Type);
   end;

end Boiler.RDF_Recursive_Descent;
