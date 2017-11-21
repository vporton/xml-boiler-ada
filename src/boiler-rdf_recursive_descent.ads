with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Model; use RDF.Redland.Model;

package Boiler.RDF_Recursive_Descent is

   -- WARNING: Don't use this parser to parse recursive data structures,
   -- because it may lead to infinite recursion on circular RDF.

   -- TODO: Logger and booelan mode whether produce a warning on a subconstuct
   -- TODO: Another option to make an error fatal

   Parse_Error, Fatal_Parse_Error: exception;

   type Error_Enum is (Ignore, Warning, Fatal);

   type Log_Level_Enum is (Debug, Warning, Fatal);

   type Logger_Type is tagged private;

   procedure Log (Logger: Logger_Type; Message: String; Log_Level: Log_Level_Enum) is null;

   type Parser_Context_Type is
      record
         World : access Redland_World_Type_Without_Finalize'Class;
         Logger: access Logger_Type'Class;
      end record;

   procedure Raise_Warning (Context: Parser_Context_Type; On_Error: Error_Enum; Message: String);

   generic
      type Data_Type (<>) is private;
   package Base_Predicate is

      type Base_Predicate_Parser is abstract tagged limited
         record
            Predicate: URI_Type;
            On_Error: Error_Enum := Ignore;
         end record;

      -- TODO: condition to restrict Node to only URI or Blank
      not overriding function Parse (Context: Parser_Context_Type;
                                     Parser: Base_Predicate_Parser;
                                     Model: Model_Type_Without_Finalize'Class;
                                     Node: Node_Type_Without_Finalize'Class)
                                     return Data_Type
                                     is abstract;

   end Base_Predicate;

   generic
      type Data_Type (<>) is private; -- for indefinite types use indefinite holder
   package Base_Node is

      type Base_Node_Parser is abstract tagged limited
         record
            On_Error: Error_Enum := Ignore;
         end record;

      not overriding function Parse (Context: Parser_Context_Type;
                                     Parser: Base_Node_Parser;
                                     Model: Model_Type_Without_Finalize'Class;
                                     Node: Node_Type_Without_Finalize'Class)
                                     return Data_Type
                                     is abstract;

   end Base_Node;

   generic
      type Child_Type (<>) is private;
   package One_Predicate is
      package Node_Parser is new Base_Node(Child_Type);
      package Predicate_Parser is new Base_Predicate(Child_Type);
      type One_Predicate_Parser is new Predicate_Parser.Base_Predicate_Parser with
         record
            Child_Parser: access Node_Parser.Base_Node_Parser'Class;
         end record;
      overriding function Parse (Context: Parser_Context_Type;
                                 Parser: One_Predicate_Parser;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Node: Node_Type_Without_Finalize'Class)
                                 return Child_Type;
   end One_Predicate;

   generic
      type Child_Type (<>) is private;
      type Holder_Type (<>) is private;
      with function To_Holder (From: Child_Type) return Holder_Type;
      Default_Value: Holder_Type;
   package Zero_One_Predicate is
      package Node_Parser is new Base_Node(Child_Type);
      package Predicate_Parser is new Base_Predicate(Holder_Type);
      type Zero_One_Predicate_Parser is new Predicate_Parser.Base_Predicate_Parser with
         record
            Child_Parser: access Node_Parser.Base_Node_Parser'Class;
         end record;
      overriding function Parse (Context: Parser_Context_Type;
                                 Parser: Zero_One_Predicate_Parser;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Node: Node_Type_Without_Finalize'Class)
                                 return Holder_Type;
   end Zero_One_Predicate;

   generic
      type Child_Type (<>) is private;
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
      type Temporary_Child_Type (<>) is private;
      with function Child_Converter (From: Temporary_Child_Type) return Child_Type;
   package Zero_Or_More_Predicate is
      package Vectors is new Ada.Containers.Indefinite_Vectors(Natural, Child_Type); -- It could work faster without "Indefinite"
      package Predicate_Parser is new Base_Predicate(Vectors.Vector);
      package Node_Parser is new Base_Node(Temporary_Child_Type);
      type Zero_Or_More_Predicate_Parser is new Predicate_Parser.Base_Predicate_Parser with
         record
            Child_Parser: access Node_Parser.Base_Node_Parser'Class;
         end record;
      overriding function Parse (Context: Parser_Context_Type;
                                 Parser: Zero_Or_More_Predicate_Parser;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Node: Node_Type_Without_Finalize'Class)
                                 return Vectors.Vector;
   end Zero_Or_More_Predicate;

   generic
      type Child_Type (<>) is private;
   package Simple_Zero_Or_More_Predicate is
      function Identity (From: Child_Type) return Child_Type is (From);
      package Parent is new Zero_Or_More_Predicate(Child_Type, Child_Type, Identity);
      type Zero_Or_More_Predicate_Parser is new Parent.Zero_Or_More_Predicate_Parser with null record;
   end Simple_Zero_Or_More_Predicate;

   generic
      type Base_Type (<>) is private;
   package Choice is
      package Predicate_Parser is new Base_Predicate(Base_Type);
      type Choices_Array is array(Natural range <>) of access Predicate_Parser.Base_Predicate_Parser'Class;
      type Choice_Parser is new Predicate_Parser.Base_Predicate_Parser with
         record
            Choices: access Choices_Array;
         end record;
      overriding function Parse (Context: Parser_Context_Type;
                                 Parser: Choice_Parser;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Node: Node_Type_Without_Finalize'Class)
                                 return Base_Type;
   end Choice;

   -- No need to make this conforming to parser API
   -- Raises the exception if not match
   procedure Check_Node_Class (Is_Subclass: access function (Sub, Super: URI_Type_Without_Finalize'Class) return Boolean;
                               Context: Parser_Context_Type;
                               Model: Model_Type_Without_Finalize'Class;
                               Node: Node_Type_Without_Finalize'Class;
                               Class: URI_Type_Without_Finalize'Class);

   -- Finds trees having root of a given class (or subclasses)
   generic
      type Base_Type (<>) is private;
   package Class_Forest is
      package Vectors is new Ada.Containers.Indefinite_Vectors(Natural, Base_Type); -- It could work faster without "Indefinite"
      package Node_Parser is new Base_Node(Base_Type);
      type Class_Forest_Parser is tagged
         record
            Parser: access Node_Parser.Base_Node_Parser'Class;
            Class: URI_Type;
            Is_Subclass: access function (Sub, Super: URI_Type_Without_Finalize'Class) return Boolean;
         end record;
      function Parse (Context: Parser_Context_Type;
                      Parser: Class_Forest_Parser;
                      Model: Model_Type_Without_Finalize'Class)
                      return Vectors.Vector;
   end Class_Forest;

private

   type Logger_Type is tagged null record;

end Boiler.RDF_Recursive_Descent;
