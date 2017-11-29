--  Copyright 2017 Victor Porton
--
--  This file is part of XML Boiler.
--
--  XML Boiler is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  XML Boiler is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with XML Boiler.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Model; use RDF.Redland.Model;

package Boiler.RDF_Recursive_Descent is

   -- WARNING: Don't use this parser to parse recursive data structures,
   -- because it may lead to infinite recursion on circular RDF.

   Parse_Error, Fatal_Parse_Error: exception;

   type Error_Enum is (Ignore, Warning, Fatal);

   type Log_Level_Enum is (Debug, Warning, Fatal);

   type Logger_Type is tagged private;

   procedure Log (Logger: Logger_Type; Message: String; Log_Level: Log_Level_Enum) is null;

   type Parser_Context_Type is tagged
      record
         World : access Redland_World_Type_Without_Finalize'Class;
         Logger: access Logger_Type'Class;
      end record;

   procedure Raise_Warning (Context: Parser_Context_Type'Class;
                            On_Error: Error_Enum;
                            Message: access function return String)
     with No_Return;

   procedure Raise_Warning (Context: Parser_Context_Type'Class; On_Error: Error_Enum; Message: String)
     with No_Return;

   generic
      type Data (<>) is private;
   package Base_Predicate is

      subtype Data_Type is Data;

      type Base_Predicate_Parser is abstract tagged limited
         record
            Predicate: URI_Type;
            On_Error: Error_Enum := Ignore;
         end record;

      -- TODO: condition to restrict Node to only URI or Blank
      not overriding function Parse (Context: Parser_Context_Type'Class;
                                     Parser: Base_Predicate_Parser;
                                     Model: Model_Type_Without_Finalize'Class;
                                     Node: Node_Type_Without_Finalize'Class)
                                     return Data_Type
                                     is abstract;

   end Base_Predicate;

   generic
      type Data (<>) is private; -- for indefinite types use indefinite holder
   package Base_Node is

      subtype Data_Type is Data;

      type Base_Node_Parser is abstract tagged limited
         record
            On_Error: Error_Enum := Ignore;
         end record;

      not overriding function Parse (Context: Parser_Context_Type'Class;
                                     Parser: Base_Node_Parser;
                                     Model: Model_Type_Without_Finalize'Class;
                                     Node: Node_Type_Without_Finalize'Class)
                                     return Data_Type
                                     is abstract;

   end Base_Node;

   generic
      with package Node_Parser is new Base_Node(<>);
   package One_Predicate is
      subtype Child_Type is Node_Parser.Data_Type;
      package Predicate_Parser is new Base_Predicate(Child_Type);
      type One_Predicate_Parser is new Predicate_Parser.Base_Predicate_Parser with
         record
            Child_Parser: access Node_Parser.Base_Node_Parser'Class;
         end record;
      overriding function Parse (Context: Parser_Context_Type'Class;
                                 Parser: One_Predicate_Parser;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Node: Node_Type_Without_Finalize'Class)
                                 return Child_Type;
   end One_Predicate;

   generic
      with package Node_Parser is new Base_Node(<>);
      type Holder_Type (<>) is private;
      with function To_Holder (From: Node_Parser.Data_Type) return Holder_Type;
      Default_Value: Holder_Type;
   package Zero_One_Predicate is
      subtype Child_Type is Node_Parser.Data_Type;
      package Predicate_Parser is new Base_Predicate(Holder_Type);
      type Zero_One_Predicate_Parser is new Predicate_Parser.Base_Predicate_Parser with
         record
            Child_Parser: access Node_Parser.Base_Node_Parser'Class;
         end record;
      overriding function Parse (Context: Parser_Context_Type'Class;
                                 Parser: Zero_One_Predicate_Parser;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Node: Node_Type_Without_Finalize'Class)
                                 return Holder_Type;
   end Zero_One_Predicate;

   generic
      with package Node_Parser is new Base_Node(<>);
      Default_Value: Node_Parser.Data_Type;
   package Simple_Zero_One_Predicate is
      subtype Child_Type is Node_Parser.Data_Type;
      function Identity (From: Child_Type) return Child_Type is (From);
      package Parent is new Zero_One_Predicate(Node_Parser,
                                               Child_Type,
                                               Identity,
                                               Default_Value);
      type Zero_One_Predicate_Parser is new Parent.Zero_One_Predicate_Parser with null record;
   end Simple_Zero_One_Predicate;

   generic
      with package Node_Parser is new Base_Node(<>);
      with function "=" (Left, Right: Node_Parser.Data_Type) return Boolean is <>;
   package Holder_Zero_One_Predicate is
      subtype Child_Type is Node_Parser.Data_Type;
      package Holders is new Ada.Containers.Indefinite_Holders(Child_Type);
      package Parent is new Zero_One_Predicate(Node_Parser,
                                               Holders.Holder,
                                               Holders.To_Holder,
                                               Holders.Empty_Holder);
      type Zero_One_Predicate_Parser is new Parent.Zero_One_Predicate_Parser with null record;
   end Holder_Zero_One_Predicate;

   generic
      with package Node_Parser is new Base_Node(<>);
      type Child_Type (<>) is private;
      with function Child_Converter (From: Node_Parser.Data_Type) return Child_Type;
   package Zero_Or_More_Predicate is
      subtype Temporary_Child_Type is Node_Parser.Data_Type;
      package Vectors is new Ada.Containers.Indefinite_Vectors(Natural, Child_Type); -- It could work faster without "Indefinite"
      package Predicate_Parser is new Base_Predicate(Vectors.Vector);
      type Zero_Or_More_Predicate_Parser is new Predicate_Parser.Base_Predicate_Parser with
         record
            Child_Parser: access Node_Parser.Base_Node_Parser'Class;
         end record;
      overriding function Parse (Context: Parser_Context_Type'Class;
                                 Parser: Zero_Or_More_Predicate_Parser;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Node: Node_Type_Without_Finalize'Class)
                                 return Vectors.Vector;
   end Zero_Or_More_Predicate;

   generic
      with package Node_Parser is new Base_Node(<>);
   package Simple_Zero_Or_More_Predicate is
      subtype Child_Type is Node_Parser.Data_Type;
      function Identity (From: Child_Type) return Child_Type is (From);
      package Parent is new Zero_Or_More_Predicate(Node_Parser, Child_Type, Identity);
      type Zero_Or_More_Predicate_Parser is new Parent.Zero_Or_More_Predicate_Parser with null record;
   end Simple_Zero_Or_More_Predicate;

   -- FIXME: Does it make sense for other than One_Predicate predicates?
   generic
      with package Predicate_Parser is new Base_Predicate(<>);
   package Choice is
      subtype Base_Type is Predicate_Parser.Data_Type;
      type Choices_Array is array(Natural range <>) of access Predicate_Parser.Base_Predicate_Parser'Class; -- FIXME: It isn't predicate parser
      type Choice_Parser is new Predicate_Parser.Base_Predicate_Parser with
         record
            Choices: access Choices_Array;
         end record;
      overriding function Parse (Context: Parser_Context_Type'Class;
                                 Parser: Choice_Parser;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Node: Node_Type_Without_Finalize'Class)
                                 return Base_Type;
   end Choice;

   -- No need to make this conforming to parser API
   -- Raises the exception if not match
   procedure Check_Node_Class (Is_Subclass: access function (Sub, Super: URI_Type_Without_Finalize'Class) return Boolean;
                               Context: Parser_Context_Type'Class;
                               Model: Model_Type_Without_Finalize'Class;
                               Node: Node_Type_Without_Finalize'Class;
                               Class: URI_Type_Without_Finalize'Class;
                               On_Error: Error_Enum := Ignore);

   -- Finds trees having root of a given class (or subclasses)
   generic
      with package Node_Parser is new Base_Node(<>);
      with function "=" (Left, Right: Node_Parser.Data_Type) return Boolean is <>;
   package Class_Forest is
      subtype Base_Type is Node_Parser.Data_Type;
      package Vectors is new Ada.Containers.Indefinite_Vectors(Natural, Base_Type); -- It could work faster without "Indefinite"
      type Class_Forest_Parser is tagged
         record
            Parser: access Node_Parser.Base_Node_Parser'Class;
            Class: URI_Type;
            Is_Subclass: access function (Sub, Super: URI_Type_Without_Finalize'Class) return Boolean;
         end record;
      function Parse (Context: Parser_Context_Type'Class;
                      Parser: Class_Forest_Parser;
                      Model: Model_Type_Without_Finalize'Class)
                      return Vectors.Vector;
   end Class_Forest;

private

   type Logger_Type is tagged null record;

end Boiler.RDF_Recursive_Descent;
