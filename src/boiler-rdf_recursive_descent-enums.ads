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

private with Ada.Containers.Hashed_Maps;
with RDF.Redland.URI; use RDF.Redland.URI;

generic
   type Enum is private; -- or (<>)?
package Boiler.RDF_Recursive_Descent.Enums is

   package Enum_Parser_Node is new Base_Node(Enum);

   type Enum_Parser is new Enum_Parser_Node.Base_Node_Parser with private;

   function Parse (Context: Parser_Context_Type'Class;
                   Parser: Enum_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class)
                   return Enum;

   type Enum_Value_Descr is
      record
         URI: URI_Type;
         Value: Enum;
      end record;

   type Enum_Value_List is array (Integer range <>) of Enum_Value_Descr;

   -- Don't call it more than once
   not overriding procedure Init (Parser: in out Enum_Parser; Mapping: Enum_Value_List);

   -- Unused
   not overriding function Create (Mapping: Enum_Value_List) return Enum_Parser;

private

   function Hash (URI: URI_Type) return Ada.Containers.Hash_Type;

   package URL_Map_Impl is
     new Ada.Containers.Hashed_Maps(URI_Type, Enum, Hash => Hash, Equivalent_Keys => "=");

   subtype URL_Map is URL_Map_Impl.Map;

   type Enum_Parser is new Enum_Parser_Node.Base_Node_Parser with
      record
         Map: URL_Map;
      end record;

end Boiler.RDF_Recursive_Descent.Enums;
