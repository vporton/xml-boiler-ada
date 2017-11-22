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

package Boiler.RDF_Recursive_Descent.Literals is

   package String_Node is new Base_Node(String);

   type String_Literal_Parser is new String_Node.Base_Node_Parser with null record;

   overriding function Parse (Context: Parser_Context_Type;
                              Parser: String_Literal_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return String;

   package Integer_Node is new Base_Node(Integer);

   type Integer_Literal_Parser is new Integer_Node.Base_Node_Parser with null record;

   overriding function Parse (Context: Parser_Context_Type;
                              Parser: Integer_Literal_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Integer;

   package Float_Node is new Base_Node(Long_Float);

   type Float_Literal_Parser is new Float_Node.Base_Node_Parser with null record;

   overriding function Parse (Context: Parser_Context_Type;
                              Parser: Float_Literal_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Long_Float;

   package Boolean_Node is new Base_Node(Boolean);

   type Boolean_Literal_Parser is new Boolean_Node.Base_Node_Parser with null record;

   overriding function Parse (Context: Parser_Context_Type;
                              Parser: Boolean_Literal_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Boolean;

end Boiler.RDF_Recursive_Descent.Literals;
