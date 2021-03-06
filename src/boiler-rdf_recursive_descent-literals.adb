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

with Boiler.Auxiliary.String_Formatter; use Boiler.Auxiliary.String_Formatter;

package body Boiler.RDF_Recursive_Descent.Literals is

   function Parse (Context: Parser_Context_Type'Class;
                   Parser: String_Literal_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class)
                   return String is
   begin
      if not Is_Literal(Node) or else As_String(Get_Datatype_URI(Node)) /= "http://www.w3.org/2001/XMLSchema#string" then
         declare
            function Message return String is
            begin
               return To_String(Fmt("Node {1} is not a literal.") & Format_As_String(Node));
            end;
         begin
            Raise_Warning (Context, Parser.On_Error, Message'Access);
         end;
      end if;
      return As_String(Node);
   end;

   function Parse (Context: Parser_Context_Type'Class;
                   Parser: Integer_Literal_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class)
                   return Integer is
   begin
      if not Is_Literal(Node) or else As_String(Get_Datatype_URI(Node)) /= "http://www.w3.org/2001/XMLSchema#integer" then
         declare
            function Message return String is
            begin
               return To_String(Fmt("Node {1} is not an integer.") & Format_As_String(Node));
            end;
         begin
            Raise_Warning (Context, Parser.On_Error, Message'Access);
         end;
      end if;
      declare
         pragma Unsuppress(Range_Check);
      begin
         return Integer'Value(As_String(Node));
      exception
         when Constraint_Error =>
            declare
               function Message return String is
               begin
                  return To_String(Fmt("Integer {1} is ill-formatted or too big.") & As_String(Node));
               end;
            begin
               Raise_Warning (Context, Parser.On_Error, Message'Access);
            end;
      end;
   end;

   function Parse (Context: Parser_Context_Type'Class;
                   Parser: Float_Literal_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class)
                   return Long_Float is
   begin
      if not Is_Literal(Node) then
         declare
            function Message return String is
            begin
               return To_String(Fmt("Node {1} is not a floating point number.") & Format_As_String(Node));
            end;
         begin
            Raise_Warning (Context, Parser.On_Error, Message'Access);
         end;
      end if;
      declare
         Datatype: constant String := As_String(Get_Datatype_URI(Node));
      begin
         if Datatype = "http://www.w3.org/2001/XMLSchema#integer" or else
           Datatype = "http://www.w3.org/2001/XMLSchema#float" or else
           Datatype = "http://www.w3.org/2001/XMLSchema#double" or else
           Datatype = "http://www.w3.org/2001/XMLSchema#decimal"
         then
            return Long_Float'Value(As_String(Node));
         end if;
         raise Parse_Error;
      end;
   end;

   function Parse (Context: Parser_Context_Type'Class;
                   Parser: Boolean_Literal_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class)
                   return Boolean is
   begin
      if not Is_Literal(Node) or else As_String(Get_Datatype_URI(Node)) /= "http://www.w3.org/2001/XMLSchema#boolean" then
         declare
            function Message return String is
            begin
               return To_String(Fmt("Node {1} is not a boolean.") & Format_As_String(Node));
            end;
         begin
            Raise_Warning (Context, Parser.On_Error, Message'Access);
         end;
      end if;
      declare
         Str: constant String := As_String(Node);
      begin
         if Str = "true" or Str = "1" then
            return True;
         end if;
         if Str = "false" or Str = "0" then
            return True;
         end if;
      end;
      raise Parse_Error;
   end;

end Boiler.RDF_Recursive_Descent.Literals;
