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

with Ada.Containers;
with Ada.Strings.Hash;
with Boiler.Auxiliary.String_Formatter; use Boiler.Auxiliary.String_Formatter;

package body Boiler.RDF_Recursive_Descent.Enums is

   function Hash (URI: URI_Type) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash(As_String(URI));
   end;

   function Parse (Context: Parser_Context_Type'Class;
                   Parser: Enum_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class)
                   return Enum is
   begin
      -- FIXME: What to do if Is_Blank(Node)?
      if not Is_Resource(Node) then
         declare
            function Message return String is
            begin
               return To_String(Fmt("Node {1} should be a IRI.") & Format_As_String(Node));
            end;
         begin
            Raise_Warning(Context, Parser.On_Error, Message'Access);
         end;
      end if;
      declare
         URI: constant URI_Type := Copy(Get_URI(Node));
         use URL_Map_Impl;
         C: constant URL_Map_Impl.Cursor := Find(Parser.Map, URI);
      begin
         if C = No_Element then
            declare
               function Message return String is
               begin
                  return To_String(Fmt("IRI <{1}> is unknown.") & As_String(URI));
               end;
            begin
               Raise_Warning(Context, Parser.On_Error, Message'Access);
            end;
         end if;
         return Parser.Map(URI);
      end;
   end;

   function Create (Mapping: Enum_Value_List) return Enum_Parser is
   begin
      return Result: Enum_Parser do
         for Pair of Mapping loop
            Result.Map.Insert(Pair.URI, Pair.Value);
         end loop;
      end return;
   end;

end Boiler.RDF_Recursive_Descent.Enums;
