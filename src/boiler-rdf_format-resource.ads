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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Boiler.RDF_Format.Resource is

   type Script_Kind_Enum is (Transformer, Validator);

   type Transformer_Kind_Enum is (Entire, Sequential, Up_Down, Down_Up);

   type Validator_Kind_Enum is (Entire, Parts);

   package String_Holder is new Ada.Containers.Indefinite_Holders(String);

   type Script_Info (Script_Kind: Script_Kind_Enum) is tagged
      record
         Completeness, Stability, Preference: Long_Float; -- or better just Float?
         -- TODO: Distinguishing transformer and validator scripts does not conform to the specification
         case Script_Kind is
            when Transformer =>
               Transformer_Kind: Transformer_Kind_Enum;
            when Validator =>
               Validator_Kind  : Validator_Kind_Enum;
               OK_Result: String_Holder.Holder;
         end case;
      end record;

   type Command_Script_Invocation is (Command, URL);

   type Command_Script_Info (Script_Kind: Script_Kind_Enum; Invocation: Command_Script_Invocation) is
     new Script_Info(Script_Kind) with
      record
         Language: URI_Type;
         Min_Version, Max_Version: String_Holder.Holder;
         case Invocation is
            when Command =>
               Command_String: Unbounded_String;
            when URL =>
               Script_URL:     Unbounded_String;
         end case;
      end record;

   type Web_Service_Script_Info is new Script_Info with
      record
         Action: RDF.Redland.URI.URI_Type;
         Method: Unbounded_String;
         XML_Field: Unbounded_String;
      end record;

end Boiler.RDF_Format.Resource;
