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

package Boiler.RDF_Format.Resource is

   type Transformer_Kind_Enum is (Entire, Sequential, Up_Down, Down_Up);

   type Validator_Kind_Enum is (Entire, Parts);

   package String_Holder is new Ada.Containers.Indefinite_Holders(String);

   type Script_Info is
      record
         Language: URI_Type;
         Min_Version, Max_Version: String_Holder.Holder;
         Script_URL:     String_Holder.Holder;
         Command_String: String_Holder.Holder;
         OK_Result: String_Holder.Holder;
         Completeness, Stability, Preference: Long_Float; -- or better just Float?
         Transformer_Kind: Transformer_Kind_Enum; -- unused when validation
         Validator_Kind  : Validator_Kind_Enum;   -- unused when transforming
      end record;

end Boiler.RDF_Format.Resource;
