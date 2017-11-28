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

package body Boiler.RDF_Format.Resource.Parser is

   function Parse (Context: Parser_Context_Type'Class;
                   Parser: Script_Info_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class)
                   return Script_Info_Class is
   begin
      -- FIXME
      pragma Warnings(Off);
      return Parse (Context, Parser, Model, Node);
   end;

end Boiler.RDF_Format.Resource.Parser;
