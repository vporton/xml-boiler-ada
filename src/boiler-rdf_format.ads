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

with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Model; use RDF.Redland.Model;
with Boiler.RDF_Recursive_Descent; use Boiler.RDF_Recursive_Descent;
limited with Boiler.Global;

package Boiler.RDF_Format is

   Main_Namespace: constant URI_String := "http://portonvictor.org/ns/trans/";

   type Boiler_Context_Type is new Parser_Context_Type with
      record
         Global: access Boiler.Global.Global_State_Type;
      end record;

   procedure Check_Node_Class (Context: Boiler_Context_Type'Class;
                               Model: Model_Type_Without_Finalize'Class;
                               Node: Node_Type_Without_Finalize'Class;
                               Class: URI_Type_Without_Finalize'Class);

end Boiler.RDF_Format;
