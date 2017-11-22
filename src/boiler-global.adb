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

with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.Model; use RDF.Redland.Model;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Statement; use RDF.Redland.Statement;
with RDF.Redland.Storage; use RDF.Redland.Storage;
with RDF.Redland.Stream; use RDF.Redland.Stream;

package body Boiler.Global is

   procedure Initialize (Object: in out Global_State_Type) is
--        World: World_Type renames Object.Initialization_Redland_World;
      World: Redland_World_Type;
      Storage: Storage_Type := Create(World, "memory", "initialization");
      Model: Model_Type := Create(World, Storage);
   begin
      Load(Model, From_Filename(World, Get_Data_Dir(Object.Directories_Config) & "/subclasses.rdf"));
      declare
         Predicate: constant Node_Type :=
           From_URI_String(World, "http://www.w3.org/2000/01/rdf-schema#subClassOf");
         Pattern: constant Statement_Type :=
           From_Nodes(World,
                      Node_Type_Without_Finalize'(From_Handle(null)),
                      Predicate,
                      Node_Type_Without_Finalize'(From_Handle(null)));
         Stream: Stream_Type := Find(Model, Pattern);
      begin
         for X in Create_Stream_Iterator(Stream) loop
            declare
               use URI_To_Access;
               Statement: constant Statement_Type := Copy(Get_Object(Stream));
               function Add_To_Map (URI: URI_Type) return Node is
                  Elt: constant URI_To_Access.Cursor := Find(Object.URIs_Map, URI);
               begin
                  if Elt /= No_Element then
                     return Element(Elt);
                  else
                     declare
                        The_Node: constant Node := new Null_Record'(null record);
                     begin
                        Insert(Object.URIs_Map, URI, The_Node);
                        return The_Node;
                     end;
                  end if;
               end;
            begin
               Connect(Add_To_Map(Copy(Get_URI(Get_Subject(Statement)))),
                       Add_To_Map(Copy(Get_URI(Get_Object (Statement)))),
                       Acyclic => True);
            end;
         end loop;
      end;
   end;

   -- TODO: Add caching for speed?
   function Is_Subclass (Global: Global_State_Type; Sub, Super: URI_Type_Without_Finalize'Class) return Boolean is
      Sub2  : constant URI_Type := Copy(Sub  );
      Super2: constant URI_Type := Copy(Super);
      Sub3, Super3: Sub_Classes_Graph.Node;
   begin
      begin
         Sub3   := Global.URIs_Map.Element(Sub2  );
         Super3 := Global.URIs_Map.Element(Super2);
      exception
         when Constraint_Error =>
            return False;
      end;
      return Is_Ancestor(Sub3, Super3);
   end;

   function Get_Directories (Global: Global_State_Type) return access constant Directories_Config_Type is
   begin
      return Global.Directories_Config'Unchecked_Access;
   end;

end Boiler.Global;
