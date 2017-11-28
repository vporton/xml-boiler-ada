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

with Boiler.RDF_Recursive_Descent.Literals; use Boiler.RDF_Recursive_Descent.Literals;
with RDF.Redland.URI; use RDF.Redland.URI;

package body Boiler.RDF_Format.Resource.Parser is

   use all type URI_String;

   package Script_Info_Node             is new Base_Node(Script_Info_Class);
   package Base_Script_Info_Node        is new Base_Node(Script_Info);
   package Command_Script_Info_Node     is new Base_Node(Script_Info_Class);
   package Web_Service_Script_Info_Node is new Base_Node(Script_Info_Class);

   type Base_Script_Info_Parser is new Base_Script_Info_Node.Base_Node_Parser with
      record
         Script_Kind: Script_Kind_Enum;
      end record;

   type Command_Script_Info_Parser is new Command_Script_Info_Node.Base_Node_Parser with
      record
         Script_Kind: Script_Kind_Enum;
      end record;

   overriding function Parse (Context: Parser_Context_Type'Class;
                              Parser: Base_Script_Info_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Script_Info is
      package CSP_Parser is new Simple_Zero_One_Predicate(Long_Float, 1.0);
   begin
      -- No need to check the namespace because this function can be called only from below Parse functions
      --Check_Node_Class (Context, Model, Node, From_String(Context.World, Main_Namespace & "??"));
      return Info: Script_Info(Script_Kind => Parser.Script_Kind) do
         declare
            Float_Parser: aliased Float_Literal_Parser;
            Completeness_Parser: constant CSP_Parser.Zero_One_Predicate_Parser :=
              (Predicate => From_String(Context.World.all, Main_Namespace & "completeness"),
               Child_Parser => Float_Node.Base_Node_Parser'Class(Float_Parser)'Access,
               On_Error => <>);
            Stability_Parser   : constant CSP_Parser.Zero_One_Predicate_Parser :=
              (Predicate => From_String(Context.World.all, Main_Namespace & "stability"));
            Preference_Parser  : constant CSP_Parser.Zero_One_Predicate_Parser :=
              (Predicate => From_String(Context.World.all, Main_Namespace & "preference"));
         begin
            Info.Completeness := Parse(Context, Completeness_Parser, Model, Node);
            Info.Stability    := Parse(Context, Stability_Parser   , Model, Node);
            Info.Preference   := Parse(Context, Preference_Parser  , Model, Node);
            -- TODO: Transformer and Validator Info
         end;
      end return;
   end;

   overriding function Parse (Context: Parser_Context_Type'Class;
                              Parser: Command_Script_Info_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Script_Info_Class is
   begin
      Check_Node_Class (Context, Model, Node, From_String(Context.World, Main_Namespace & "Command"));
      -- TODO
   end;

   type Web_Service_Script_Info_Parser is new Web_Service_Script_Info_Node.Base_Node_Parser with
      record
         Script_Kind: Script_Kind_Enum;
      end record;

   overriding function Parse (Context: Parser_Context_Type'Class;
                              Parser: Web_Service_Script_Info_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Script_Info_Class is
   begin
      Check_Node_Class (Context, Model, Node, From_String(Context.World, Main_Namespace & "WebService"));
      -- TODO
   end;

   package Script_Choice is new Choice(Script_Info_Class);

   function Parse_Script (Context: Parser_Context_Type'Class;
                          Model: Model_Type_Without_Finalize'Class;
                          Node: Node_Type_Without_Finalize'Class;
                          Script_Kind: Script_Kind_Enum)
                          return Script_Info_Class is
   begin
      null; -- TODO
   end;

end Boiler.RDF_Format.Resource.Parser;
