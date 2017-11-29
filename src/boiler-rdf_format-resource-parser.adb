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
with Boiler.RDF_Recursive_Descent; use Boiler.RDF_Recursive_Descent;
with Boiler.RDF_Recursive_Descent.Literals; use Boiler.RDF_Recursive_Descent.Literals;
with Boiler.Global;

package body Boiler.RDF_Format.Resource.Parser is

   use all type URI_String;

   package Base_Script_Info_Node is new Base_Node(Script_Info);

   type Base_Script_Info_Parser is new Base_Script_Info_Node.Base_Node_Parser with
      record
         Script_Kind: Script_Kind_Enum;
      end record;

   overriding function Parse (Context: Parser_Context_Type'Class;
                              Parser: Base_Script_Info_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Script_Info;

   type Command_Script_Info_Parser is new Script_Info_Node.Base_Node_Parser with
      record
         Script_Kind: Script_Kind_Enum;
      end record;

   overriding function Parse (Context: Parser_Context_Type'Class;
                              Parser: Command_Script_Info_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Script_Info_Class;

   type Web_Service_Script_Info_Parser is new Script_Info_Node.Base_Node_Parser with
      record
         Script_Kind: Script_Kind_Enum;
      end record;

   overriding function Parse (Context: Parser_Context_Type'Class;
                              Parser: Web_Service_Script_Info_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Script_Info_Class;

   overriding function Parse (Context: Parser_Context_Type'Class;
                              Parser: Base_Script_Info_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Script_Info is
      package CSP_Parser is new Simple_Zero_One_Predicate(Float_Node, 1.0);
      use CSP_Parser;
      function U (Str: URI_String) return URI_Type is (From_String(Context.World.all, Str));
   begin
      -- No need to check the namespace because this function can be called only from below Parse functions
      --Check_Node_Class (Context, Model, Node, From_String(Context.World, Main_Namespace & "??"));
      return Info: Script_Info(Script_Kind => Parser.Script_Kind) do
         declare
            Float_Parser: aliased Float_Literal_Parser;
            Completeness_Parser: constant CSP_Parser.Zero_One_Predicate_Parser :=
              (Predicate => U(Main_Namespace & "completeness"),
               Child_Parser => Float_Parser'Unchecked_Access,
               On_Error => Warning);
            Stability_Parser   : constant CSP_Parser.Zero_One_Predicate_Parser :=
              (Predicate => U(Main_Namespace & "stability"),
               Child_Parser => Float_Parser'Unchecked_Access,
               On_Error => Warning);
            Preference_Parser  : constant CSP_Parser.Zero_One_Predicate_Parser :=
              (Predicate => U(Main_Namespace & "preference"),
               Child_Parser => Float_Parser'Unchecked_Access,
               On_Error => Warning);
         begin
            Info.Completeness := Parse(Context, Completeness_Parser, Model, Node);
            Info.Stability    := Parse(Context, Stability_Parser   , Model, Node);
            Info.Preference   := Parse(Context, Preference_Parser  , Model, Node);
         end;
         declare
            Parsers: Boiler.Global.Global_Parsers renames
              Boiler.Global.Get_Parsers(Boiler_Context_Type(Context).Global.all).all;
            use Boiler.Global;
         begin
            case Parser.Script_Kind is
               when Transformer =>
                  declare
                     package My_Enum_Parser is new One_Predicate(Transformer_Kind_Parser.Enum_Parser_Node);
                     Transformer_Kind_P: constant My_Enum_Parser.One_Predicate_Parser :=
                       (Predicate => U(Main_Namespace & "transformerKind"),
                        Child_Parser => Parsers.Transformer_Kind'Access,
                        On_Error => Warning);
                  begin
                     Info.Transformer_Kind := My_Enum_Parser.Parse(Context, Transformer_Kind_P, Model, Node);
                  end;
               when Validator =>
                  declare
                     package My_Enum_Parser is new One_Predicate(Validator_Kind_Parser.Enum_Parser_Node);
                     Validator_Kind_P: constant My_Enum_Parser.One_Predicate_Parser :=
                       (Predicate => U(Main_Namespace & "validatorKind"),
                        Child_Parser => Parsers.Validator_Kind'Access,
                        On_Error => Warning);
                  begin
                     Info.Validator_Kind := My_Enum_Parser.Parse(Context, Validator_Kind_P, Model, Node);
                  end;
            end case;
         end;
      end return;
   end;

   overriding function Parse (Context: Parser_Context_Type'Class;
                              Parser: Command_Script_Info_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Script_Info_Class is
   begin
      Check_Node_Class (Boiler_Context_Type(Context), Model, Node, From_String(Context.World.all, Main_Namespace & "Command"));
      declare
         Base_Parser: constant Base_Script_Info_Parser := (Script_Kind => Parser.Script_Kind,
                                                           others => <>);
         Base_Info: constant Script_Info := Parse(Context, Base_Parser, Model, Node);
      begin
         -- FIXME: Specify correct Invocation
         return Info: constant Script_Info_Class :=
           Command_Script_Info'(Base_Info with
                                Script_Kind => Parser.Script_Kind,
                                Invocation => Command,
                                others => <>)
         do
            null; -- TODO
         end return;
      end;
   end;

   overriding function Parse (Context: Parser_Context_Type'Class;
                              Parser: Web_Service_Script_Info_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Script_Info_Class is
   begin
      Check_Node_Class (Boiler_Context_Type(Context), Model, Node, From_String(Context.World.all, Main_Namespace & "WebService"));
      declare
         Base_Parser: constant Base_Script_Info_Parser := (Script_Kind => Parser.Script_Kind,
                                                           others => <>);
         Base_Info: constant Script_Info := Parse(Context, Base_Parser, Model, Node);
      begin
         -- FIXME: Specify correct Invocation
         return Info: constant Script_Info_Class :=
           Web_Service_Script_Info'(Base_Info with others => <>)
         do
            null; -- TODO
         end return;
      end;
   end;

   function Parse (Context: Parser_Context_Type'Class;
                   Parser: Script_Info_Parser;
                   Model: Model_Type_Without_Finalize'Class;
                   Node: Node_Type_Without_Finalize'Class)
                   return Script_Info_Class
   is
      package Script_Choice is new Choice(Script_Info_Node);
      Command_Parser: aliased constant Command_Script_Info_Parser :=
        (Script_Kind => Parser.Script_Kind, others => <>);
      Web_Service_Parser: aliased constant Web_Service_Script_Info_Parser :=
        (Script_Kind => Parser.Script_Kind, others => <>);
      List: aliased Script_Choice.Choices_Array := (Command_Parser'Unchecked_Access, Web_Service_Parser'Unchecked_Access);
      Real_Parser: constant Script_Choice.Choice_Parser := (Choices => List'Unchecked_Access, others => <>);
   begin
      return Script_Choice.Parse(Context, Real_Parser, Model, Node);
   end;

end Boiler.RDF_Format.Resource.Parser;
