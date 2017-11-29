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

with RDF.Redland.Statement; use RDF.Redland.Statement;
with RDF.Redland.Node_Iterator; use RDF.Redland.Node_Iterator;
with RDF.Redland.Stream; use RDF.Redland.Stream;
with Gettext_Lib;
with Boiler.Auxiliary.String_Formatter; use Boiler.Auxiliary.String_Formatter;

package body Boiler.RDF_Recursive_Descent is

   function Gettext (Msg : String) return String renames Gettext_Lib.Gettext;

   function Fmt (Str: String) return Boiler.Auxiliary.String_Formatter.Formatter
                 renames Boiler.Auxiliary.String_Formatter.Fmt;

   procedure Raise_Warning (Context: Parser_Context_Type'Class;
                            On_Error: Error_Enum;
                            Message: access function return String) is
   begin
      case On_Error is
         when Ignore =>
            raise Parse_Error;
         when Warning =>
            declare
               Msg: constant String := Message.all;
            begin
               Log(Context.Logger.all, Msg, Warning);
               raise Parse_Error with Msg;
            end;
         when Fatal =>
            declare
               Msg: constant String := Message.all;
            begin
               Log(Context.Logger.all, Msg, Fatal);
               raise Fatal_Parse_Error with Msg;
            end;
      end case;
   end;

   procedure Raise_Warning (Context: Parser_Context_Type'Class; On_Error: Error_Enum; Message: String) is
      function Handler return String is (Message);
   begin
      Raise_Warning(Context, On_Error, Handler'Access);
   end;

   package body One_Predicate is

      function Parse (Context: Parser_Context_Type'Class;
                      Parser: One_Predicate_Parser;
                      Model: Model_Type_Without_Finalize'Class;
                      Node: Node_Type_Without_Finalize'Class)
                      return Child_Type is
         Iterator: Node_Iterator_Type :=
           Get_Targets(Model, Node, From_URI(Context.World.all, Parser.Predicate));
         Child_Nodes: constant Node_Array := To_Array(Iterator);
         use Predicate_Parser;
      begin
         if Child_Nodes'Length /= 1 then
            declare
               function Message return String is
               begin
                  return To_String(Fmt(Gettext("Exactly one predicate <{1}> required for node {2}.")) &
                                     As_String(Parser.Predicate) &
                                     Format_As_String(Node));
               end;
            begin
               Raise_Warning (Context, Parser.On_Error, Message'Access);
            end;
         end if;
         declare
            Child_Node: constant Node_Type := Child_Nodes(Child_Nodes'First);
            use Node_Parser;
         begin
            return Parse(Context, Parser.Child_Parser.all, Model, Child_Node);
         end;
      end;

   end One_Predicate;

   package body Zero_One_Predicate is

      function Parse (Context: Parser_Context_Type'Class;
                      Parser: Zero_One_Predicate_Parser;
                      Model: Model_Type_Without_Finalize'Class;
                      Node: Node_Type_Without_Finalize'Class)
                      return Holder_Type is
         Iterator: Node_Iterator_Type :=
           Get_Targets(Model, Node, From_URI(Context.World.all, Parser.Predicate));
         Child_Nodes: constant Node_Array := To_Array(Iterator);
         use Predicate_Parser;
      begin
         if Child_Nodes'Length /= 1 then
            return Default_Value;
         end if;
         declare
            Child_Node: constant Node_Type := Child_Nodes(Child_Nodes'First);
            use Node_Parser;
         begin
            return To_Holder(Parse(Context,
                             Parser.Child_Parser.all,
                             Model,
                             Child_Node));
         exception
            when Parse_Error =>
               return Default_Value;
         end;
      end;

   end Zero_One_Predicate;

   package body Zero_Or_More_Predicate is

      function Parse (Context: Parser_Context_Type'Class;
                      Parser: Zero_Or_More_Predicate_Parser;
                      Model: Model_Type_Without_Finalize'Class;
                      Node: Node_Type_Without_Finalize'Class)
                      return Vectors.Vector is
         Iter: Node_Iterator_Type :=
           Get_Targets (Model, Node, From_URI(Context.World.all, Parser.Predicate));
         use Vectors, Node_Parser;
      begin
         return Result: Vectors.Vector do
            while not Is_End(Iter) loop
               begin
                  Append(Result, Child_Converter(Parse(Context,
                         Parser.Child_Parser.all,
                         Model,
                         Get_Node(Iter))));
               exception
                  when Parse_Error =>
                     null; -- ignore non-parsed childs
               end;
               Next(Iter);
            end loop;
         end return;
      end;

   end Zero_Or_More_Predicate;

   package body Choice is

      -- FIXME
      function Parse (Context: Parser_Context_Type'Class;
                      Parser: Choice_Parser;
                      Model: Model_Type_Without_Finalize'Class;
                      Node: Node_Type_Without_Finalize'Class)
                      return Base_Type is
         use Node_Parser;
      begin
         for C of Parser.Choices.all loop
            begin
               return Parse(Context, C.all, Model, Node);
            exception
               when Parse_Error =>
                  null; -- do next loop iteration
            end;
         end loop;
         declare
            function Message return String is
            begin
               return To_String(Fmt("On node {1} there is no known object.") &
                                  Format_As_String(Node));
            end;
         begin
            Raise_Warning (Context, Parser.On_Error, Message'Access);
         end;
      end Parse;

   end Choice;

   procedure Check_Node_Class (Is_Subclass: access function (Sub, Super: URI_Type_Without_Finalize'Class) return Boolean;
                               Context: Parser_Context_Type'Class;
                               Model: Model_Type_Without_Finalize'Class;
                               Node: Node_Type_Without_Finalize'Class;
                               Class: URI_Type_Without_Finalize'Class;
                               On_Error: Error_Enum := Ignore) is
      Iterator: Node_Iterator_Type :=
        Get_Targets(Model, Node, From_URI_String(Context.World.all, "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"));
   begin
      while not Is_End(Iterator) loop
         declare
            Target: constant Node_Type_Without_Finalize := Get_Node(Iterator);
         begin
            if Is_Resource(Target) and then Is_Subclass(Get_URI(Target), Class) then
               return;
            end if;
         end;
         Next(Iterator);
      end loop;
      declare
         function Message return String is
         begin
            return To_String(Fmt("Node {1} class is not a subclass of <{2}>") &
                               Format_As_String(Node) &
                               As_String(Class));
         end;
      begin
         Raise_Warning (Context, On_Error, Message'Access);
      end;
   end;

   package body Class_Forest is

      function Parse (Context: Parser_Context_Type'Class;
                      Parser: Class_Forest_Parser;
                      Model: Model_Type_Without_Finalize'Class)
                      return Vectors.Vector is
         Pattern: constant Statement_Type :=
           From_Nodes(Context.World.all,
                      Node_Type_Without_Finalize'(From_Handle(null)),
                      From_URI_String(Context.World.all, "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                      Node_Type_Without_Finalize'(From_Handle(null)));
         Stream: constant Stream_Type := Find(Model, Pattern);
      begin
         return V: Vectors.Vector do
            while not Is_End(Stream) loop -- slow algorithm
               declare
                  -- TODO: Unfortunate clash of two Get_Object functions. Change RDF.Redland?
                  Statement: constant Statement_Type_Without_Finalize := Get_Object(Stream);
                  Node: constant Node_Type_Without_Finalize := Get_Object(Statement);
                  Class_Node: constant Node_Type_Without_Finalize := Get_Subject(Statement);
               begin
                  if Is_Resource(Class_Node) then
                     declare
                        Class: constant URI_Type_Without_Finalize := Get_URI(Class_Node);
                        use Node_Parser;
                     begin
                        Check_Node_Class(Parser.Is_Subclass, Context, Model, Node, Class);
                        V.Append(Parse(Context, Parser.Parser.all, Model, Node));
                     exception
                        when Parse_Error =>
                           null;
                     end;
                  end if;
               end;
               Next(Stream);
            end loop;
         end return;
      end;

   end Class_Forest;

end Boiler.RDF_Recursive_Descent;
