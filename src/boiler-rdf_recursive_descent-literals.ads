package Boiler.RDF_Recursive_Descent.Literals is

   package String_Node is new Base_Node(String);

   type String_Literal_Parser is new String_Node.Base_Node_Parser with null record;

   overriding function Parse (World: Redland_World_Type_Without_Finalize'Class;
                              Parser: String_Literal_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return String;

   package Integer_Node is new Base_Node(Integer);

   type Integer_Literal_Parser is new Integer_Node.Base_Node_Parser with null record;

   overriding function Parse (World: Redland_World_Type_Without_Finalize'Class;
                              Parser: Integer_Literal_Parser;
                              Model: Model_Type_Without_Finalize'Class;
                              Node: Node_Type_Without_Finalize'Class)
                              return Integer;

end Boiler.RDF_Recursive_Descent.Literals;
