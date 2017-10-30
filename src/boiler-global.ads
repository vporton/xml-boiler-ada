with Boiler.Directories;
with RDF.Redland.URI; use RDF.Redland.URI;
private with Generic_Directed_Graph;

package Boiler.Global is

   type Global_State_Type is private;

   function Is_Subclass (Sub, Super: URI_Type_Without_Finalize'Class) return Boolean;

private

   package Sub_Classes_Graph is new Generic_Directed_Graph(URI_Type,
                                                           Pool   => URI_Type'Storage_Pool,
                                                           Equals => "=",
                                                           Less   => "<"); -- FIXME: Is "<" properly defined?

   type Global_State_Type is
      record
         Directories_Config: Boiler.Directories.Directories_Config_Type;
         Sub_Classes_Graph: Sub_Classes_Graph_Type;
      end record;

end Boiler.Global;
