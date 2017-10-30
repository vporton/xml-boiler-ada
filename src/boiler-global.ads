with Boiler.Directories;
with RDF.Redland.URI; use RDF.Redland.URI;
private with Ada.Containers.Ordered_Maps;
private with Generic_Directed_Graph;
private with Generic_Address_Order;

package Boiler.Global is

   type Global_State_Type is limited private;

   function Is_Subclass (Global: Global_State_Type; Sub, Super: URI_Type_Without_Finalize'Class) return Boolean;

private

   type Null_Record is null record;
   type Null_Access is access Null_Record;
   package Null_Order is new Generic_Address_Order(Null_Record);

   package Sub_Classes_Graph is new Generic_Directed_Graph(Null_Record,
                                                           Pool  => Null_Access'Storage_Pool, -- TODO: Use allocation-only simple pool
                                                           Equal => Null_Order.Equal,
                                                           Less  => Null_Order.Less); -- FIXME: Is "<" properly defined?

   use Sub_Classes_Graph;
   package URI_To_Access is new Ada.Containers.Ordered_Maps(URI_Type, Sub_Classes_Graph.Node);

   -- FIXME: Should be made public?
   type Global_State_Type is
      record
         Directories_Config: Boiler.Directories.Directories_Config_Type;
         URIs_Map: URI_To_Access.Map;
      end record;

end Boiler.Global;
