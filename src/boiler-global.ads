with Ada.Finalization;
with Boiler.Directories; use Boiler.Directories;
with RDF.Redland.URI; use RDF.Redland.URI;
private with Ada.Containers.Ordered_Maps;
private with Generic_Directed_Graph;
private with Generic_Address_Order;
--  private with RDF.Redland.World;

package Boiler.Global is

   type Global_State_Type is limited private;

   function Get_Directories (Global: Global_State_Type) return access constant Directories_Config_Type;

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

   type Global_State_Type is new Ada.Finalization.Limited_Controlled with
      record
         Directories_Config: aliased Boiler.Directories.Directories_Config_Type;
--           Initialization_Redland_World: Redland_World_Type;
         URIs_Map: URI_To_Access.Map;
      end record;

   overriding procedure Initialize (Object: in out Global_State_Type);

end Boiler.Global;
