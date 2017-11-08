with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Model; use RDF.Redland.Model;
with Boiler.Global;

package Boiler.RDF_Format is

   procedure Check_Node_Class (Global: Boiler.Global.Global_State_Type;
                               World: Redland_World_Type_Without_Finalize'Class;
                               Model: Model_Type_Without_Finalize'Class;
                               Node: Node_Type_Without_Finalize'Class;
                               Class: URI_Type_Without_Finalize'Class);

end Boiler.RDF_Format;
