package body Boiler.Global is

   function Is_Subclass (Global: Global_State_Type; Sub, Super: URI_Type_Without_Finalize'Class) return Boolean is
      Sub2: constant URI_Type := Copy(Sub);
      Super2: constant URI_Type := Copy(Super);
   begin
      return Is_Ancestor(Node(Global.URIs_Map.Element(Sub2)),
                         Node(Global.URIs_Map.Element(Super2)));
   end;

end Boiler.Global;
