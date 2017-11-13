package Boiler.RDF_Format.Resource is

   type Transformer_Kind_Enum is (Entire, Sequential, Up_Down, Down_Up);

   type Script_Info is abstract
      record
         Transforms_Kind: Transformer_Kind_Enum; -- unused when validation
         Completeness, Stability, Preference: Long_Float; -- or better just Float?
      end record;

end Boiler.RDF_Format.Resource;
