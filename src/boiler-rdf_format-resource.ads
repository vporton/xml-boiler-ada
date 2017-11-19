with Ada.Containers.Indefinite_Holders;

package Boiler.RDF_Format.Resource is

   type Transformer_Kind_Enum is (Entire, Sequential, Up_Down, Down_Up);

   type Validator_Kind_Enum is (Entire, Parts);

   package String_Holder is new Ada.Containers.Indefinite_Holders(String);

   type Script_Info is
      record
         Language: URI_Type;
         Min_Version, Max_Version: String_Holder.Holder;
         Script_URL:     String_Holder.Holder;
         Command_String: String_Holder.Holder;
         OK_Result: String_Holder.Holder;
         Completeness, Stability, Preference: Long_Float; -- or better just Float?
         Transformer_Kind: Transformer_Kind_Enum; -- unused when validation
         Validator_Kind  : Validator_Kind_Enum;   -- unused when transforming
      end record;

end Boiler.RDF_Format.Resource;
