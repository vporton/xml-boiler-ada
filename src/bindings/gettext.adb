with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gettext is

   procedure Text_Domain (Domain : String := "") is
      procedure Internal (Domain : char_array);
      pragma Import (C, Internal, "textdomain");
   begin
      Internal (To_C(Domain));
   end Text_Domain;

   procedure Bind_Text_Domain (Domain : String; Dirname : String) is
      procedure Internal (Domain, Dirname : char_array);
      pragma Import (C, Internal, "bindtextdomain");
   begin
      Internal (To_C(Domain), To_C(Dirname));
   end Bind_Text_Domain;

   function Gettext (Msg : String) return String is
      function Internal (Msg : char_array) return chars_ptr;
      pragma Import (C, Internal, "gettext");
   begin
      return Value (Internal (To_C(Msg)));
   end Gettext;

end Gettext;
