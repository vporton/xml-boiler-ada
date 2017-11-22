package Gettext_Lib is

   procedure Text_Domain (Domain : String := "");

   procedure Bind_Text_Domain (Domain : String; Dirname : String);

   function Gettext (Msg : String) return String;

   function Gettext_Noop (Msg : String) return String is (Msg);

--     function Gettext (Locale: access Glibc.Locale.Locale_Type; Msg : String) return String;

end Gettext_Lib;
