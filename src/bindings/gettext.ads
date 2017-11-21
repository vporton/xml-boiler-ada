package Gettext is

   procedure Text_Domain (Domain : String := "");

   procedure Bind_Text_Domain (Domain : String; Dirname : String);

   function Gettext (Msg : String) return String;

--     function Gettext (Locale: access Glibc.Locale.Locale_Type; Msg : String) return String;

end Gettext;
