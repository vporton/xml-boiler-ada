package body Glibc.Locale is

   function New_Locale (Category_Mask: int; Locale: String) return Locale_Type is
      function Internal (Category_Mask: int; Locale: char_array; Old: locale_t) return locale_t
        with Import, Convention=>C, External_Name=>"newlocale";
      Handle: constant locale_t := Internal(Category_Mask, To_C(Locale), null);
   begin
      return (Ada.Finalization.Controlled with Handle => Handle);
   end New_Locale;

   function Use_Locale (Locale: Locale_Type) return Locale_Type is
      function Internal (Locale: locale_t) return locale_t
        with Import, Convention=>C, External_Name=>"uselocale";
      Handle: constant locale_t := Internal(Locale.Handle);
   begin
      return (Ada.Finalization.Controlled with Handle => Handle);
   end Use_Locale;

   overriding procedure Finalize (Object: in out Locale_Type) is
      procedure Internal (Locale: locale_t)
        with Import, Convention=>C, External_Name=>"freelocale";
   begin
      Internal(Object.Handle);
      Object.Handle := null;
   end Finalize;

   overriding procedure Adjust (Object: in out Locale_Type) is
      function Internal (Locale: locale_t) return locale_t
        with Import, Convention=>C, External_Name=>"duplocale";
   begin
      Object.Handle := Internal(Object.Handle);
   end Adjust;

end Glibc.Locale;
