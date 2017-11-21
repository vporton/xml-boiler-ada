package body Glibc.Locale is

   function uselocale (Locale: locale_t) return locale_t
     with Import, Convention=>C, External_Name=>"uselocale";

   function New_Locale (Category_Mask: int; Locale: String) return Locale_Type is
      function Internal (Category_Mask: int; Locale: char_array; Old: locale_t) return locale_t
        with Import, Convention=>C, External_Name=>"newlocale";
      Handle: constant locale_t := Internal(Category_Mask, To_C(Locale), null);
   begin
      if Handle = null then
         raise Locale_Exception with "Cannot create locale.";
      end if;
      return (Ada.Finalization.Controlled with Handle => Handle);
   end New_Locale;

   function Use_Locale (Locale: Locale_Type) return Locale_Type is
      Handle: constant locale_t := uselocale(Locale.Handle);
   begin
      if Handle = null then
         raise Locale_Exception with "Cannot use locale.";
      end if;
      return (Ada.Finalization.Controlled with Handle => Handle);
   end Use_Locale;

   overriding procedure Finalize (Object: in out Locale_Type) is
      procedure Internal (Locale: locale_t)
        with Import, Convention=>C, External_Name=>"freelocale";
   begin
      if Object.Handle = null then
         return;
      end if;
      Internal(Object.Handle);
      Object.Handle := null;
   end Finalize;

   overriding procedure Adjust (Object: in out Locale_Type) is
      function Internal (Locale: locale_t) return locale_t
        with Import, Convention=>C, External_Name=>"duplocale";
   begin
      if Object.Handle = null then
         return;
      end if;
      Object.Handle := Internal(Object.Handle);
   end Adjust;

   overriding procedure Initialize (Object: in out Local_Locale) is
   begin
      Object.Old_Locale := uselocale(Object.Locale.all.Handle);
   end;

   overriding procedure Finalize (Object: in out Local_Locale) is
      Dummy: constant locale_t := uselocale(Object.Old_Locale);
      pragma Unreferenced(Dummy);
   begin
      null;
   end;

end Glibc.Locale;
