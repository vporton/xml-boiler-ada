with Ada.Finalization;
with Interfaces.C; use Interfaces.C;

package Glibc.Locale is

   type Locale_Type is private;

   function New_Locale (Category_Mask: int; Locale: String) return Locale_Type;

   function Use_Locale (Locale: Locale_Type) return Locale_Type;

   type Local_Locale (Locale: access Locale_Type) is new Ada.Finalization.Limited_Controlled with private;

private

   type Dummy_Record is null record
     with Convention=>C;

   type locale_t is access all Dummy_Record;

   type Locale_Type is new Ada.Finalization.Controlled with
      record
         Handle: locale_t;
      end record;

   overriding procedure Finalize (Object: in out Locale_Type);

   overriding procedure Adjust (Object: in out Locale_Type);

   type Local_Locale (Locale: access Locale_Type) is new Ada.Finalization.Limited_Controlled with
      record
         Old_Locale: locale_t;
      end record;

   overriding procedure Initialize (Object: in out Local_Locale);

   overriding procedure Finalize (Object: in out Local_Locale);

end Glibc.Locale;
