with Ada.Finalization;
with Interfaces.C; use Interfaces.C;

package Glibc.Locale is

   Locale_Exception: exception;

   type Locale_Type is private;

   Empty_Locale: constant Locale_Type;

   type Locale_Category is mod 2**16;

   LC_CTYPE_MASK          : constant Locale_Category := 2 ** LC_CTYPE;
   LC_NUMERIC_MASK        : constant Locale_Category := 2 ** LC_NUMERIC;
   LC_TIME_MASK           : constant Locale_Category := 2 ** LC_TIME;
   LC_COLLATE_MASK        : constant Locale_Category := 2 ** LC_COLLATE;
   LC_MONETARY_MASK       : constant Locale_Category := 2 ** LC_MONETARY;
   LC_MESSAGES_MASK       : constant Locale_Category := 2 ** LC_MESSAGES;
   LC_PAPER_MASK          : constant Locale_Category := 2 ** LC_PAPER;
   LC_NAME_MASK           : constant Locale_Category := 2 ** LC_NAME;
   LC_ADDRESS_MASK        : constant Locale_Category := 2 ** LC_ADDRESS;
   LC_TELEPHONE_MASK      : constant Locale_Category := 2 ** LC_TELEPHONE;
   LC_MEASUREMENT_MASK    : constant Locale_Category := 2 ** LC_MEASUREMENT;
   LC_IDENTIFICATION_MASK : constant Locale_Category := 2 ** LC_IDENTIFICATION;
   LC_ALL_MASK            : constant Locale_Category :=
     LC_CTYPE_MASK or
     LC_NUMERIC_MASK or
     LC_TIME_MASK or
     LC_COLLATE_MASK or
     LC_MONETARY_MASK or
     LC_MESSAGES_MASK or
     LC_PAPER_MASK or
     LC_NAME_MASK or
     LC_ADDRESS_MASK or
     LC_TELEPHONE_MASK or
     LC_MEASUREMENT_MASK or
     LC_IDENTIFICATION_MASK;


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

   Empty_Locale: constant Locale_Type :=
     (Ada.Finalization.Controlled with Handle => null);

   type Local_Locale (Locale: access Locale_Type) is new Ada.Finalization.Limited_Controlled with
      record
         Old_Locale: locale_t;
      end record;

   overriding procedure Initialize (Object: in out Local_Locale);

   overriding procedure Finalize (Object: in out Local_Locale);

end Glibc.Locale;
