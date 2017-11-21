with Interfaces.C; use Interfaces.C;

package body Glibc is

   procedure Setlocale (Category : Integer := LC_ALL; Locale : String := "") is
      procedure Internal (Category : int; Locale : char_array);
      pragma Import (C, Internal, "setlocale");
   begin
      Internal (int (Category), To_C(Locale));
   end Setlocale;


end Glibc;
