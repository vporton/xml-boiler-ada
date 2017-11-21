package Glibc is

   --  Category values
   LC_CTYPE          : constant := 0;
   LC_NUMERIC        : constant := 1;
   LC_TIME           : constant := 2;
   LC_COLLATE        : constant := 3;
   LC_MONETARY       : constant := 4;
   LC_MESSAGES       : constant := 5;
   LC_ALL            : constant := 6;
   LC_PAPER          : constant := 7;
   LC_NAME           : constant := 8;
   LC_ADDRESS        : constant := 9;
   LC_TELEPHONE      : constant := 10;
   LC_MEASUREMENT    : constant := 11;
   LC_IDENTIFICATION : constant := 12;

   procedure Setlocale (Category : Integer := LC_ALL; Locale : String := "");

end Glibc;
