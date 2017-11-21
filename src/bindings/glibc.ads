package Glibc is

   --  Category values
   LC_ALL      : constant Integer := 0;
   LC_COLLATE  : constant Integer := 1;
   LC_CTYPE    : constant Integer := 2;
   LC_MONETARY : constant Integer := 3;
   LC_NUMERIC  : constant Integer := 4;
   LC_TIME     : constant Integer := 5;
   LC_MESSAGES : constant Integer := 6;
   u_LC_LAST   : constant Integer := 7;  --  Marks end

   procedure Setlocale (Category : Integer := LC_ALL; Locale : String := "");

end Glibc;
