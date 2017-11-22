--  Copyright 2017 Victor Porton
--
--  This file is part of XML Boiler.
--
--  XML Boiler is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  XML Boiler is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with XML Boiler.  If not, see <http://www.gnu.org/licenses/>.

-- This file is released not only under GPLv3 but also under Apache License 2.0

package body Boiler.Auxiliary.String_Formatter is

   use Arguments, Ada.Strings.Unbounded;

   -- TODO: Unit test the below code

   -- return -1 if it is not a placeholder, otherwise return placeholder number.
   function Extract_Placeholder (Format_String: String; I: in out Positive) return Integer is
      New_I: Positive := I;
      Num_Str: Unbounded_String;
   begin
      if Format_String(I) /= '{' then -- not necessary but extra check won't harm
         return -1;
      end if;
      New_I := New_I + 1;
      Main_Loop: while New_I <= Format_String'Last loop
         case Format_String(New_I) is
            when '0'..'9' =>
               Num_Str := Num_Str & Format_String(New_I);
            when '}' =>
               New_I := New_I + 1;
               exit Main_Loop;
            when others =>
               return -1;
         end case;
         New_I := New_I + 1;
      end loop Main_Loop;
      if Num_Str = "" then -- impossible when called from Format_String but an extra check won't harm
         return -1;
      end if;
      I := New_I;
      return Integer'Value(To_String(Num_Str));
   end;

   function Format_String (Format_String: String; Args: Arguments.Vector) return String is
      Result: Unbounded_String;
      I: Positive := Format_String'First;
   begin
      while I <= Format_String'Last loop
         -- special processing of last character, without lookup of the next character
         if I = Format_String'Last then
            return To_String(Result & Format_String(I));
         end if;
         case Format_String(I) is
            when '{' =>
               case Format_String(I+1) is
                  when '{' =>
                     I := I + 1; -- skip second {
                  when '0' .. '9' =>
                     declare
                        -- The function has side effect on I
                        Placeholder: constant Integer := Extract_Placeholder(Format_String, I);
                     begin
                        if Placeholder /= -1 then
                           Result := Result & Args(Placeholder);
                           goto End_Of_Loop; -- var I was already updated
                        end if;
                        -- exit all cases
                     end;
                  when others =>
                     null;
               end case;
            when '}' =>
               case Format_String(I+1) is
                  when '}' =>
                     I := I + 1; -- skip second }
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         Result := Result & Format_String(I);
         I := I + 1;
         <<End_Of_Loop>>
      end loop;
      return To_String(Result); -- necessary for empty Format_String
   end Format_String;

   function Fmt (Str: String) return Formatter is
   begin
      return (Format_String => To_Unbounded_String(Str), Args => Arguments.Empty_Vector);
   end Fmt;

   function To_String (Fmt: Formatter) return String is
   begin
      return Format_String (To_String(Fmt.Format_String), Fmt.Args);
   end To_String;

   function "&" (Left: Formatter; Right: String) return Formatter is
   begin
      return (Format_String => Left.Format_String, Args => Left.Args & Right);
   end "&";

   function "&" (Left: Formatter; Right: Integer) return Formatter is
      Str: String := Integer'Image(Right);
   begin
      if Str(Str'First) = ' ' then
         Str := Str(Str'First + 1 .. Str'Last);
      end if;
      return Left & Str;
   end "&";

end Boiler.Auxiliary.String_Formatter;
