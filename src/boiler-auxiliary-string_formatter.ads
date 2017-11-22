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

-- Template strings like: "First phrase: {1}. Second phrase: {2}."
-- If you need { before a natural number, then double it: {{.
-- Double {{ and }} are interpreted as single { or }.

private with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;

package Boiler.Auxiliary.String_Formatter is

   -- unstable API
   package Arguments is new Ada.Containers.Indefinite_Vectors(Natural, String);

   -- unstable API
   function Format_String (Format_String: String; Args: Arguments.Vector) return String;

   type Formatter is private;

   function Fmt (Str: String) return Formatter;

   function To_String (Fmt: Formatter) return String;

   function "&" (Left: Formatter; Right: String ) return Formatter;
   function "&" (Left: Formatter; Right: Integer) return Formatter;

private

   type Formatter is
      record
         Format_String: Ada.Strings.Unbounded.Unbounded_String;
         Args: Arguments.Vector;
      end record;

end Boiler.Auxiliary.String_Formatter;
