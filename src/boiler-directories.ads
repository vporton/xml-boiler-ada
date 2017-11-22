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

private with Ada.Finalization;
private with Ada.Strings.Unbounded;

package Boiler.Directories is

   -- Influenced by XMLBOILER_PATH environment variable
   type Directories_Config_Type is limited private;

   -- Get "data" dir (usually share/)
   function Get_Data_Dir (Config: Directories_Config_Type) return String;

private

   type Directories_Config_Type is new Ada.Finalization.Limited_Controlled with
      record
         Root: Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Initialize (Object: in out Directories_Config_Type);

end Boiler.Directories;
