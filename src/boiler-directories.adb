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

with Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Boiler.Config;

package body Boiler.Directories is

   procedure Initialize (Object: in out Directories_Config_Type) is
   begin
      Object.Root := To_Unbounded_String(Ada.Environment_Variables.Value("XMLBOILER_PATH"));
   exception
      when Constraint_Error =>
         null;
   end;

   function Get_Data_Dir (Config: Directories_Config_Type) return String is
   begin
      if Config.Root = Null_Unbounded_String then
         return Boiler.Config.Data_Dir;
      else
         return To_String(Config.Root) & "/share";
      end if;
   end;

end Boiler.Directories;
