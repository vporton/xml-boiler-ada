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
