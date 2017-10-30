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
