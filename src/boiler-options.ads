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

with Ada.Containers.Ordered_Sets;

package Boiler.Options is

   type Worklow_Kind is (Transformation, Validation);

   type Recursive_Download_Type is (None, Depth_First, Breadth_First);

   type Validation_Order_Type is (Depth_First, Breadth_First);

   type Not_In_Target_Namespace_Type is (Ignore, Remove, Error);

   type Recursive_Retrieval_Priority_Order_Element is (Sources, Targets);
   package Recursive_Retrieval_Priority is
     new Ada.Containers.Ordered_Sets(Recursive_Retrieval_Priority_Order_Element);

   type Recursive_Download_Options (Recursive_Download: Recursive_Download_Type) is
      record
         case Recursive_Download is
            when None =>
               null;
            when others =>
               Retrieval_Priority: Recursive_Retrieval_Priority.Set;
         end case;
      end record;

   -- In this version the same options are applied to all elements of the
   -- workflow, but in future we may increase "granularity" to have different
   -- options for different elements.
   type Automatic_Workflow_Element_Options (Kind: Worklow_Kind;
                                            Recursive_Download: Recursive_Download_Type) is
      record
         Recursive_Options: Recursive_Download_Options(Recursive_Download);
         case Kind is
            when Transformation =>
               Not_In_Target_Namespace: Not_In_Target_Namespace_Type;
            when Validation =>
               Validation_Order: Validation_Order_Type;
               Unknown_Namespaces_Is_Invalid: Boolean;
         end case;
      end record;

end Boiler.Options;
