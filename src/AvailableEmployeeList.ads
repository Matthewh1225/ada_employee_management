-- AvailableEmployeeList.ads
-- Free list management for employee storage available list
-- Vehicles are now heap-allocated using 'new' (inheritance model)

with Types; use Types;

package AvailableEmployeeList is

   procedure Initialize;
   --  new employee from the available list
   function Allocate_Employee return Employee_Index_Number;
   -- Free employee back to available list
   procedure Free_Employee (Index : Employee_Index_Number);

end AvailableEmployeeList;
