with Types; use Types;
package AvailableEmployeeList is
   procedure Initialize;
   -- Get a new emploee slot 
   function Allocate_Employee return Employee_Index_Number;
   -- Return an employee 
   procedure Free_Employee (Index : Employee_Index_Number);
end AvailableEmployeeList;
