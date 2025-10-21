-- AvailableEmployeeList.adb
-- Implementation of free list management for employee storage available list
-- Vehicles are now heap-allocated using 'new' (inheritance model)

package body AvailableEmployeeList is

   -- Free list head 
   Employee_Free_List_Head : Employee_Index_Number := 0;
   
   -- Next available spot in list
   Next_Available_Employee : Employee_Index_Number := 1;

   procedure Initialize is
   begin
      Employee_Free_List_Head := 0;
      Next_Available_Employee := 1;
   end Initialize;

   -- Allocate a new employee
   function Allocate_Employee return Employee_Index_Number is
      Allocated_Index : Employee_Index_Number;
   begin
      if Employee_Free_List_Head /= 0 then
         -- Reuse from free list not full yet
         Allocated_Index := Employee_Free_List_Head;
         Employee_Free_List_Head := Employee_Array (Allocated_Index).Next_Free;
         Employee_Array (Allocated_Index).Next_Free := 0;
      else
         if Next_Available_Employee > Maximum_Employees then
            return 0;  -- Available list empty
         end if;
         Allocated_Index := Next_Available_Employee;
         Next_Available_Employee := Next_Available_Employee + 1;
      end if;
      return Allocated_Index;
   end Allocate_Employee;

   -- Free an employee back to available list
   procedure Free_Employee (Index : Employee_Index_Number) is
   begin
      if Index /= 0 then
         Employee_Array (Index).Next_Free := Employee_Free_List_Head;
         Employee_Free_List_Head := Index;
      end if;
   end Free_Employee;

end AvailableEmployeeList;
