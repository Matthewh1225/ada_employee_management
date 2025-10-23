--AvailableEmployeeList.adb
package body AvailableEmployeeList is
   
   -- Free list head 
   Employee_Free_List_Head : Employee_Index_Number := 0;
   -- Next slot
   Next_Available_Employee : Employee_Index_Number := 1;
   
   procedure Initialize is
   begin
      Employee_Free_List_Head:=0;
      Next_Available_Employee:=1;
   end Initialize;  
   
   -- Get a new employee 
   function Allocate_Employee return Employee_Index_Number is
   begin
      -- First check if any recycled slots are available
      if Employee_Free_List_Head/=0 then
         declare
            Allocated_Index:constant Employee_Index_Number:= Employee_Free_List_Head;
         begin
            Employee_Free_List_Head:=Employee_Array (Allocated_Index).Next_Free;
            Employee_Array (Allocated_Index).Next_Free:= 0;
            return Allocated_Index;
         end;
      end if;
      
      if Next_Available_Employee = Maximum_Employees then
         return 0;  
      end if;
      declare
         Allocated_Index:constant Employee_Index_Number := Next_Available_Employee;
      begin
         Next_Available_Employee := Next_Available_Employee + 1;
         return Allocated_Index;
      end;
   end Allocate_Employee;
   -- Return employee slot to free list for reuse
   procedure Free_Employee (Index:Employee_Index_Number) is
   begin
      if Index /= 0 then
         Employee_Array (Index).Next_Free := Employee_Free_List_Head;
         Employee_Free_List_Head := Index;
      end if;
   end Free_Employee;
end AvailableEmployeeList;
