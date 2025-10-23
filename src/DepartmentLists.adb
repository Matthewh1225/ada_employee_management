--DepartmentLists.adb
with AvailableEmployeeList;
package body DepartmentLists is
   -- Initialize all department heads to empty
   procedure Initialize_Departments is
   begin
      for Department_Type in Job_Type loop
         if Department_Type /= None then
            Departments (Department_Type).Count := 0;
            Departments (Department_Type).Left_Link := 0;
            Departments (Department_Type).Right_Link := 0;
         end if;
      end loop;
   end Initialize_Departments;
   -- Insert employee in order by Age, then Name
   procedure Insert_Sorted (Employee_Index_To_Insert : Employee_Index_Number) is
      Department_Of_Employee : constant Job_Type := Employee_Array (Employee_Index_To_Insert).Job;
      Age_Of_Employee : constant Natural := Employee_Array (Employee_Index_To_Insert).Age;
      Name_Of_Employee : constant Name_String := Employee_Array (Employee_Index_To_Insert).Name;
      Name_Length_Of_Employee : constant Natural := Employee_Array (Employee_Index_To_Insert).Name_Length;
      Current_Employee_Index : Employee_Index_Number;
   begin
      Departments (Department_Of_Employee).Count := Departments (Department_Of_Employee).Count + 1;
   
      -- First employee
      if Departments (Department_Of_Employee).Right_Link = 0 then
         Employee_Array (Employee_Index_To_Insert).Left_Link := 0;
         Employee_Array (Employee_Index_To_Insert).Right_Link := 0;
         Departments (Department_Of_Employee).Right_Link := Employee_Index_To_Insert;
         Departments (Department_Of_Employee).Left_Link := Employee_Index_To_Insert;
         return;
      end if;
      
      Current_Employee_Index := Departments (Department_Of_Employee).Right_Link;

      -- Check if new employee should be inserted at front
      if Age_Of_Employee < Employee_Array (Current_Employee_Index).Age or else 
         (Age_Of_Employee = Employee_Array (Current_Employee_Index).Age and then 
          Name_Less_Than (Name_Of_Employee, Name_Length_Of_Employee, 
                         Employee_Array (Current_Employee_Index).Name, 
                         Employee_Array (Current_Employee_Index).Name_Length))
      then
         Employee_Array (Employee_Index_To_Insert).Left_Link := 0;
         Employee_Array (Employee_Index_To_Insert).Right_Link := Current_Employee_Index;
         Employee_Array (Current_Employee_Index).Left_Link := Employee_Index_To_Insert;
         Departments (Department_Of_Employee).Right_Link := Employee_Index_To_Insert;
         return;
      end if;

      -- Find correct positionin list
      loop
         -- Check if end 
         if Employee_Array (Current_Employee_Index).Right_Link = 0 then
            -- Insert 
            Employee_Array (Employee_Index_To_Insert).Left_Link := Current_Employee_Index;
            Employee_Array (Employee_Index_To_Insert).Right_Link := 0;
            Employee_Array (Current_Employee_Index).Right_Link := Employee_Index_To_Insert;
            Departments (Department_Of_Employee).Left_Link := Employee_Index_To_Insert;
            return;
         end if;

         -- Check if should insert between 
         declare
            Next_Employee_Index:constant Employee_Index_Number := Employee_Array (Current_Employee_Index).Right_Link;
            Age_Of_Next_Employee:constant Natural:= Employee_Array (Next_Employee_Index).Age;
         begin
            if Age_Of_Employee < Age_Of_Next_Employee or else 
               (Age_Of_Employee = Age_Of_Next_Employee and then 
                Name_Less_Than (Name_Of_Employee, Name_Length_Of_Employee,
                               Employee_Array (Next_Employee_Index).Name, 
                               Employee_Array (Next_Employee_Index).Name_Length))
            then
               -- Insert between 
               Employee_Array (Employee_Index_To_Insert).Left_Link:= Current_Employee_Index;
               Employee_Array (Employee_Index_To_Insert).Right_Link:= Next_Employee_Index;
               Employee_Array (Current_Employee_Index).Right_Link :=Employee_Index_To_Insert;
               Employee_Array (Next_Employee_Index).Left_Link := Employee_Index_To_Insert;
               return;
            end if;
         end;

         -- Move to next employee
         Current_Employee_Index := Employee_Array (Current_Employee_Index).Right_Link;
      end loop;
   end Insert_Sorted;

   -- Visit each employee fron front to bac
   procedure Traverse_Forward (Department : Job_Type;
      Visit : access procedure (Employee_Index_To_Visit : Employee_Index_Number)) is
      Current_Employee_Index:Employee_Index_Number:=Departments (Department).Right_Link;
   begin
      while Current_Employee_Index/=0 loop
         Visit (Current_Employee_Index);
         Current_Employee_Index:=Employee_Array (Current_Employee_Index).Right_Link;
      end loop;
   end Traverse_Forward;
   -- Remove employee from list and free employee 
   procedure Delete_Employee (Employee_Index_To_Delete : Employee_Index_Number) is
      Department_Of_Employee : constant Job_Type := Employee_Array (Employee_Index_To_Delete).Job;
      Left_Index : constant Employee_Index_Number:= Employee_Array (Employee_Index_To_Delete).Left_Link;
      Right_Index : constant Employee_Index_Number := Employee_Array (Employee_Index_To_Delete).Right_Link;
   begin
      Departments (Department_Of_Employee).Count := Departments (Department_Of_Employee).Count - 1;
      if Left_Index = 0 then
         Departments (Department_Of_Employee).Right_Link := Right_Index;
      else
         Employee_Array (Left_Index).Right_Link := Right_Index;
      end if;

      if Right_Index = 0 then
         Departments(Department_Of_Employee).Left_Link := Left_Index;
      else
         Employee_Array (Right_Index).Left_Link:=Left_Index;
      end if;
      -- Clear the employee's links
      Employee_Array (Employee_Index_To_Delete).Left_Link := 0;
      Employee_Array (Employee_Index_To_Delete).Right_Link := 0;
      -- Return employee slot to free list
      AvailableEmployeeList.Free_Employee (Employee_Index_To_Delete);
   end Delete_Employee;
end DepartmentLists;
