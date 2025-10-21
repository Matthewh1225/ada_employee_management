-- DepartmentLists.adb
-- Implementation of doubly-linked list operations for department management

with Enums; use Enums;
with StringConversions; use StringConversions;

package body DepartmentLists is

   procedure Initialize_Departments is
   begin
      -- Set up head for each department
      for Department_Type in Job_Type loop
         if Department_Type /= None then
            Departments (Department_Type).Job := Department_Type;
            Departments (Department_Type).Count := 0;
            Departments (Department_Type).Left_Link := 0;
            Departments (Department_Type).Right_Link := 0;
         end if;
      end loop;
   end Initialize_Departments;

   -- First by Age then by Name  if age areequal
   procedure Insert_Sorted (Employee_Index_To_Insert : Employee_Index_Number) is
      Department_Of_Employee : constant Job_Type := Employee_Array (Employee_Index_To_Insert).Job;
      Age_Of_Employee        : constant Natural := Employee_Array (Employee_Index_To_Insert).Age;
      Name_Of_Employee       : constant Name_String := Employee_Array (Employee_Index_To_Insert).Name;
      Name_Length_Of_Employee: constant Natural := Employee_Array (Employee_Index_To_Insert).Name_Length;
      
      Current_Employee_Index : Employee_Index_Number;
   begin
      -- Increment count
      Departments (Department_Of_Employee).Count := Departments (Department_Of_Employee).Count + 1;

      -- if Empty list insert as first spot
      if Departments (Department_Of_Employee).Right_Link = 0 then
         Employee_Array (Employee_Index_To_Insert).Left_Link := 0;
         Employee_Array (Employee_Index_To_Insert).Right_Link := 0;
         Departments (Department_Of_Employee).Right_Link := Employee_Index_To_Insert;
         Departments (Department_Of_Employee).Left_Link := Employee_Index_To_Insert;
         return;
      end if;

      -- Start at beginning of list poitedf to by head node
      Current_Employee_Index := Departments (Department_Of_Employee).Right_Link;

      -- Insert at beginning
      if Age_Of_Employee < Employee_Array (Current_Employee_Index).Age or else (Age_Of_Employee = Employee_Array (Current_Employee_Index).Age and then 
      Name_Less_Than (Name_Of_Employee, Name_Length_Of_Employee, Employee_Array (Current_Employee_Index).Name,Employee_Array (Current_Employee_Index).Name_Length))
      then
         Employee_Array (Employee_Index_To_Insert).Left_Link := 0;
         Employee_Array (Employee_Index_To_Insert).Right_Link := Current_Employee_Index;
         Employee_Array (Current_Employee_Index).Left_Link := Employee_Index_To_Insert;
         Departments (Department_Of_Employee).Right_Link := Employee_Index_To_Insert;
         return;
      end if;

      -- insert in middle or insert at end
      loop
         if Employee_Array (Current_Employee_Index).Right_Link = 0 then
            -- Current is last element - insert at end
            Employee_Array (Employee_Index_To_Insert).Left_Link := Current_Employee_Index;
            Employee_Array (Employee_Index_To_Insert).Right_Link := 0;
            Employee_Array (Current_Employee_Index).Right_Link := Employee_Index_To_Insert;
            Departments (Department_Of_Employee).Left_Link := Employee_Index_To_Insert;
            return;
         end if;

         declare
            Next_Employee_Index : constant Employee_Index_Number := Employee_Array (Current_Employee_Index).Right_Link;
            Age_Of_Next_Employee : constant Natural := Employee_Array (Next_Employee_Index).Age;
         begin
            -- Compare with next element to see if we insert here
            if Age_Of_Employee < Age_Of_Next_Employee or else (Age_Of_Employee = Age_Of_Next_Employee and then
                Name_Less_Than (Name_Of_Employee, Name_Length_Of_Employee,Employee_Array (Next_Employee_Index).Name,Employee_Array (Next_Employee_Index).Name_Length))
            then
               -- Insert between Current and Next employe nodes
               Employee_Array (Employee_Index_To_Insert).Left_Link := Current_Employee_Index;
               Employee_Array (Employee_Index_To_Insert).Right_Link := Next_Employee_Index;
               Employee_Array (Current_Employee_Index).Right_Link := Employee_Index_To_Insert;
               Employee_Array (Next_Employee_Index).Left_Link := Employee_Index_To_Insert;
               return;
            end if;
         end;

         Current_Employee_Index := Employee_Array (Current_Employee_Index).Right_Link;
      end loop;
      end Insert_Sorted;

   --ascending order, go unitl
   procedure Traverse_Forward (Department : Job_Type;
                              Visit : access procedure (Employee_Index_To_Visit : Employee_Index_Number)) is
      Current_Employee_Index : Employee_Index_Number := Departments (Department).Right_Link;
   begin
      while Current_Employee_Index /= 0 loop
         Visit (Current_Employee_Index);
         Current_Employee_Index := Employee_Array (Current_Employee_Index).Right_Link;
      end loop;
   end Traverse_Forward;

   -- descending order
   procedure Traverse_Backward (Department : Job_Type;
                               Visit : access procedure (Employee_Index_To_Visit : Employee_Index_Number)) is
      Current_Employee_Index : Employee_Index_Number := Departments (Department).Left_Link;
   begin
      while Current_Employee_Index /= 0 loop
         Visit (Current_Employee_Index);
         Current_Employee_Index := Employee_Array (Current_Employee_Index).Left_Link;
      end loop;
   end Traverse_Backward;

end DepartmentLists;
