-- lists.adb
-- Doubly-linked list implementation with head nodes

with Enums; use Enums;
with Utils; use Utils;

package body Lists is

   -- Initialize all department head nodes
   procedure Initialize_Departments is
   begin
      -- Set up head node for each department
      for Department_Type in Job_Type loop
         if Department_Type /= None then
            Departments (Department_Type).Job := Department_Type;
            Departments (Department_Type).Count := 0;
            Departments (Department_Type).Left_Link := 0;
            Departments (Department_Type).Right_Link := 0;
         end if;
      end loop;
   end Initialize_Departments;

   -- Insert employee into sorted position
   -- First by Age (ascending), then by Name (ascending) if ages equal
   procedure Insert_Sorted (Employee_Index_To_Insert : Employee_Index_Number) is
      Department_Of_Employee : constant Job_Type := Employee_Array (Employee_Index_To_Insert).Job;
      Age_Of_Employee        : constant Natural := Employee_Array (Employee_Index_To_Insert).Age;
      Name_Of_Employee       : constant Name_String := Employee_Array (Employee_Index_To_Insert).Name;
      Name_Length_Of_Employee: constant Natural := Employee_Array (Employee_Index_To_Insert).Name_Length;
      
      Current_Employee_Index : Employee_Index_Number;
   begin
      -- Increment count
      Departments (Department_Of_Employee).Count := Departments (Department_Of_Employee).Count + 1;

      -- Case 1: Empty list - insert as first element
      if Departments (Department_Of_Employee).Right_Link = 0 then
         Employee_Array (Employee_Index_To_Insert).Left_Link := 0;
         Employee_Array (Employee_Index_To_Insert).Right_Link := 0;
         Departments (Department_Of_Employee).Right_Link := Employee_Index_To_Insert;
         Departments (Department_Of_Employee).Left_Link := Employee_Index_To_Insert;
         return;
      end if;

      -- Start at beginning of list
      Current_Employee_Index := Departments (Department_Of_Employee).Right_Link;

      -- Case 2: Insert at beginning (before first element)
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

      -- Case 3 & 4: Find position in middle or insert at end
      loop
         -- Check if we should insert before next element
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
            if Age_Of_Employee < Age_Of_Next_Employee or else
               (Age_Of_Employee = Age_Of_Next_Employee and then
                Name_Less_Than (Name_Of_Employee, Name_Length_Of_Employee,
                               Employee_Array (Next_Employee_Index).Name,
                               Employee_Array (Next_Employee_Index).Name_Length))
            then
               -- Insert between Current and Next
               Employee_Array (Employee_Index_To_Insert).Left_Link := Current_Employee_Index;
               Employee_Array (Employee_Index_To_Insert).Right_Link := Next_Employee_Index;
               Employee_Array (Current_Employee_Index).Right_Link := Employee_Index_To_Insert;
               Employee_Array (Next_Employee_Index).Left_Link := Employee_Index_To_Insert;
               return;
            end if;
         end;

         -- Move to next element
         Current_Employee_Index := Employee_Array (Current_Employee_Index).Right_Link;
      end loop;
   end Insert_Sorted;

   -- Traverse forward (ascending order)
   procedure Traverse_Forward (Department : Job_Type;
                              Visit : access procedure (Employee_Index_To_Visit : Employee_Index_Number)) is
      Current_Employee_Index : Employee_Index_Number := Departments (Department).Right_Link;
   begin
      while Current_Employee_Index /= 0 loop
         Visit (Current_Employee_Index);
         Current_Employee_Index := Employee_Array (Current_Employee_Index).Right_Link;
      end loop;
   end Traverse_Forward;

   -- Traverse backward (descending order)
   procedure Traverse_Backward (Department : Job_Type;
                               Visit : access procedure (Employee_Index_To_Visit : Employee_Index_Number)) is
      Current_Employee_Index : Employee_Index_Number := Departments (Department).Left_Link;
   begin
      while Current_Employee_Index /= 0 loop
         Visit (Current_Employee_Index);
         Current_Employee_Index := Employee_Array (Current_Employee_Index).Left_Link;
      end loop;
   end Traverse_Backward;

end Lists;
