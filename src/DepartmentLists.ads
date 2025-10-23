--DepartmentLists.ads
with Types; use Types;
with Enums; use Enums;
package DepartmentLists is
   procedure Initialize_Departments;
   -- Insert employee into department in sorted order by Agethen Name
   procedure Insert_Sorted (Employee_Index_To_Insert:Employee_Index_Number);
   -- Traverse list forward, calling Visit for each
   procedure Traverse_Forward (Department : Job_Type; Visit : access procedure (Employee_Index_To_Visit : Employee_Index_Number));
   -- Remove employee from department list and return slot to free list
   procedure Delete_Employee (Employee_Index_To_Delete : Employee_Index_Number);
end DepartmentLists;
