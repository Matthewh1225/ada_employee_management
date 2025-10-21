-- DepartmentLists.ads
-- Doubly-linked list operations with head nodes for each department
-- Manages sorted employee lists organized by job type (department)

with Types; use Types;
with Enums; use Enums;

package DepartmentLists is

   -- all department head nodes
   procedure Initialize_Departments;

   -- Insert  into sorted posiion in department
   -- Sorted by Age ascending, then Name ascending
   procedure Insert_Sorted (Employee_Index_To_Insert : Employee_Index_Number);

   -- ascending order
   procedure Traverse_Forward (Department : Job_Type;
                              Visit : access procedure (Employee_Index_To_Visit : Employee_Index_Number));

   -- descending order
   procedure Traverse_Backward (Department : Job_Type;
                               Visit : access procedure (Employee_Index_To_Visit : Employee_Index_Number));

end DepartmentLists;
