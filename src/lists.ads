-- lists.ads
-- Doubly-linked list operations with head nodes

with Types; use Types;
with Enums; use Enums;

package Lists is

   -- Initialize all department head nodes
   procedure Initialize_Departments;

   -- Insert employee into sorted position in department
   -- Sorted by Age ascending, then Name ascending
   procedure Insert_Sorted (Employee_Index_To_Insert : Employee_Index_Number);

   -- Traverse department forward (ascending order)
   procedure Traverse_Forward (Department : Job_Type;
                              Visit : access procedure (Employee_Index_To_Visit : Employee_Index_Number));

   -- Traverse department backward (descending order)
   procedure Traverse_Backward (Department : Job_Type;
                               Visit : access procedure (Employee_Index_To_Visit : Employee_Index_Number));

end Lists;
