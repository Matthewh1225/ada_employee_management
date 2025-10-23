--EmployeeReports.ads
with Types; use Types;
package EmployeeReports is
   -- Print all departments and employees in ascending order
   procedure Print_Ascending;
   -- Print an employe details 
   procedure Print_Employee_Details (Employee_Index_To_Print : Employee_Index_Number);
end EmployeeReports;
