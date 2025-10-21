-- employee_manager.adb
-- Main driver program for employee management system
-- Reads Cars.txt and generates ascending/descending reports

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Types; use Types;
with Enums; use Enums;
with StringConversions; use StringConversions;
with AvailableEmployeeList; use AvailableEmployeeList;
with DepartmentLists; use DepartmentLists;
with FileParser; use FileParser;
with EmployeeReports; use EmployeeReports;

procedure Employee_Manager is
   Success : Boolean;
begin
   Put_Line ("Employee Management System");
   Put_Line ("Simple beginner-level Ada implementation");
   Put_Line ("------------------------------------------");
   New_Line;

   -- Initialize all data structures
   Put_Line ("Initializing available employee list and department lists...");
   AvailableEmployeeList.Initialize;
   DepartmentLists.Initialize_Departments;
   New_Line;

   -- Read and process input file
   Put_Line ("Reading Cars.txt...");
   Success := Process_File ("Cars.txt");
   
   if not Success then
      Put_Line ("ERROR: Failed to process file");
      return;
   end if;

   Put_Line ("File processed successfully");
   New_Line;

   -- Generate reports
   Print_Ascending;
   Print_Descending;

   Put_Line ("Program completed successfully");
end Employee_Manager;
