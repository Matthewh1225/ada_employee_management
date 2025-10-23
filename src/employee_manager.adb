-- Employee_Manager.adb(Main)
with Ada.Text_IO; use Ada.Text_IO;
with AvailableEmployeeList;
with DepartmentLists;
with RecordHandler; use RecordHandler;
with EmployeeReports; use EmployeeReports;
with VehiclePool;

procedure Employee_Manager is
   Success : Boolean;
   Output_File : File_Type;
begin
   -- Create output file
   Create (Output_File, Out_File, "output.txt");
   Set_Output (Output_File);  -- Redirect all Put_Line to file
   
   Put_Line ("Employee Transport System");
   New_Line;
   
   -- Initialize all data structures 
   AvailableEmployeeList.Initialize;
   DepartmentLists.Initialize_Departments;
   VehiclePool.Initialize;
   
   Success := Process_File ("Employee_Records.txt");
   if not Success then
      Put_Line ("Error processing file.");
      Close (Output_File);
      return;
   end if;
   
   Print_Ascending;
   
   -- Close output file and restore standard output
   Close (Output_File);
   Set_Output (Standard_Output);
   
end Employee_Manager;
