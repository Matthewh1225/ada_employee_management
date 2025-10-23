--EmployeeReports.adb
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Enums; use Enums;
with DepartmentLists;
package body EmployeeReports is
   -- Print employee name, age, and all their vehicles
   procedure Print_Employee_Details (Employee_Index_To_Print : Employee_Index_Number) is
      Name_Length_Of_Employee :constant Natural:= Employee_Array (Employee_Index_To_Print).Name_Length;
   begin
      Put ("   ");
      Put (Employee_Array(Employee_Index_To_Print).Name (1 .. Name_Length_Of_Employee));
      Put ("Age: ");
      Put (Employee_Array (Employee_Index_To_Print).Age, Width => 0);
      New_Line;
      -- Print all vehicles owned by this employee
      for Vehicle_Index in 1 .. Employee_Array (Employee_Index_To_Print).Vehicle_Count loop
         declare
            Employee_vehicle:constant Vehicle_Pointer := Employee_Array (Employee_Index_To_Print).Vehicles (Vehicle_Index);
         begin
            if Employee_vehicle/=null then
               Put ("      ");
               if Employee_vehicle.all in Car then
                  Put ("Car: ");
                  Put (Manufacturer'Image (Employee_vehicle.Make) & " " & Model_Type'Image (Employee_vehicle.Model) & " " &  Color_Type'Image (Employee_vehicle.Color)& " Doors: ");
                  Put (Car (Employee_vehicle.all).Doors, Width=>0);
                  New_Line;
                  
               elsif Employee_vehicle.all in Plane then
                  Put ("Plane: ");
                  Put (Manufacturer'Image (Employee_vehicle.Make)& " " & Model_Type'Image (Employee_vehicle.Model) & " " &  Color_Type'Image (Employee_vehicle.Color) & " Engine: " &  Engine_Type'Image (Plane (Employee_vehicle.all).Engine) & " Entry Points: ");
                  Put (Plane (Employee_vehicle.all).Entry_Points, Width => 0);
                  New_Line;
                  
               elsif Employee_vehicle.all in Submarine then
                  Put ("Submarine: ");
                  Put (Manufacturer'Image (Employee_vehicle.Make)&" " & Model_Type'Image (Employee_vehicle.Model) & " " & 
                       Color_Type'Image (Employee_vehicle.Color) & " Engine: " & 
                       Engine_Type'Image (Submarine (Employee_vehicle.all).Engine) & " Entry Points: ");
                  Put (Submarine (Employee_vehicle.all).Entry_Points, Width => 0);
                  New_Line;
               elsif Employee_vehicle.all in Spaceship then
                  Put ("Spaceship:");
                  Put (Manufacturer'Image (Employee_vehicle.Make) & " " & Model_Type'Image (Employee_vehicle.Model) & " " & 
                       Color_Type'Image (Employee_vehicle.Color) & " Engine:" & 
                       Engine_Type'Image (Spaceship (Employee_vehicle.all).Engine) & " Entry Points: ");
                  Put (Spaceship (Employee_vehicle.all).Entry_Points, Width => 0);
                  New_Line;
         
               else
                  Put_Line ("Unknown vehicle type");
               end if;
            end if;
         end;
      end loop;
   end Print_Employee_Details;
   -- Print all departments and employees in ascending order
   procedure Print_Ascending is
   begin
      Put_Line ("");
      Put_Line ("ascending order");
      for Current_Job_Type in Job_Type loop
         if Departments (Current_Job_Type).Count > 0 then
            Put_Line ("Department: "&Job_Type'Image (Current_Job_Type));
            Put ("  Count: ");
            Put (Departments (Current_Job_Type).Count, Width => 0);
            New_Line;
            DepartmentLists.Traverse_Forward (Current_Job_Type, Print_Employee_Details'Access);
            New_Line;
         end if;
      end loop;
   end Print_Ascending;
end EmployeeReports;
