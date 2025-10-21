-- report.adb
-- Report generation implementation using inheritance (A+ Option)

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Types; use Types;
with Enums; use Enums;
with Utils; use Utils;

package body Report is

   procedure Print_Employee_Details (Employee_Index_To_Print : Employee_Index_Number) is
      Name_Length_Of_Employee : constant Natural := Employee_Array (Employee_Index_To_Print).Name_Length;
   begin
      Put ("   ");
      Put (Employee_Array (Employee_Index_To_Print).Name (1 .. Name_Length_Of_Employee));
      Put (" Age: ");
      Put (Employee_Array (Employee_Index_To_Print).Age, Width => 0);
      New_Line;

      -- Print all vehicles using inheritance and type checking
      for Vehicle_Index in 1 .. Employee_Array (Employee_Index_To_Print).Vehicle_Count loop
         declare
            Current_Vehicle_Pointer : constant Vehicle_Pointer := Employee_Array (Employee_Index_To_Print).Vehicles (Vehicle_Index);
         begin
            if Current_Vehicle_Pointer /= null then
               Put ("      ");
               
               -- Use 'in' membership test to determine actual type (A+ requirement)
               if Current_Vehicle_Pointer.all in Car then
                  Put ("Car: ");
                  Put (Manufacturer_Image (Current_Vehicle_Pointer.Make));
                  Put (" ");
                  Put (Model_Image (Current_Vehicle_Pointer.Model));
                  Put (" ");
                  Put (Color_Image (Current_Vehicle_Pointer.Color));
                  Put (" Doors: ");
                  Put (Car (Current_Vehicle_Pointer.all).Doors, Width => 0);  -- Type conversion to access Doors
                  New_Line;
                  
               elsif Current_Vehicle_Pointer.all in Plane then
                  Put ("Plane: ");
                  Put (Manufacturer_Image (Current_Vehicle_Pointer.Make));
                  Put (" ");
                  Put (Model_Image (Current_Vehicle_Pointer.Model));
                  Put (" ");
                  Put (Color_Image (Current_Vehicle_Pointer.Color));
                  Put (" Engine: ");
                  Put (Engine_Type_Image (Plane (Current_Vehicle_Pointer.all).Engine));  -- Type conversion to access Engine
                  New_Line;
                  
               elsif Current_Vehicle_Pointer.all in Submarine then
                  Put ("Submarine: ");
                  Put (Manufacturer_Image (Current_Vehicle_Pointer.Make));
                  Put (" ");
                  Put (Model_Image (Current_Vehicle_Pointer.Model));
                  Put (" ");
                  Put (Color_Image (Current_Vehicle_Pointer.Color));
                  Put (" Engine: ");
                  Put (Engine_Type_Image (Submarine (Current_Vehicle_Pointer.all).Engine));  -- Type conversion to access Engine
                  New_Line;
                  
               elsif Current_Vehicle_Pointer.all in Motorcycle then
                  Put ("Motorcycle: ");
                  Put (Manufacturer_Image (Current_Vehicle_Pointer.Make));
                  Put (" ");
                  Put (Model_Image (Current_Vehicle_Pointer.Model));
                  Put (" ");
                  Put (Color_Image (Current_Vehicle_Pointer.Color));
                  Put (" Engine: ");
                  Put (Engine_Type_Image (Motorcycle (Current_Vehicle_Pointer.all).Engine));  -- Type conversion to access Engine
                  New_Line;
                  
               else
                  Put ("Unknown vehicle type");
                  New_Line;
               end if;
            end if;
         end;
      end loop;
   end Print_Employee_Details;

   procedure Print_Ascending is
      Current_Employee_Index : Employee_Index_Number;
   begin
      Put_Line ("");
      Put_Line ("=== ASCENDING REPORT ===");
      Put_Line ("Departments and employees in ascending order");
      Put_Line ("(Sorted by Job Type, then Age, then Name)");
      Put_Line ("");

      for Current_Job_Type in Job_Type loop
         if Departments (Current_Job_Type).Count > 0 then
            Put ("Department: ");
            Put_Line (Job_Type_Image (Current_Job_Type));
            Put ("   Count: ");
            Put (Departments (Current_Job_Type).Count, Width => 0);
            New_Line;

            -- Traverse the doubly-linked list forward
            Current_Employee_Index := Departments (Current_Job_Type).Right_Link;
            while Current_Employee_Index /= 0 loop
               Print_Employee_Details (Current_Employee_Index);
               exit when Employee_Array (Current_Employee_Index).Right_Link = Departments (Current_Job_Type).Right_Link;
               Current_Employee_Index := Employee_Array (Current_Employee_Index).Right_Link;
            end loop;
            New_Line;
         end if;
      end loop;

      Put_Line ("=== END ASCENDING REPORT ===");
   end Print_Ascending;

   procedure Print_Descending is
      Current_Employee_Index : Employee_Index_Number;
   begin
      Put_Line ("");
      Put_Line ("=== DESCENDING REPORT ===");
      Put_Line ("Departments and employees in descending order");
      Put_Line ("(Sorted by Job Type descending, then Age descending, then Name descending)");
      Put_Line ("");

      for Current_Job_Type in reverse Job_Type loop
         if Departments (Current_Job_Type).Count > 0 then
            Put ("Department: ");
            Put_Line (Job_Type_Image (Current_Job_Type));
            Put ("   Count: ");
            Put (Departments (Current_Job_Type).Count, Width => 0);
            New_Line;

            -- Traverse the doubly-linked list backward
            Current_Employee_Index := Departments (Current_Job_Type).Left_Link;
            while Current_Employee_Index /= 0 loop
               Print_Employee_Details (Current_Employee_Index);
               exit when Employee_Array (Current_Employee_Index).Left_Link = Departments (Current_Job_Type).Left_Link;
               Current_Employee_Index := Employee_Array (Current_Employee_Index).Left_Link;
            end loop;
            New_Line;
         end if;
      end loop;

      Put_Line ("=== END DESCENDING REPORT ===");
   end Print_Descending;

end Report;
