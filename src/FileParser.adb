-- FileParser.adb
-- File parsing implementation using inheritance (A+ Option)
-- This coordinates the reading of Cars.txt and creation of employee/vehicle objects

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Types; use Types;
with Enums; use Enums;
with StringConversions; use StringConversions;
with AvailableEmployeeList; use AvailableEmployeeList;
with DepartmentLists; use DepartmentLists;
with ProcessFile; use ProcessFile;

package body FileParser is

   -- Current employee
   Current_Employee_Index : Employee_Index_Number := 0;

   procedure Handle_Employee (Employee_Record_Data : ProcessFile.Employee_Record) is
      Employee_Index_To_Check : Employee_Index_Number;
      Name_String_From_File : constant String := ProcessFile.To_String(Employee_Record_Data.Name);
      Name_Length_From_File : constant Natural := Employee_Record_Data.Name.Length;
   begin
      -- Check if this is a new employee or existing employee
      if Employee_Record_Data.Job /= None then
         -- Used name + department + age as unique key
         declare
            Found_Existing_Employee : Boolean := False;
         begin
            -- Look through all employees in department
            if Departments (Employee_Record_Data.Job).Count > 0 then
               Employee_Index_To_Check := Departments (Employee_Record_Data.Job).Right_Link;
               while Employee_Index_To_Check /= 0 loop
                  -- Compare names, department, and age for identification
                  if Employee_Array (Employee_Index_To_Check).Name_Length = Name_Length_From_File and then
                     Employee_Array (Employee_Index_To_Check).Age = Employee_Record_Data.Age then
                     if Employee_Array (Employee_Index_To_Check).Name (1 .. Name_Length_From_File) = 
                        Name_String_From_File (1 .. Name_Length_From_File) then
                        Found_Existing_Employee := True;
                        Current_Employee_Index := Employee_Index_To_Check;
                        exit;
                     end if;
                  end if;
                  exit when Employee_Array (Employee_Index_To_Check).Right_Link = Departments (Employee_Record_Data.Job).Right_Link;
                  Employee_Index_To_Check := Employee_Array (Employee_Index_To_Check).Right_Link;
               end loop;
            end if;

            if not Found_Existing_Employee then
               Current_Employee_Index := Allocate_Employee;
               if Current_Employee_Index = 0 then
                  Put_Line ("Error: Employee pool empty");
                  return;
               end if;

               Employee_Array (Current_Employee_Index).Name := (others => ' ');
               for Character_Position in 1 .. Name_Length_From_File loop
                  Employee_Array (Current_Employee_Index).Name (Character_Position) := 
                     Name_String_From_File (Character_Position);
               end loop;
               -- Set fields from employee record
               Employee_Array (Current_Employee_Index).Name_Length := Name_Length_From_File;
               Employee_Array (Current_Employee_Index).Age := Employee_Record_Data.Age;
               Employee_Array (Current_Employee_Index).Job := Employee_Record_Data.Job;
               Employee_Array (Current_Employee_Index).Vehicle_Count := 0;
               Employee_Array (Current_Employee_Index).Vehicles := (others => null);

               -- Add to department linked list
               DepartmentLists.Insert_Sorted (Current_Employee_Index);
            end if;
         end;
      end if;

      if Current_Employee_Index /= 0 and Employee_Record_Data.Vehicle_Category /= None_Vehicle then
         -- Check if employee has max vehicles(4)
         if Employee_Array (Current_Employee_Index).Vehicle_Count >= Maximum_Vehicles_Per_Employee then
            Put_Line ("Warning: Employee has too many vehicles");
            return;
         end if;

         -- Vehicle fields based on category eneum
         case Employee_Record_Data.Vehicle_Category is
            when Car_Type =>
               declare
                  New_Car_Pointer : constant Vehicle_Pointer := new Car'( Make  => Employee_Record_Data.Vehicle_Make,Model => Employee_Record_Data.Vehicle_Model, Color => Employee_Record_Data.Vehicle_Color,Doors => Employee_Record_Data.Vehicle_Count);
               begin
                  Employee_Array (Current_Employee_Index).Vehicle_Count := 
                     Employee_Array (Current_Employee_Index).Vehicle_Count + 1;
                  Employee_Array (Current_Employee_Index).Vehicles (Employee_Array (Current_Employee_Index).Vehicle_Count) := New_Car_Pointer;
               end;

            when Plane_Type =>
               declare
                  New_Plane_Pointer : constant Vehicle_Pointer := new Plane'(Make   => Employee_Record_Data.Vehicle_Make,Model  => Employee_Record_Data.Vehicle_Model,Color  => Employee_Record_Data.Vehicle_Color, Engine => Jet);
               begin
                  Employee_Array (Current_Employee_Index).Vehicle_Count := 
                     Employee_Array (Current_Employee_Index).Vehicle_Count + 1;
                  Employee_Array (Current_Employee_Index).Vehicles (Employee_Array (Current_Employee_Index).Vehicle_Count) := New_Plane_Pointer;
               end;

            when Submarine_Type =>
               declare
                  Engine_Type_For_Submarine : Engine_Type;
                  New_Submarine_Pointer : Vehicle_Pointer;
               begin
                  -- Determine engine type based on manufacturer
                  if Employee_Record_Data.Vehicle_Make = NavalGroup then
                     Engine_Type_For_Submarine := Nuclear;
                  elsif Employee_Record_Data.Vehicle_Make = ThyssenKrupp then
                     Engine_Type_For_Submarine := Diesel;
                  else
                     Engine_Type_For_Submarine := Nuclear; -- 
                  end if;

                  New_Submarine_Pointer := new Submarine'(Make   => Employee_Record_Data.Vehicle_Make, Model  => Employee_Record_Data.Vehicle_Model,Color  => Employee_Record_Data.Vehicle_Color,Engine => Engine_Type_For_Submarine);

                  Employee_Array (Current_Employee_Index).Vehicle_Count := Employee_Array (Current_Employee_Index).Vehicle_Count + 1;
                  Employee_Array (Current_Employee_Index).Vehicles (Employee_Array (Current_Employee_Index).Vehicle_Count) := New_Submarine_Pointer;
               end;

            when Motorcycle_Type =>
               declare
                  Engine_Type_For_Motorcycle : Engine_Type;
                  New_Motorcycle_Pointer : Vehicle_Pointer;
               begin
                  -- Determine engine based on manufacturer/model
                  if Employee_Record_Data.Vehicle_Model = R1 then
                     Engine_Type_For_Motorcycle := Inline_Four;
                  else
                     Engine_Type_For_Motorcycle := V_Twin;  -- Default for other motorcycles
                  end if;

                  New_Motorcycle_Pointer := new Motorcycle'(Make  => Employee_Record_Data.Vehicle_Make, Model  => Employee_Record_Data.Vehicle_Model, Color  => Employee_Record_Data.Vehicle_Color,  Engine => Engine_Type_For_Motorcycle);

                  Employee_Array (Current_Employee_Index).Vehicle_Count := Employee_Array (Current_Employee_Index).Vehicle_Count + 1;
                  Employee_Array (Current_Employee_Index).Vehicles (Employee_Array (Current_Employee_Index).Vehicle_Count) := New_Motorcycle_Pointer;
               end;

            when None_Vehicle =>
               null; 
         end case;
      end if;

   exception
      when Error : others =>
         Put_Line ("Error in Handle_Employee: " & Ada.Exceptions.Exception_Message (Error));
   end Handle_Employee;

   function Process_File (Filename : String) return Boolean is
   begin
      Put_Line ("Reading " & Filename);
      ProcessFile.Read_Employees (Filename, Handle_Employee'Access);
      Put_Line ("File processed successfully");
      return True;
   exception
      when Error : others =>
         Put_Line ("Error processing file: " & Ada.Exceptions.Exception_Message (Error));
         return False;
   end Process_File;

end FileParser;
