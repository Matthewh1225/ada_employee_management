--RecordHandler.adb
with Ada.Text_IO; use Ada.Text_IO;
with Types; use Types;
with AvailableEmployeeList; use AvailableEmployeeList;
with DepartmentLists;
with FileReader;
with VehiclePool;

package body RecordHandler is
   Current_Employee_Index : Employee_Index_Number := 0;
   -- Create new employee or find existing one
   procedure Handle_Employee (Name:String; Job : Job_Type;Age:Natural) is
      Employee_Index_To_Check:Employee_Index_Number;
      Found_Existing : Boolean:= False;
   begin
      -- Search department for existing employee with same Name, Job, Age
      if Departments (Job).Count > 0 then
         Employee_Index_To_Check := Departments (Job).Right_Link;
         
         while Employee_Index_To_Check /= 0 loop
            if Employee_Array(Employee_Index_To_Check).Name_Length = Name'Length and then
               Employee_Array(Employee_Index_To_Check).Age = Age and then
               Employee_Array(Employee_Index_To_Check).Name (1 .. Name'Length) = Name then
               -- Found duplicate
               Found_Existing := True;
               Current_Employee_Index := Employee_Index_To_Check;
               exit;
            end if;
            
            Employee_Index_To_Check := Employee_Array (Employee_Index_To_Check).Right_Link;
         end loop;
      end if;
      -- Create new employee if not found
      if not Found_Existing then
         Current_Employee_Index := Allocate_Employee;
         if Current_Employee_Index = 0 then
            return;
         end if;
         Employee_Array (Current_Employee_Index).Name := (others => ' ');
         Employee_Array (Current_Employee_Index).Name (1 .. Name'Length) := Name;
         Employee_Array (Current_Employee_Index).Name_Length := Name'Length;
         Employee_Array (Current_Employee_Index).Age := Age;
         Employee_Array (Current_Employee_Index).Job := Job;
         Employee_Array (Current_Employee_Index).Vehicle_Count := 0;
         Employee_Array (Current_Employee_Index).Vehicles := (others => null);

         -- Insert into sorted  list
         DepartmentLists.Insert_Sorted (Current_Employee_Index);
      end if;
   end Handle_Employee;

   procedure Handle_Vehicle (Make : Manufacturer; Model : Model_Type; Count : Natural; Color : Color_Type) is
      New_Vehicle : Vehicle_Pointer;
   begin
      if Current_Employee_Index = 0 then
      return;
      end if;
      if Employee_Array (Current_Employee_Index).Vehicle_Count >= Maximum_Vehicles_Per_Employee then
         return;
      end if;
      -- Determine vehicle type and engine based on manufacturer
      case Make is
         when Ford | Chevrolet | Dodge | GMC =>
            -- Car manufacturers: Count = number of doors
        New_Vehicle := VehiclePool.Get_Car (Make, Model, Color, Count);

         when GeneralDynamics | Grumman | Lockheed | Boeing =>
         New_Vehicle := VehiclePool.Get_Plane (Make, Model, Color, Jet, Count);

      when NavalGroup | ThyssenKrupp =>
            declare
               Sub_Engine : constant Engine_Type := 
          (if Make = NavalGroup then Nuclear else Diesel);
            begin
             New_Vehicle := VehiclePool.Get_Submarine (Make, Model, Color, Sub_Engine, Count);
            end;
         when SpaceX | Starfleet =>
            declare
               Ship_Engine : constant Engine_Type := 
             (if Model = Starship or Model = Enterprise then Warp_Drive else Light_Speed);
            begin
             New_Vehicle := VehiclePool.Get_Spaceship (Make, Model, Color, Ship_Engine, Count);
            end;
            
         when None_Make=> return;
      end case;
      if New_Vehicle = null then return;
      end if;
      -- Add vehicle to employee's vehicle list
      Employee_Array (Current_Employee_Index).Vehicle_Count := Employee_Array (Current_Employee_Index).Vehicle_Count + 1;
      Employee_Array (Current_Employee_Index).Vehicles (Employee_Array (Current_Employee_Index).Vehicle_Count) := New_Vehicle;
   end Handle_Vehicle;
   function Process_File (Filename : String) return Boolean is
   begin
   FileReader.Read_File (Filename, Handle_Employee'Access, Handle_Vehicle'Access);
   Put_Line ("File processed ");
return True;
   end Process_File;
end RecordHandler;
