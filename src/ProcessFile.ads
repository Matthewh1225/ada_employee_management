with Enums; use Enums;

package ProcessFile is
   Max_Name_Length : constant := 32;

   type Name_Buffer is record
      Length : Natural range 0 .. Max_Name_Length := 0;
      Data   : String (1 .. Max_Name_Length)      := (others => ' ');
   end record;

   type Employee_Record is record
      Name             : Name_Buffer;
      Job              : Job_Type      := None;
      Age              : Integer       := 0;
      Vehicle_Make     : Manufacturer  := None_Make;
      Vehicle_Model    : Model_Type    := None_Model;
      Vehicle_Count    : Natural       := 0;
      Vehicle_Color    : Color_Type    := None_Color;
      Vehicle_Category : Vehicle_Kind  := None_Vehicle;
   end record;

   function To_String (Value : Name_Buffer) return String;

   procedure Read_Employees
     (File_Name : String;
      Process_Employee :
        not null access procedure (Current_Employee : in Employee_Record));
end ProcessFile;
