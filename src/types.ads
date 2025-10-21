with Enums; use Enums;

package Types is

   -- Maximum employees and vehicles
   Maximum_Employees : constant := 1000;
   Maximum_Vehicles_Per_Employee : constant := 4;  -- Max vehicles per employee 
   
   -- String length 
   Maximum_Name_Length  : constant := 40;
   Maximum_Model_Length : constant := 30;

   -- Fixed-length strings
   subtype Name_String  is String (1 .. Maximum_Name_Length);
   subtype Model_String is String (1 .. Maximum_Model_Length);

   type Employee_Index_Number is range 0 .. Maximum_Employees;
   

   -- Base type thast all vehicles get shared attributes
   type Vehicle is tagged record
      Make  : Manufacturer := None_Make;
      Model : Model_Type := None_Model;
      Color : Color_Type := None_Color;
   end record;
   
   type Car is new Vehicle with record
      Doors : Natural := 0;
   end record;
   
   type Plane is new Vehicle with record
      Engine : Engine_Type := None_Engine;
   end record;
   
   type Submarine is new Vehicle with record
      Engine : Engine_Type := None_Engine;
   end record;
   
   type Motorcycle is new Vehicle with record
      Engine : Engine_Type := None_Engine;
   end record;
   
   -- classwide pointer for heterogeneous container
   type Vehicle_Pointer is access all Vehicle'Class;
   
   type Vehicle_Pointer_Array is array (1 .. Maximum_Vehicles_Per_Employee) of Vehicle_Pointer;

   -- Employee record
   type Employee_Record is record
      Name          : Name_String;
      Name_Length   : Natural := 0;
      Age           : Natural := 0;
      Job           : Job_Type := None;
      
      Left_Link     : Employee_Index_Number := 0;
      Right_Link    : Employee_Index_Number := 0;
      
      Vehicles      : Vehicle_Pointer_Array := (others => null);
      Vehicle_Count : Natural := 0;
      
      Next_Free     : Employee_Index_Number := 0;
   end record;

   -- Department head node
   type Department_Head is record
      Job         : Job_Type := None;
      Count       : Natural := 0;
      Left_Link   : Employee_Index_Number := 0;  -- Points to last employee
      Right_Link  : Employee_Index_Number := 0;  -- Points to first employee
   end record;

   type Employee_Array_Type is array (Employee_Index_Number range 1 .. Maximum_Employees) of Employee_Record;
   type Department_Array_Type is array (Job_Type) of Department_Head;

   Employee_Array : Employee_Array_Type;
   Departments    : Department_Array_Type;

end Types;
