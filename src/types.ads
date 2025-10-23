-- Types.ads
with Enums; use Enums;
package Types is
   -- Maximum capacity limits
    Maximum_Employees:constant := 1000;  
    Maximum_Vehicles_Per_Employee:constant := 10;  
    Maximum_Name_Length: constant:= 40;  
    Maximum_Cars:Constant:= 10000;
     Maximum_Planes:constant := 10000;  
    Maximum_Submarines : constant:= 10000;  
   Maximum_Spaceships:constant:= 10000;  
   -- Fixed-length string for employee names
   subtype Name_String is String (1 .. Maximum_Name_Length);
   type Employee_Index_Number is range 0 ..Maximum_Employees;
   type Car_Index_Number is range 0.. Maximum_Cars;
   type Plane_Index_Number is range 0..Maximum_Planes;
   type Submarine_Index_Number is range 0 .. Maximum_Submarines;
   type Spaceship_Index_Number is range 0 ..Maximum_Spaceships;
   -- Base vehicle type
   type Vehicle is tagged record
    Make  : Manufacturer := None_Make;
    Model : Model_Type := None_Model;
    Color : Color_Type := None_Color;
   end record;
   -- types using inheritance
   type Car is new Vehicle with record
    Doors:Natural:=0;
   end record;
   type Plane is new Vehicle with record
   Engine : Engine_Type := None_Engine;
      Entry_Points : Natural := 0;
   end record;
   type Submarine is new Vehicle with record
   Engine : Engine_Type := None_Engine;
      Entry_Points : Natural := 0;
   end record;
   type Spaceship is new Vehicle with record
      Engine : Engine_Type := None_Engine;
      Entry_Points : Natural := 0;
   end record;
   type Vehicle_Pointer is access all Vehicle'Class;
   -- Array to hold multiple vehicles per employee
   type Vehicle_Pointer_Array is array (1..Maximum_Vehicles_Per_Employee) of Vehicle_Pointer;
   -- Employee node record
   type Employee_Record is record
      Name: Name_String;
      Name_Length : Natural := 0;
      Age: Natural := 0;
      Job :Job_Type := None;
      Left_Link : Employee_Index_Number := 0;
      Right_Link : Employee_Index_Number := 0;
      Vehicles : Vehicle_Pointer_Array := (others => null);
      Vehicle_Count : Natural := 0;
      Next_Free : Employee_Index_Number := 0;
   end record;
   -- Department head node for doubly-linked list
   type Department_Head is record
      Count : Natural := 0;
      Left_Link : Employee_Index_Number := 0;
      Right_Link : Employee_Index_Number := 0;
   end record;
   -- Array type definitions
   type Employee_Array_Type is array (Employee_Index_Number range 1 .. Maximum_Employees) of Employee_Record;
   type Department_Array_Type is array (Job_Type) of Department_Head;
   type Car_Array_Type is array (Car_Index_Number range 1 .. Maximum_Cars) of aliased Car;
   type Plane_Array_Type is array (Plane_Index_Number range 1 .. Maximum_Planes) of aliased Plane;
   type Submarine_Array_Type is array (Submarine_Index_Number range 1 .. Maximum_Submarines) of aliased Submarine;
   type Spaceship_Array_Type is array (Spaceship_Index_Number range 1 .. Maximum_Spaceships) of aliased Spaceship;
   Employee_Array : Employee_Array_Type;
   Departments : Department_Array_Type;
   Car_Array : Car_Array_Type;
   Plane_Array : Plane_Array_Type;
   Submarine_Array : Submarine_Array_Type;
   Spaceship_Array : Spaceship_Array_Type;
   -- Compare two names lexicographically
   function Name_Less_Than (Left : Name_String; Left_Length : Natural;  Right : Name_String; Right_Length : Natural) return Boolean;
end Types;
