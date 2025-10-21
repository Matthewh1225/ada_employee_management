-- types.ads
-- Type definitions using inheritance (tagged types) for A+ Option
-- Heterogeneous container implementation using Vehicle'Class

with Enums; use Enums;

package Types is

   -- Maximum sizes for our arrays
   Maximum_Employees : constant := 1000;
   Maximum_Vehicles_Per_Employee : constant := 10;  -- Max vehicles per employee (heterogeneous)
   
   -- String length limits
   Maximum_Name_Length  : constant := 40;
   Maximum_Model_Length : constant := 30;

   -- Fixed-length strings for simplicity
   subtype Name_String  is String (1 .. Maximum_Name_Length);
   subtype Model_String is String (1 .. Maximum_Model_Length);

   -- Index types for our arrays (0 means null/empty)
   type Employee_Index_Number is range 0 .. Maximum_Employees;
   
   -- ========================================
   -- INHERITANCE HIERARCHY (A+ Requirement)
   -- ========================================
   
   -- Base tagged type - all vehicles share these attributes
   type Vehicle is tagged record
      Make  : Manufacturer := None_Make;
      Model : Model_Type := None_Model;
      Color : Color_Type := None_Color;
   end record;
   
   -- Derived type: Car extends Vehicle with door count
   type Car is new Vehicle with record
      Doors : Natural := 0;
   end record;
   
   -- Derived type: Plane extends Vehicle with engine type
   type Plane is new Vehicle with record
      Engine : Engine_Type := None_Engine;
   end record;
   
   -- Derived type: Submarine extends Vehicle with engine type
   type Submarine is new Vehicle with record
      Engine : Engine_Type := None_Engine;
   end record;
   
   -- Derived type: Motorcycle extends Vehicle with engine type
   type Motorcycle is new Vehicle with record
      Engine : Engine_Type := None_Engine;
   end record;
   
   -- Pointer type for heterogeneous container (classwide pointer)
   type Vehicle_Pointer is access all Vehicle'Class;
   
   -- Heterogeneous container: array of classwide pointers
   type Vehicle_Pointer_Array is array (1 .. Maximum_Vehicles_Per_Employee) of Vehicle_Pointer;

   -- Employee record (node in doubly-linked list)
   type Employee_Record is record
      Name          : Name_String;
      Name_Length   : Natural := 0;
      Age           : Natural := 0;
      Job           : Job_Type := None;
      
      -- Doubly-linked list pointers (0 = null)
      Left_Link     : Employee_Index_Number := 0;
      Right_Link    : Employee_Index_Number := 0;
      
      -- Heterogeneous container: array of Vehicle'Class pointers
      -- This is the A+ requirement - allows mixing Car, Plane, Submarine, Motorcycle
      Vehicles      : Vehicle_Pointer_Array := (others => null);
      Vehicle_Count : Natural := 0;
      
      -- For free list management
      Next_Free     : Employee_Index_Number := 0;
   end record;

   -- Department head node
   type Department_Head is record
      Job         : Job_Type := None;
      Count       : Natural := 0;
      Left_Link   : Employee_Index_Number := 0;  -- Points to last employee
      Right_Link  : Employee_Index_Number := 0;  -- Points to first employee
   end record;

   -- Global storage arrays
   type Employee_Array_Type is array (Employee_Index_Number range 1 .. Maximum_Employees) of Employee_Record;
   type Department_Array_Type is array (Job_Type) of Department_Head;

   -- Global storage
   Employee_Array : Employee_Array_Type;
   Departments    : Department_Array_Type;

end Types;
