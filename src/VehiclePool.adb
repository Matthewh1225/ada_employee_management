-- VehiclePool.adb
package body VehiclePool is
   Next_Car_Index:Car_Index_Number:=1;
   Next_Plane_Index: Plane_Index_Number := 1;
   Next_Submarine_Index: Submarine_Index_Number:= 1;
   Next_Spaceship_Index : Spaceship_Index_Number := 1;
   procedure Initialize is
   begin
       Next_Car_Index:= 1;
      Next_Plane_Index:=1;
       Next_Submarine_Index:=1;
      Next_Spaceship_Index := 1;
   end Initialize;
   -- Allocate a car from the pool and retur pointe
   -- Returnsnull if pool is full 
   function Get_Car (Make : Manufacturer; Model : Model_Type; Color: Color_Type; Doors : Natural) return Vehicle_Pointer is
   begin
      if Next_Car_Index = Maximum_Cars then
         return null;
      end if;
      Car_Array (Next_Car_Index) := (Make =>Make, Model => Model, Color=> Color, Doors => Doors);
      -- Return pointer to this car and advance to next slot
      Next_Car_Index := Next_Car_Index + 1;
      return Car_Array (Next_Car_Index - 1)'Access;
   end Get_Car;
   function Get_Plane (Make : Manufacturer; Model : Model_Type; Color : Color_Type; Engine :Engine_Type; Entry_Points : Natural) return Vehicle_Pointer is
   begin
      if Next_Plane_Index = Maximum_Planes then
         return null;
      end if;
      Plane_Array (Next_Plane_Index) := (Make => Make, Model => Model, Color => Color,  Engine =>Engine, Entry_Points => Entry_Points);
      Next_Plane_Index := Next_Plane_Index + 1;
      return Plane_Array (Next_Plane_Index - 1)'Access;
   end Get_Plane;
   function Get_Submarine (Make : Manufacturer;Model : Model_Type;Color : Color_Type;Engine : Engine_Type;Entry_Points : Natural) return Vehicle_Pointer is
   begin
      if Next_Submarine_Index = Maximum_Submarines then
         return null;
      end if;
      Submarine_Array (Next_Submarine_Index) := (Make => Make, Model =>Model, Color => Color, Engine => Engine,Entry_Points => Entry_Points);
      Next_Submarine_Index := Next_Submarine_Index + 1;
      return Submarine_Array (Next_Submarine_Index - 1)'Access;
   end Get_Submarine;
   function Get_Spaceship (Make : Manufacturer; Model : Model_Type;Color : Color_Type; Engine : Engine_Type; Entry_Points : Natural) return Vehicle_Pointer is
   begin
      if Next_Spaceship_Index = Maximum_Spaceships then
      return null;
      end if;
      Spaceship_Array (Next_Spaceship_Index) := (Make => Make, Model =>Model, Color => Color, Engine => Engine, Entry_Points => Entry_Points);
      Next_Spaceship_Index := Next_Spaceship_Index + 1;
      return Spaceship_Array (Next_Spaceship_Index - 1)'Access;
   end Get_Spaceship;
end VehiclePool;
