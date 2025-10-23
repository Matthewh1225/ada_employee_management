-- VehiclePool.ads
with Types; use Types;
with Enums; use Enums;
-- vehicle pools on the stack
package VehiclePool is
   procedure Initialize;
   function Get_Car (Make :Manufacturer; Model : Model_Type; Color : Color_Type; Doors : Natural)return Vehicle_Pointer;
    function Get_Plane (Make :Manufacturer; Model : Model_Type; Color :Color_Type;  Engine :Engine_Type; Entry_Points : Natural)return Vehicle_Pointer;
   function Get_Submarine (Make :Manufacturer; Model : Model_Type; Color :Color_Type; Engine : Engine_Type; Entry_Points : Natural)return Vehicle_Pointer;
   function Get_Spaceship (Make:Manufacturer; Model : Model_Type; Color : Color_Type; Engine : Engine_Type; Entry_Points : Natural)return Vehicle_Pointer;
end VehiclePool;
