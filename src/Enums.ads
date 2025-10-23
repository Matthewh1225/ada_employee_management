-- Enums.ads
with Ada.Text_IO;
package Enums is
   -- Employee job types
   type Job_Type is (None, Accountant, Analysist, Manager, Programmer, Sales, Captain);
   -- Vehicle manufacturers 
   type Manufacturer is (None_Make, Ford, Chevrolet, Dodge, GMC, GeneralDynamics, Grumman, Lockheed, Boeing,   NavalGroup, ThyssenKrupp, SpaceX, Starfleet);

   -- Vehicle models 
   type Model_Type is (None_Model, Expedition, Raptor, Camaro, Pickup, Stingray, Charger, Ram, Devil, F16, Commercial, F35, B747, 
    Virginia, Typhoon, Trident, Starship, Dragon, Enterprise);
   -- Vehicle colors
   type Color_Type is (None_Color, Blue, Red, White, Orange, Black, Silver, Camo, Yellow, Green);
   -- Engine types 
   type Engine_Type is (None_Engine, Jet, Nuclear, Diesel, Warp_Drive, Light_Speed);
   -- Enumeration_IO  for  string-to-enum conversion
   package Job_Type_IO is new Ada.Text_IO.Enumeration_IO (Job_Type);
   package Manufacturer_IO is new Ada.Text_IO.Enumeration_IO (Manufacturer);
   package Model_Type_IO is new Ada.Text_IO.Enumeration_IO (Model_Type);
   package Color_Type_IO is new Ada.Text_IO.Enumeration_IO (Color_Type);
   package Engine_Type_IO is new Ada.Text_IO.Enumeration_IO (Engine_Type);

end Enums;