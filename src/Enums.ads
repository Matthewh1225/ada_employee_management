package Enums is
   -- Job types
   type Job_Type is (None, Accountant, Analysist, Manager, Programmer, Sales);
   
   -- Transportation types
   type Vehicle_Kind is (Car_Type, Plane_Type, Submarine_Type, Motorcycle_Type, None_Vehicle);
   
   type Manufacturer is (None_Make, Ford, Chevrolet, Dodge, GMC,GeneralDynamics, Grumman, Lockheed, Boeing, NavalGroup, ThyssenKrupp,  Harley, Honda, Yamaha, Ducati);         
   
   type Model_Type is (None_Model, Expedition, Raptor, Camaro, Pickup, Stingray, Charger, Ram, Devil,F16, Commercial, F35, B747, Virginia, Typhoon, Trident,Sportster, Shadow, R1, Panigale);
   
   -- Colors
   type Color_Type is (None_Color, Blue, Red, White, Orange, Black, Silver, Camo, Yellow, Green);
   
   -- Engine types 
   type Engine_Type is (None_Engine, Jet, Blade, Nuclear, Diesel, V_Twin, Inline_Four);
end Enums;