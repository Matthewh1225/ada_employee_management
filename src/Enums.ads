package Enums is
   -- Job types
   type Job_Type is (None, Accountant, Analysist, Manager, Programmer, Sales);
   
   -- Transportation types
   type Vehicle_Kind is (Car_Type, Plane_Type, Submarine_Type, Motorcycle_Type, None_Vehicle);
   
   -- Manufacturers (expanded for new vehicle types)
   type Manufacturer is (None_Make, 
                        Ford, Chevrolet, Dodge, GMC,                    -- Car manufacturers
                        GeneralDynamics, Grumman, Lockheed, Boeing,     -- Plane manufacturers
                        NavalGroup, ThyssenKrupp,                       -- Submarine manufacturers
                        Harley, Honda, Yamaha, Ducati);                 -- Motorcycle manufacturers
   
   -- Models (expanded for all vehicle types)
   type Model_Type is (None_Model, 
                      -- Car models
                      Expedition, Raptor, Camaro, Pickup, Stingray, Charger, Ram, Devil,
                      -- Plane models
                      F16, Commercial, F35, B747,
                      -- Submarine models
                      Virginia, Typhoon, Trident,
                      -- Motorcycle models
                      Sportster, Shadow, R1, Panigale);
   
   -- Colors
   type Color_Type is (None_Color, Blue, Red, White, Orange, Black, Silver, Camo, Yellow, Green);
   
   -- Engine types (expanded for all vehicle types)
   type Engine_Type is (None_Engine, Jet, Blade, Nuclear, Diesel, V_Twin, Inline_Four);
end Enums;