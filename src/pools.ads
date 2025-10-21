-- pools.ads
-- Free list management for employee storage pool
-- Vehicles are now heap-allocated using 'new' (inheritance model)

with Types; use Types;

package Pools is

   -- Initialize employee pool (set up free list)
   procedure Initialize;

   -- Allocate a new employee from the pool
   function Allocate_Employee return Employee_Index_Number;

   -- Free an employee back to the pool
   procedure Free_Employee (Index : Employee_Index_Number);

end Pools;
