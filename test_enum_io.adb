-- Test to show Enumeration_IO output
with Ada.Text_IO; use Ada.Text_IO;
with Enums; use Enums;
with StringConversions; use StringConversions;

procedure Test_Enum_IO is
begin
   Put_Line ("=== Direct Enumeration_IO Output ===");
   Put ("Using Manufacturer_IO.Put: ");
   Manufacturer_IO.Put (Ford);
   New_Line;
   
   Put ("Using Model_Type_IO.Put: ");
   Model_Type_IO.Put (Raptor);
   New_Line;
   
   Put ("Using Color_Type_IO.Put: ");
   Color_Type_IO.Put (Blue);
   New_Line;
   
   New_Line;
   Put_Line ("=== Using Your Image Functions ===");
   Put_Line ("Using Manufacturer_Image: " & Manufacturer_Image (Ford));
   Put_Line ("Using Model_Image: " & Model_Image (Raptor));
   Put_Line ("Using Color_Image: " & Color_Image (Blue));
   
   New_Line;
   Put_Line ("=== The Problem ===");
   Put_Line ("Enumeration_IO outputs ALL CAPS and is padded.");
   Put_Line ("Your Image functions output nicely capitalized text.");
end Test_Enum_IO;
