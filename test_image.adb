with Ada.Text_IO; use Ada.Text_IO;
with Enums; use Enums;
with StringConversions; use StringConversions;

procedure Test_Image is
begin
   Put_Line ("Testing 'Image vs Custom Functions:");
   Put_Line ("");
   
   Put_Line ("Job_Type'Image(Programmer): " & Job_Type'Image(Programmer));
   Put_Line ("Job_Type_Image(Programmer):  " & Job_Type_Image(Programmer));
   Put_Line ("");
   
   Put_Line ("Manufacturer'Image(Ford): " & Manufacturer'Image(Ford));
   Put_Line ("Manufacturer_Image(Ford):  " & Manufacturer_Image(Ford));
   Put_Line ("");
   
   Put_Line ("Model_Type'Image(F16): " & Model_Type'Image(F16));
   Put_Line ("Model_Image(F16):       " & Model_Image(F16));
   Put_Line ("");
   
   Put_Line ("Engine_Type'Image(V_Twin): " & Engine_Type'Image(V_Twin));
   Put_Line ("Engine_Type_Image(V_Twin):  " & Engine_Type_Image(V_Twin));
end Test_Image;
