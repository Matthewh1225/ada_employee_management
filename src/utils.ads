-- utils.ads
-- Simple utility functions for string handling and parsing
-- Beginner-friendly helpers without complex libraries

with Types; use Types;
with Enums; use Enums;

package Utils is

   -- Trim leading and trailing spaces from a string
   function Trim (S : String) return String;

   -- Copy a string into a fixed-length string, padding with spaces
   procedure Copy_String (Source : String; Target : out Name_String; Length : out Natural);
   procedure Copy_Model (Source : String; Target : out Model_String; Length : out Natural);

   -- Compare two name strings (for sorting)
   function Name_Less_Than (Left : Name_String; Left_Len : Natural;
                           Right : Name_String; Right_Len : Natural) return Boolean;

   -- Convert string to integer (simple parser)
   function To_Integer (S : String) return Integer;

   -- Convert string to enumeration types
   function To_Job_Type (S : String) return Job_Type;
   function To_Manufacturer (S : String) return Manufacturer;
   function To_Model_Type (S : String) return Model_Type;
   function To_Color_Type (S : String) return Color_Type;
   function To_Engine_Type (S : String) return Engine_Type;

   -- Convert enumeration to string (for output)
   function Job_Type_Image (J : Job_Type) return String;
   function Manufacturer_Image (M : Manufacturer) return String;
   function Model_Image (M : Model_Type) return String;
   function Color_Image (C : Color_Type) return String;
   function Engine_Type_Image (E : Engine_Type) return String;

end Utils;
