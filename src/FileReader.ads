-- FileReader.ads
with Enums; use Enums;
package FileReader is
   -- Fixed-size string
   subtype Word_String is String (1 .. 50);
   -- Array to hold words  from one line
   type Word_Array is array (1 .. 10) of Word_String;
   -- Split line to words
   procedure Get_Words (Line : String; Words : out Word_Array; Count : out Natural);
   -- Remove trailing spaces from a word (returns actual content)
   function Trim_Word (Word : Word_String) return String;
   procedure Read_File (File_Name : String;  On_Employee : access procedure (Name : String; Job : Job_Type; Age : Natural); 
    On_Vehicle : access procedure (Make : Manufacturer; Model : Model_Type; Count : Natural; Color : Color_Type));

end FileReader;
