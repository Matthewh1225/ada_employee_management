-- ProcessFile.adb
-- BEGINNER VERSION using Get_Line (250 lines vs 500!)
--
-- SIMPLIFIED APPROACH:
-- Instead of reading character-by-character (low-level), we:
--   1. Read entire lines with Get_Line
--   2. Split lines into words  
--   3. Convert words to proper types
--
-- This is MUCH EASIER for beginners to understand!

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body ProcessFile is

   -- HELPER FUNCTION: Remove spaces from start and end
   -- Example: "  hello  " becomes "hello"
   function Trim (Text : String) return String is
      First : Natural := Text'First;  -- Start position
      Last  : Natural := Text'Last;   -- End position
   begin
      -- Skip spaces at the beginning
      while First <= Last and then Text (First) = ' ' loop
         First := First + 1;
      end loop;
      
      -- Skip spaces at the end
      while Last >= First and then Text (Last) = ' ' loop
         Last := Last - 1;
      end loop;
      
      -- Return trimmed string (or empty if all spaces)
      if First > Last then
         return "";
      else
         return Text (First .. Last);
      end if;
   end Trim;

   -- HELPER FUNCTION: Convert string to uppercase
   -- Example: "Hello" becomes "HELLO"
   function Upper (Text : String) return String is
      Result : String (Text'Range);  -- Same size as input
   begin
      for I in Text'Range loop
         Result (I) := To_Upper (Text (I));  -- Convert each character
      end loop;
      return Result;
   end Upper;

   -- KEY SIMPLIFICATION: Store up to 10 words, each up to 50 characters
   -- Example: "Bob Manager 44" becomes 3 words in the array
   type Word_List is array (1 .. 10) of String (1 .. 50);
   
   -- CORE FUNCTION: Split a line into separate words
   -- Example: "Bob Manager 44 Ford" → ["Bob", "Manager", "44", "Ford"]
   procedure Split_Words (Line : String; Words : out Word_List; Count : out Natural) is
      Pos : Natural := Line'First;  -- Current position in line
      Word_Start : Natural;         -- Where current word begins
      Word_End : Natural;           -- Where current word ends
   begin
      Count := 0;  -- No words found yet
      
      -- Loop through entire line
      while Pos <= Line'Last loop
         -- Skip any spaces between words
         while Pos <= Line'Last and then Line (Pos) = ' ' loop
            Pos := Pos + 1;
         end loop;
         
         -- If we're at the end, we're done
         exit when Pos > Line'Last;
         
         -- Found start of a word
         Word_Start := Pos;
         
         -- Find the end of this word (next space)
         while Pos <= Line'Last and then Line (Pos) /= ' ' loop
            Pos := Pos + 1;
         end loop;
         Word_End := Pos - 1;
         
         -- Store this word in our array
         Count := Count + 1;
         if Count <= 10 then
            declare
               Len : constant Natural := Word_End - Word_Start + 1;
            begin
               Words (Count) (1 .. Len) := Line (Word_Start .. Word_End);
               Words (Count) (Len + 1 .. 50) := (others => ' ');  -- Fill rest with spaces
            end;
         end if;
      end loop;
   end Split_Words;

   -- HELPER: Get a single word from the word array
   -- Automatically removes extra spaces
   function Get_Word (Words : Word_List; Index : Positive) return String is
   begin
      return Trim (Words (Index));
   end Get_Word;

   -- CONVERSION FUNCTION: Convert text to Job_Type enumeration
   -- Example: "manager" or "MANAGER" → Manager
   function To_Job (Text : String) return Job_Type is
      U : constant String := Upper (Trim (Text));  -- Clean and uppercase
   begin
      if U = "MANAGER" then return Manager;
      elsif U = "SALES" then return Sales;
      elsif U = "ANALYSIST" then return Analysist;
      elsif U = "PROGRAMMER" then return Programmer;
      elsif U = "ACCOUNTANT" then return Accountant;
      else return None;
      end if;
   end To_Job;

   function To_Make (Text : String) return Manufacturer is
      U : constant String := Upper (Trim (Text));
   begin
      if U = "FORD" then return Ford;
      elsif U = "CHEVROLET" then return Chevrolet;
      elsif U = "DODGE" then return Dodge;
      elsif U = "GMC" then return GMC;
      elsif U = "GENERALDYNAMICS" then return GeneralDynamics;
      elsif U = "GRUMMAN" then return Grumman;
      elsif U = "LOCKHEED" then return Lockheed;
      elsif U = "BOEING" then return Boeing;
      elsif U = "NAVALGROUP" then return NavalGroup;
      elsif U = "THYSSENKRUPP" then return ThyssenKrupp;
      elsif U = "HARLEY" then return Harley;
      elsif U = "HONDA" then return Honda;
      elsif U = "YAMAHA" then return Yamaha;
      elsif U = "DUCATI" then return Ducati;
      else return None_Make;
      end if;
   end To_Make;

   function To_Model (Text : String) return Model_Type is
      U : constant String := Upper (Trim (Text));
   begin
      if U = "EXPEDITION" then return Expedition;
      elsif U = "RAPTOR" then return Raptor;
      elsif U = "CAMARO" then return Camaro;
      elsif U = "PICKUP" then return Pickup;
      elsif U = "STINGRAY" then return Stingray;
      elsif U = "CHARGER" then return Charger;
      elsif U = "RAM" then return Ram;
      elsif U = "DEVIL" then return Devil;
      elsif U = "F16" or U = "F-16" then return F16;
      elsif U = "COMMERCIAL" then return Commercial;
      elsif U = "F35" then return F35;
      elsif U = "747" or U = "B747" then return B747;
      elsif U = "VIRGINIA" then return Virginia;
      elsif U = "TYPHOON" then return Typhoon;
      elsif U = "TRIDENT" then return Trident;
      elsif U = "SPORTSTER" then return Sportster;
      elsif U = "SHADOW" then return Shadow;
      elsif U = "R1" then return R1;
      elsif U = "PANIGALE" then return Panigale;
      else return None_Model;
      end if;
   end To_Model;

   function To_Color (Text : String) return Color_Type is
      U : constant String := Upper (Trim (Text));
   begin
      if U = "BLUE" then return Blue;
      elsif U = "RED" then return Red;
      elsif U = "WHITE" then return White;
      elsif U = "ORANGE" then return Orange;
      elsif U = "BLACK" then return Black;
      elsif U = "SILVER" then return Silver;
      elsif U = "CAMO" then return Camo;
      elsif U = "YELLOW" then return Yellow;
      elsif U = "GREEN" then return Green;
      else return None_Color;
      end if;
   end To_Color;

   -- CONVERSION FUNCTION: Convert text to number
   -- Example: "44" → 44, "2" → 2
   function To_Number (Text : String) return Natural is
      T : constant String := Trim (Text);
      Result : Natural := 0;
   begin
      -- Process each digit character
      for I in T'Range loop
         if T (I) >= '0' and T (I) <= '9' then
            -- Build number digit by digit (like "44" = 4*10 + 4)
            Result := Result * 10 + (Character'Pos (T (I)) - Character'Pos ('0'));
         end if;
      end loop;
      return Result;
   end To_Number;

   -- LOGIC FUNCTION: Determine vehicle type from manufacturer
   -- Example: Ford → Car_Type, Grumman → Plane_Type
   function Vehicle_Type (Make : Manufacturer) return Vehicle_Kind is
   begin
      case Make is
         when Ford | Chevrolet | Dodge | GMC => return Car_Type;
         when GeneralDynamics | Grumman | Lockheed | Boeing => return Plane_Type;
         when NavalGroup | ThyssenKrupp => return Submarine_Type;
         when Harley | Honda | Yamaha | Ducati => return Motorcycle_Type;
         when None_Make => return None_Vehicle;
      end case;
   end Vehicle_Type;

   procedure Set_Name (Buffer : out Name_Buffer; Text : String) is
      T : constant String := Trim (Text);
      Len : constant Natural := Natural'Min (T'Length, Max_Name_Length);
   begin
      Buffer.Length := Len;
      for I in 1 .. Len loop
         Buffer.Data (I) := T (T'First + I - 1);
      end loop;
      for I in Len + 1 .. Max_Name_Length loop
         Buffer.Data (I) := ' ';
      end loop;
   end Set_Name;

   function To_String (Value : Name_Buffer) return String is
   begin
      if Value.Length = 0 then
         return "";
      else
         return Value.Data (1 .. Value.Length);
      end if;
   end To_String;

   -- MAIN FUNCTION: Read employee file line by line
   -- This is the BEGINNER-FRIENDLY approach!
   -- Instead of reading character-by-character, we:
   --   1. Read a whole line at once with Get_Line
   --   2. Split it into words
   --   3. Process the words
   procedure Read_Employees
     (File_Name : String;
      Process_Employee : not null access procedure (Current_Employee : in Employee_Record))
   is
      File : File_Type;              -- The file we're reading
      Line : String (1 .. 200);      -- Buffer for one line (max 200 chars)
      Line_Len : Natural;            -- How many characters we actually read
      Words : Word_List;             -- Array to hold the words from the line
      Word_Count : Natural;          -- How many words we found
      Rec : Employee_Record;         -- The employee record we're building
   begin
      Put_Line ("Reading " & File_Name);
      Open (File, In_File, File_Name);
      
      -- Process file one line at a time (SIMPLE!)
      while not End_Of_File (File) loop
         Get_Line (File, Line, Line_Len);  -- Read entire line
         
         -- Skip empty lines
         if Line_Len = 0 then
            goto Next_Line;
         end if;
         
         -- Split line into words (KEY STEP!)
         Split_Words (Line (1 .. Line_Len), Words, Word_Count);
         
         -- Need at least 3 words to be useful
         if Word_Count < 3 then
            goto Next_Line;
         end if;
         
         -- Process the words we found
         declare
            Job : constant Job_Type := To_Job (Get_Word (Words, 2));  -- Word 2 is job type
         begin
            -- CASE 1: This is an employee line
            -- Format: Name Job Age [Make Model Count Color]
            if Job /= None then
               -- Get employee info from words
               Set_Name (Rec.Name, Get_Word (Words, 1));  -- Word 1 = name
               Rec.Job := Job;                             -- Word 2 = job (already converted)
               Rec.Age := To_Number (Get_Word (Words, 3)); -- Word 3 = age
               
               -- Check if this line also has vehicle info
               if Word_Count >= 7 then
                  -- Employee with vehicle: Name Job Age Make Model Count Color
                  Rec.Vehicle_Make := To_Make (Get_Word (Words, 4));
                  Rec.Vehicle_Model := To_Model (Get_Word (Words, 5));
                  Rec.Vehicle_Count := To_Number (Get_Word (Words, 6));
                  Rec.Vehicle_Color := To_Color (Get_Word (Words, 7));
                  Rec.Vehicle_Category := Vehicle_Type (Rec.Vehicle_Make);
               else
                  -- Employee with no vehicle
                  Rec.Vehicle_Make := None_Make;
                  Rec.Vehicle_Model := None_Model;
                  Rec.Vehicle_Count := 0;
                  Rec.Vehicle_Color := None_Color;
                  Rec.Vehicle_Category := None_Vehicle;
               end if;
               
               -- Send this record to be processed
               Process_Employee.all (Rec);
               

             -- CASE 2: This is just a vehicle line (no employee)
             -- Format: Make Model Count Color
             elsif Word_Count >= 4 then
                Rec.Name.Length := 0;  -- No name
                Rec.Job := None;       -- No job
                Rec.Age := 0;          -- No age
                
                -- Get vehicle info from words
                Rec.Vehicle_Make := To_Make (Get_Word (Words, 1));     -- Word 1 = make
                Rec.Vehicle_Model := To_Model (Get_Word (Words, 2));   -- Word 2 = model
                Rec.Vehicle_Count := To_Number (Get_Word (Words, 3));  -- Word 3 = count
                Rec.Vehicle_Color := To_Color (Get_Word (Words, 4));   -- Word 4 = color
                Rec.Vehicle_Category := Vehicle_Type (Rec.Vehicle_Make);
                
                -- Send this vehicle record to be processed
                Process_Employee.all (Rec);
             end if;
          end;         <<Next_Line>>
         null;
      end loop;
      
      Close (File);
      
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Read_Employees;

end ProcessFile;
