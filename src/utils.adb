-- utils.adb
-- Implementation of utility functions

package body Utils is

   -- Trim leading and trailing spaces
   function Trim (S : String) return String is
      First_Position : Natural := S'First;
      Last_Position  : Natural := S'Last;
   begin
      -- Find first non-space
      while First_Position <= Last_Position and then S (First_Position) = ' ' loop
         First_Position := First_Position + 1;
      end loop;

      -- Find last non-space
      while Last_Position >= First_Position and then S (Last_Position) = ' ' loop
         Last_Position := Last_Position - 1;
      end loop;

      if First_Position > Last_Position then
         return "";
      else
         return S (First_Position .. Last_Position);
      end if;
   end Trim;

   -- Copy string into fixed-length name string
   procedure Copy_String (Source : String; Target : out Name_String; Length : out Natural) is
      Trimmed : constant String := Trim (Source);
   begin
      Length := Natural'Min (Trimmed'Length, Maximum_Name_Length);
      
      -- Copy characters
      for Character_Index in 1 .. Length loop
         Target (Character_Index) := Trimmed (Trimmed'First + Character_Index - 1);
      end loop;

      -- Pad with spaces
      for Character_Index in Length + 1 .. Maximum_Name_Length loop
         Target (Character_Index) := ' ';
      end loop;
   end Copy_String;

   -- Copy string into fixed-length model string
   procedure Copy_Model (Source : String; Target : out Model_String; Length : out Natural) is
      Trimmed : constant String := Trim (Source);
   begin
      Length := Natural'Min (Trimmed'Length, Maximum_Model_Length);
      
      -- Copy characters
      for Character_Index in 1 .. Length loop
         Target (Character_Index) := Trimmed (Trimmed'First + Character_Index - 1);
      end loop;

      -- Pad with spaces
      for Character_Index in Length + 1 .. Maximum_Model_Length loop
         Target (Character_Index) := ' ';
      end loop;
   end Copy_Model;

   -- Compare two names for sorting (alphabetical)
   function Name_Less_Than (Left : Name_String; Left_Len : Natural;
                           Right : Name_String; Right_Len : Natural) return Boolean is
   begin
      -- Compare character by character
      for I in 1 .. Natural'Min (Left_Len, Right_Len) loop
         if Left (I) < Right (I) then
            return True;
         elsif Left (I) > Right (I) then
            return False;
         end if;
      end loop;

      -- If all characters match, shorter name comes first
      return Left_Len < Right_Len;
   end Name_Less_Than;

   -- Simple string to integer conversion
   function To_Integer (S : String) return Integer is
      Trimmed : constant String := Trim (S);
      Result  : Integer := 0;
      Negative : Boolean := False;
      Start_Pos : Natural := Trimmed'First;
   begin
      if Trimmed'Length = 0 then
         return 0;
      end if;

      -- Check for negative sign
      if Trimmed (Start_Pos) = '-' then
         Negative := True;
         Start_Pos := Start_Pos + 1;
      end if;

      -- Parse digits
      for I in Start_Pos .. Trimmed'Last loop
         if Trimmed (I) >= '0' and Trimmed (I) <= '9' then
            Result := Result * 10 + (Character'Pos (Trimmed (I)) - Character'Pos ('0'));
         end if;
      end loop;

      if Negative then
         return -Result;
      else
         return Result;
      end if;
   end To_Integer;

   -- Convert string to uppercase for comparison
   function To_Upper (S : String) return String is
      Result : String (S'Range);
   begin
      for I in S'Range loop
         if S (I) >= 'a' and S (I) <= 'z' then
            Result (I) := Character'Val (Character'Pos (S (I)) - 32);
         else
            Result (I) := S (I);
         end if;
      end loop;
      return Result;
   end To_Upper;

   -- Convert string to Job_Type
   function To_Job_Type (S : String) return Job_Type is
      Upper : constant String := To_Upper (Trim (S));
   begin
      if Upper = "ACCOUNTANT" then return Accountant;
      elsif Upper = "ANALYSIST" then return Analysist;
      elsif Upper = "MANAGER" then return Manager;
      elsif Upper = "PROGRAMMER" then return Programmer;
      elsif Upper = "SALES" then return Sales;
      else return None;
      end if;
   end To_Job_Type;

   -- Convert string to Manufacturer
   function To_Manufacturer (S : String) return Manufacturer is
      Upper : constant String := To_Upper (Trim (S));
   begin
      if Upper = "FORD" then return Ford;
      elsif Upper = "CHEVROLET" then return Chevrolet;
      elsif Upper = "DODGE" then return Dodge;
      elsif Upper = "GMC" then return GMC;
      elsif Upper = "GENERALDYNAMICS" then return GeneralDynamics;
      elsif Upper = "GRUMMAN" then return Grumman;
      elsif Upper = "LOCKHEED" then return Lockheed;
      elsif Upper = "BOEING" then return Boeing;
      elsif Upper = "NAVALGROUP" then return NavalGroup;
      elsif Upper = "THYSSENKRUPP" then return ThyssenKrupp;
      elsif Upper = "HARLEY" then return Harley;
      elsif Upper = "HONDA" then return Honda;
      elsif Upper = "YAMAHA" then return Yamaha;
      elsif Upper = "DUCATI" then return Ducati;
      else return None_Make;
      end if;
   end To_Manufacturer;

   -- Convert string to Model_Type
   function To_Model_Type (S : String) return Model_Type is
      Upper : constant String := To_Upper (Trim (S));
   begin
      if Upper = "EXPEDITION" then return Expedition;
      elsif Upper = "RAPTOR" then return Raptor;
      elsif Upper = "CAMARO" then return Camaro;
      elsif Upper = "PICKUP" then return Pickup;
      elsif Upper = "STINGRAY" then return Stingray;
      elsif Upper = "CHARGER" then return Charger;
      elsif Upper = "RAM" then return Ram;
      elsif Upper = "DEVIL" then return Devil;
      elsif Upper = "F16" or Upper = "F-16" then return F16;
      elsif Upper = "COMMERCIAL" then return Commercial;
      elsif Upper = "F35" then return F35;
      elsif Upper = "B747" or Upper = "747" then return B747;
      elsif Upper = "VIRGINIA" then return Virginia;
      elsif Upper = "TYPHOON" then return Typhoon;
      elsif Upper = "TRIDENT" then return Trident;
      elsif Upper = "SPORTSTER" then return Sportster;
      elsif Upper = "SHADOW" then return Shadow;
      elsif Upper = "R1" then return R1;
      elsif Upper = "PANIGALE" then return Panigale;
      else return None_Model;
      end if;
   end To_Model_Type;

   -- Convert string to Color_Type
   function To_Color_Type (S : String) return Color_Type is
      Upper : constant String := To_Upper (Trim (S));
   begin
      if Upper = "BLUE" then return Blue;
      elsif Upper = "RED" then return Red;
      elsif Upper = "WHITE" then return White;
      elsif Upper = "ORANGE" then return Orange;
      elsif Upper = "BLACK" then return Black;
      elsif Upper = "SILVER" then return Silver;
      elsif Upper = "CAMO" then return Camo;
      elsif Upper = "YELLOW" then return Yellow;
      elsif Upper = "GREEN" then return Green;
      else return None_Color;
      end if;
   end To_Color_Type;

   -- Convert string to Engine_Type
   function To_Engine_Type (S : String) return Engine_Type is
      Upper : constant String := To_Upper (Trim (S));
   begin
      if Upper = "JET" then return Jet;
      elsif Upper = "BLADE" then return Blade;
      elsif Upper = "NUCLEAR" then return Nuclear;
      elsif Upper = "DIESEL" then return Diesel;
      elsif Upper = "V_TWIN"  then return V_Twin;
      elsif Upper = "INLINE_FOUR" then return Inline_Four;
      else return None_Engine;
      end if;
   end To_Engine_Type;

   -- Convert Job_Type to string
   function Job_Type_Image (J : Job_Type) return String is
   begin
      case J is
         when None       => return "None";
         when Accountant => return "Accountant";
         when Analysist  => return "Analysist";
         when Manager    => return "Manager";
         when Programmer => return "Programmer";
         when Sales      => return "Sales";
      end case;
   end Job_Type_Image;

   -- Convert Manufacturer to string
   function Manufacturer_Image (M : Manufacturer) return String is
   begin
      case M is
         when None_Make       => return "None";
         when Ford            => return "Ford";
         when Chevrolet       => return "Chevrolet";
         when Dodge           => return "Dodge";
         when GMC             => return "GMC";
         when GeneralDynamics => return "GeneralDynamics";
         when Grumman         => return "Grumman";
         when Lockheed        => return "Lockheed";
         when Boeing          => return "Boeing";
         when NavalGroup      => return "NavalGroup";
         when ThyssenKrupp    => return "ThyssenKrupp";
         when Harley          => return "Harley";
         when Honda           => return "Honda";
         when Yamaha          => return "Yamaha";
         when Ducati          => return "Ducati";
      end case;
   end Manufacturer_Image;

   -- Convert Model_Type to string
   function Model_Image (M : Model_Type) return String is
   begin
      case M is
         when None_Model => return "None";
         when Expedition => return "Expedition";
         when Raptor     => return "Raptor";
         when Camaro     => return "Camaro";
         when Pickup     => return "Pickup";
         when Stingray   => return "Stingray";
         when Charger    => return "Charger";
         when Ram        => return "Ram";
         when Devil      => return "Devil";
         when F16        => return "F-16";
         when Commercial => return "Commercial";
         when F35        => return "F35";
         when B747       => return "B747";
         when Virginia   => return "Virginia";
         when Typhoon    => return "Typhoon";
         when Trident    => return "Trident";
         when Sportster  => return "Sportster";
         when Shadow     => return "Shadow";
         when R1         => return "R1";
         when Panigale   => return "Panigale";
      end case;
   end Model_Image;

   -- Convert Color_Type to string
   function Color_Image (C : Color_Type) return String is
   begin
      case C is
         when None_Color => return "None";
         when Blue       => return "Blue";
         when Red        => return "Red";
         when White      => return "White";
         when Orange     => return "Orange";
         when Black      => return "Black";
         when Silver     => return "Silver";
         when Camo       => return "Camo";
         when Yellow     => return "Yellow";
         when Green      => return "Green";
      end case;
   end Color_Image;

   -- Convert Engine_Type to string
   function Engine_Type_Image (E : Engine_Type) return String is
   begin
      case E is
         when None_Engine => return "None";
         when Jet         => return "Jet";
         when Blade       => return "Blade";
         when Nuclear     => return "Nuclear";
         when Diesel      => return "Diesel";
         when V_Twin      => return "V-Twin";
         when Inline_Four => return "Inline-Four";
      end case;
   end Engine_Type_Image;

end Utils;
