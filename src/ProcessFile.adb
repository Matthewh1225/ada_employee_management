with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;

package body ProcessFile is

   Max_Token_Length : constant := 40;

   type Token_Buffer is record
      Length : Natural range 0 .. Max_Token_Length := 0;
      Data   : String (1 .. Max_Token_Length)      := (others => ' ');
   end record;

   function Token_To_String (Value : Token_Buffer) return String is
   begin
      if Value.Length = 0 then
         return "";
      else
         return Value.Data (1 .. Value.Length);
      end if;
   end Token_To_String;

   function Is_Separator (C : Character) return Boolean is
     (C = ' ' or else C = HT or else C = CR or else C = LF);

   procedure Read_Token
     (File        : in Ada.Text_IO.File_Type;
      Token       : out Token_Buffer;
      End_Reached : out Boolean) is
      Ch               : Character;
      Reached_Line_End : Boolean := False;
   begin
      Token.Length := 0;

      loop
         if Ada.Text_IO.End_Of_File (File) then
            End_Reached := True;
            return;
         end if;

         Ada.Text_IO.Get (File, Ch);

         exit when not Is_Separator (Ch);
      end loop;

      Token.Length := 1;
      Token.Data (1) := Ch;

      loop
         exit when Ada.Text_IO.End_Of_File (File);

         declare
            Next        : Character;
            End_Of_Line : Boolean;
         begin
            Ada.Text_IO.Look_Ahead (File, Next, End_Of_Line);

            if End_Of_Line then
               Reached_Line_End := True;
               exit;
            elsif Is_Separator (Next) then
               exit;
            else
               Ada.Text_IO.Get (File, Next);

               if Token.Length < Max_Token_Length then
                  Token.Length := Token.Length + 1;
                  Token.Data (Token.Length) := Next;
               end if;
            end if;
         end;
      end loop;

      if Reached_Line_End and then not Ada.Text_IO.End_Of_File (File) then
         Ada.Text_IO.Skip_Line (File);
      end if;

      End_Reached := False;
   end Read_Token;

   function Normalize (Text : String) return String is
      Trimmed : constant String := Trim (Text, Both);
   begin
      return Trimmed;
   end Normalize;

   function To_Upper_Clean (Text : String) return String is
      Clean : constant String := Normalize (Text);
   begin
      return To_Upper (Clean);
   end To_Upper_Clean;

   function To_Job (Job_String : String) return Job_Type is
      Value : constant String := To_Upper_Clean (Job_String);
   begin
      if Value = "MANAGER" then
         return Manager;
      elsif Value = "SALES" then
         return Sales;
      elsif Value = "ANALYSIST" then
         return Analysist;
      elsif Value = "PROGRAMMER" then
         return Programmer;
      elsif Value = "ACCOUNTANT" then
         return Accountant;
      else
         raise Constraint_Error with "Invalid job type: " & Job_String;
      end if;
   end To_Job;

   function To_Manufacturer (Text : String) return Manufacturer is
      Value : constant String := To_Upper_Clean (Text);
   begin
      if Value = "FORD" then
         return Ford;
      elsif Value = "CHEVROLET" then
         return Chevrolet;
      elsif Value = "DODGE" then
         return Dodge;
      elsif Value = "GMC" then
         return GMC;
      elsif Value = "GENERALDYNAMICS" then
         return GeneralDynamics;
      elsif Value = "GRUMMAN" then
         return Grumman;
      elsif Value = "LOCKHEED" then
         return Lockheed;
      elsif Value = "BOEING" then
         return Boeing;
      elsif Value = "NAVALGROUP" then
         return NavalGroup;
      elsif Value = "THYSSENKRUPP" then
         return ThyssenKrupp;
      elsif Value = "HARLEY" then
         return Harley;
      elsif Value = "HONDA" then
         return Honda;
      elsif Value = "YAMAHA" then
         return Yamaha;
      elsif Value = "DUCATI" then
         return Ducati;
      else
         raise Constraint_Error with "Invalid manufacturer: " & Text;
      end if;
   end To_Manufacturer;

   function Remove_Character (Text : String; Character_To_Remove : Character) return String is
      Result : String (1 .. Text'Length);
      Length : Natural := 0;
   begin
      for Pos in Text'Range loop
         if Text (Pos) /= Character_To_Remove then
            Length := Length + 1;
            Result (Length) := Text (Pos);
         end if;
      end loop;

      if Length = 0 then
         return "";
      else
         return Result (1 .. Length);
      end if;
   end Remove_Character;

   function To_Model (Text : String) return Model_Type is
      Raw      : constant String := To_Upper_Clean (Text);
      Adjusted : constant String := Remove_Character (Raw, '-');
   begin
      if Adjusted = "EXPEDITION" then
         return Expedition;
      elsif Adjusted = "RAPTOR" then
         return Raptor;
      elsif Adjusted = "CAMARO" then
         return Camaro;
      elsif Adjusted = "PICKUP" then
         return Pickup;
      elsif Adjusted = "STINGRAY" then
         return Stingray;
      elsif Adjusted = "CHARGER" then
         return Charger;
      elsif Adjusted = "RAM" then
         return Ram;
      elsif Adjusted = "DEVIL" then
         return Devil;
      elsif Adjusted = "F16" then
         return F16;
      elsif Adjusted = "COMMERCIAL" then
         return Commercial;
      elsif Adjusted = "F35" then
         return F35;
      elsif Adjusted = "747" or else Adjusted = "B747" then
         return B747;
      elsif Adjusted = "VIRGINIA" then
         return Virginia;
      elsif Adjusted = "TYPHOON" then
         return Typhoon;
      elsif Adjusted = "TRIDENT" then
         return Trident;
      elsif Adjusted = "SPORTSTER" then
         return Sportster;
      elsif Adjusted = "SHADOW" then
         return Shadow;
      elsif Adjusted = "R1" then
         return R1;
      elsif Adjusted = "PANIGALE" then
         return Panigale;
      else
         raise Constraint_Error with "Invalid model: " & Text;
      end if;
   end To_Model;

   function To_Color (Text : String) return Color_Type is
      Value : constant String := To_Upper_Clean (Text);
   begin
      if Value = "BLUE" then
         return Blue;
      elsif Value = "RED" then
         return Red;
      elsif Value = "WHITE" then
         return White;
      elsif Value = "ORANGE" then
         return Orange;
      elsif Value = "BLACK" then
         return Black;
      elsif Value = "SILVER" then
         return Silver;
      elsif Value = "CAMO" then
         return Camo;
      elsif Value = "YELLOW" then
         return Yellow;
      elsif Value = "GREEN" then
         return Green;
      else
         raise Constraint_Error with "Invalid color: " & Text;
      end if;
   end To_Color;

   function Determine_Category (Make : Manufacturer) return Vehicle_Kind is
   begin
      case Make is
         when Ford | Chevrolet | Dodge | GMC =>
            return Car_Type;
         when GeneralDynamics | Grumman | Lockheed | Boeing =>
            return Plane_Type;
         when NavalGroup | ThyssenKrupp =>
            return Submarine_Type;
         when Harley | Honda | Yamaha | Ducati =>
            return Motorcycle_Type;
         when None_Make =>
            return None_Vehicle;
      end case;
   end Determine_Category;

   function To_Natural (Text : String) return Natural is
      Value : Integer;
   begin
      Value := Integer'Value (Normalize (Text));
      if Value < 0 then
         raise Constraint_Error with "Negative number encountered: " & Text;
      end if;
      return Natural (Value);
   end To_Natural;

   procedure Set_Name (Target : out Name_Buffer; Source : String) is
      Clean    : constant String := Normalize (Source);
      Length   : constant Natural := Clean'Length;
      Actual   : constant Natural := Natural'Min (Length, Max_Name_Length);
   begin
      Target.Length := Actual;

      if Actual > 0 then
         for Index in 1 .. Actual loop
            Target.Data (Index) := Clean (Clean'First + Index - 1);
         end loop;
      end if;

      if Actual < Max_Name_Length then
         for Index in Actual + 1 .. Max_Name_Length loop
            Target.Data (Index) := ' ';
         end loop;
      end if;
   end Set_Name;

   function To_String (Value : Name_Buffer) return String is
   begin
      if Value.Length = 0 then
         return "";
      else
         return Value.Data (1 .. Value.Length);
      end if;
   end To_String;

   procedure Read_Employees
     (File_Name : String;
      Process_Employee :
        not null access procedure (Current_Employee : in Employee_Record)) is

      File_Handle   : Ada.Text_IO.File_Type;
      Token         : Token_Buffer;
      End_File      : Boolean;
      Pending_Token : Token_Buffer;
      Has_Pending   : Boolean := False;

      procedure Get_Token (Value : out Token_Buffer; End_Reached : out Boolean) is
      begin
         if Has_Pending then
            Value      := Pending_Token;
            Has_Pending := False;
            End_Reached := False;
         else
            Read_Token (File_Handle, Value, End_Reached);
         end if;
      end Get_Token;
   begin
      Ada.Text_IO.Open (File_Handle, Ada.Text_IO.In_File, File_Name);

      loop
         Get_Token (Token, End_File);
         exit when End_File;

         declare
            Name_Text   : constant String := Token_To_String (Token);
            Record_Item : Employee_Record;
            Completed   : Boolean := False;
         begin
            Set_Name (Record_Item.Name, Name_Text);

            Get_Token (Token, End_File);
            exit when End_File;
            declare
               Job_Text : constant String := Token_To_String (Token);
            begin
               begin
                  Record_Item.Job := To_Job (Job_Text);
               exception
                  when Constraint_Error =>
                     Completed := True;
                     Record_Item.Job := None;
                     Record_Item.Age := 0;

                     begin
                        Record_Item.Vehicle_Make := To_Manufacturer (Name_Text);
                     exception
                        when Constraint_Error =>
                           Record_Item.Vehicle_Make := None_Make;
                     end;

                     Record_Item.Vehicle_Category :=
                       Determine_Category (Record_Item.Vehicle_Make);

                     begin
                        Record_Item.Vehicle_Model := To_Model (Job_Text);
                     exception
                        when Constraint_Error =>
                           Record_Item.Vehicle_Model := None_Model;
                     end;

                     Get_Token (Token, End_File);
                     exit when End_File;
                     Record_Item.Vehicle_Count :=
                       To_Natural (Token_To_String (Token));

                     Get_Token (Token, End_File);
                     exit when End_File;
                     Record_Item.Vehicle_Color :=
                       To_Color (Token_To_String (Token));

                     Process_Employee.all (Record_Item);
               end;

               if not Completed then
                  Get_Token (Token, End_File);
                  exit when End_File;
                  Record_Item.Age :=
                    Integer'Value (Normalize (Token_To_String (Token)));

                  Get_Token (Token, End_File);
                  exit when End_File;

                  declare
                     Make_Text : constant String := Token_To_String (Token);
                  begin
                     Record_Item.Vehicle_Make     := To_Manufacturer (Make_Text);
                     Record_Item.Vehicle_Category :=
                       Determine_Category (Record_Item.Vehicle_Make);
                  exception
                     when Constraint_Error =>
                        Record_Item.Vehicle_Make     := None_Make;
                        Record_Item.Vehicle_Model    := None_Model;
                        Record_Item.Vehicle_Count    := 0;
                        Record_Item.Vehicle_Color    := None_Color;
                        Record_Item.Vehicle_Category := None_Vehicle;

                        Process_Employee.all (Record_Item);
                        Pending_Token := Token;
                        Has_Pending   := True;
                        Completed     := True;
                  end;

                  if not Completed then
                     Get_Token (Token, End_File);
                     exit when End_File;
                     Record_Item.Vehicle_Model :=
                       To_Model (Token_To_String (Token));

                     Get_Token (Token, End_File);
                     exit when End_File;
                     Record_Item.Vehicle_Count :=
                       To_Natural (Token_To_String (Token));

                     Get_Token (Token, End_File);
                     exit when End_File;
                     Record_Item.Vehicle_Color :=
                       To_Color (Token_To_String (Token));

                     Process_Employee.all (Record_Item);
                  end if;
               end if;
            end;
         end;
      end loop;

      Ada.Text_IO.Close (File_Handle);
   exception
      when others =>
         if Ada.Text_IO.Is_Open (File_Handle) then
            Ada.Text_IO.Close (File_Handle);
         end if;
         raise;
   end Read_Employees;

end ProcessFile;
