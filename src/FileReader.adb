-- FileReader.adb
with Ada.Text_IO; use Ada.Text_IO;
package body FileReader is
   -- Split a line into individual words,aces and tabs as delimiters.
   procedure Get_Words(Line :String; Words :out Word_Array;Count :out Natural) is
      Current_Position :Natural:= Line'First;
      Word_Start:Natural;
   begin
   Count := 0;
   while Current_Position<=Line'Last and Count< 10 loop
      -- Skip whitespace (spaces and tabs)
      while Current_Position<=Line'Last and then 
            (Line (Current_Position) = ' ' or else Line (Current_Position)=Character'Val (9)) loop
         Current_Position:=Current_Position + 1;
      end loop;
      
      exit when Current_Position > Line'Last;
      Word_Start := Current_Position;
      -- Find end of word (next whitespace)
      while Current_Position <= Line'Last and then 
         (Line (Current_Position) /= ' ' and then Line (Current_Position) /= Character'Val (9)) loop
         Current_Position := Current_Position + 1;
      end loop;
   
      -- Store word in array (pad with spaces to fill 50 chars)
      Count := Count + 1;
      Words (Count) := (others => ' ');
      declare
         Word_Length : constant Natural := Natural'Min (Current_Position - Word_Start, 50);
      begin
      Words (Count) (1 .. Word_Length) := Line (Word_Start .. Word_Start + Word_Length - 1);
      end;
   end loop;
end Get_Words;
   -- Returns only the actual content withoutpadding.
   function Trim_Word (Word :Word_String) return String is
      Last_Position:Natural:=Word'Last;
   begin
      -- Scan backwards to find last character
      while Last_Position>= Word'First and then Word (Last_Position) = ' ' loop
       Last_Position := Last_Position - 1;
      end loop;
      if Last_Position<Word'First then
         return "";  -- Word is all spaces
      else
         return Word (Word'First ..Last_Position);
      end if;
   end Trim_Word;

   procedure Read_File
     (File_Name : String; 
      On_Employee : access procedure(Name : String; Job : Job_Type; Age : Natural);
      On_Vehicle : access procedure (Make :Manufacturer; Model : Model_Type; Count : Natural; Color : Color_Type)) is  
      Input_File : File_Type;
      Line : String (1 .. 200);
      Last_Character : Natural;
      --all words from entire file
      All_Tokens : array (1 .. 1000) of Word_String := (others => (others => ' '));
      Token_Count : Natural := 0;
      Current_Token : Natural := 1;
   begin
      Put_Line ("Reading " & File_Name);
      Open (Input_File, In_File, File_Name);
      --Read al lines and collect all tokens
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Line, Last_Character);
         if Last_Character > 0 then
            declare
               Line_Words : Word_Array;
               Line_Word_Count : Natural;
            begin
               Get_Words (Line (1 .. Last_Character), Line_Words, Line_Word_Count);
               
               -- Append words from this line to array
               for I in 1 .. Line_Word_Count loop
                  Token_Count := Token_Count + 1;
                  All_Tokens (Token_Count) := Line_Words (I);
               end loop;
            end;
         end if;
      end loop;
      Close (Input_File);
      while Current_Token <= Token_Count loop

         -- Try parsing as employee Name, Joband Age
         if Current_Token + 2 <= Token_Count then
            declare
               Test_Job : Job_Type;
               Last_Pos : Positive;
            begin
               -- Check ifs a valid job type
               Job_Type_IO.Get (Trim_Word (All_Tokens (Current_Token + 1)), Test_Job, Last_Pos);
               if Test_Job /= None then
                  On_Employee (Trim_Word (All_Tokens (Current_Token)), 
                             Test_Job, 
                             Natural'Value (Trim_Word (All_Tokens (Current_Token + 2))));
                  Current_Token := Current_Token + 3;
                  goto Next_Record;
               end if;
            exception
               when others => null;  -- Not an employee, try vehicle
            end;
         end if;
         -- Try vehicle Make, Model ,Count, Col
         if Current_Token +3<= Token_Count then
            declare
               V_Make:Manufacturer;
               V_Model:Model_Type;
               V_Color :Color_Type;
               Make_Last, Model_Last, Color_Last : Positive;
            begin
               Manufacturer_IO.Get (Trim_Word(All_Tokens(Current_Token)), V_Make, Make_Last);
               Model_Type_IO.Get (Trim_Word(All_Tokens(Current_Token + 1)), V_Model, Model_Last);
               Color_Type_IO.Get (Trim_Word(All_Tokens(Current_Token + 3)), V_Color, Color_Last);
               
               On_Vehicle (V_Make, V_Model, 
                         Natural'Value (Trim_Word (All_Tokens (Current_Token + 2))), 
                         V_Color);
               Current_Token := Current_Token + 4;
               goto Next_Record;
            exception
               when others=>null;
            end;
         end if;
         -- match employee or vehicle
         Current_Token := Current_Token + 1;
         <<Next_Record>>
      end loop;
      
   end Read_File;

end FileReader;
