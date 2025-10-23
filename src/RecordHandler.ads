--RecordHandler.ads
with Enums; use Enums;

package RecordHandler is
   procedure Handle_Employee (Name : String; Job : Job_Type; Age : Natural);

   procedure Handle_Vehicle (Make : Manufacturer; Model : Model_Type; Count : Natural; Color : Color_Type);
      function Process_File (Filename : String) return Boolean;

end RecordHandler;
