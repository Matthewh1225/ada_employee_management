-- Types.adb
package body Types is
   function Name_Less_Than (Left : Name_String; Left_Length : Natural; Right : Name_String; Right_Length : Natural) return Boolean is
   begin
    return Left (1..Left_Length) <Right (1 .. Right_Length);
   end Name_Less_Than;
end Types;
