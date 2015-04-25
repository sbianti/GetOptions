with Ada.Characters.Latin_1;

generic
   type Option_Title is (<>);
package Get_Option is
   use Ada;

   Nul_Short_Name : constant Character := Characters.Latin_1.Nul;

   type Option_Result is record
      Set: Boolean := False;
      Value: access String;
   end record;

   type Option_Result_Array is array (Option_Title) of Option_Result;

   type Value_Presence is (Needed, Optional, No);
   type Option_Setting is record
      Short_Name: Character;
      Value: Value_Presence;
   end record;

   type Option_Setting_Array is array (Option_Title) of Option_Setting;

   procedure Set_Options(Option_Settings: in Option_Setting_Array);

   function Get_Options return Option_Result_Array;

end Get_Option;
