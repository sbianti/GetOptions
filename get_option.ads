with Ada.Characters.Latin_1;

generic
   type Option_Title is (<>);
package Get_Option is
   use Ada;

   Null_Short_Name : constant Character := Characters.Latin_1.Nul;

   type Option_Result is record
      Set: Boolean := False;
      Value: access String;
   end record;

   type Option_Result_Array is array (Option_Title) of Option_Result;

   type Value_Presence is (Yes, Optional, No);
   type Option_Setting is record
      Short_Name: Character;
      Needs_Value: Value_Presence;
   end record;

   type Option_Setting_Array is array (Option_Title) of Option_Setting;

   function Get_Options(Option: in Option_Setting_Array)
		       return Option_Result_Array;

   Parsing_Error, Bad_Grouped_Option_Error: exception;
end Get_Option;
