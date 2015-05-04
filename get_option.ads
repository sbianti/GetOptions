with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Command_Line;

generic
   type Option_Title is (<>);
package Get_Option is
   use Ada.Characters, Ada.Strings.Unbounded, Ada;

   Null_Short_Name : constant Character := Latin_1.Nul;

   type Option_Result is record
      Set: Boolean := False;
      Value: access String;
   end record;

   type Option_Result_Array is array (Option_Title) of Option_Result;

   type Value_Needed_Type is (Yes, Optional, No);
   type Option_Setting is record
      Short_Name: Character;
      Needs_Value: Value_Needed_Type;
-- These values are needed to print a fancy help menu:
      Value_Form: Unbounded_String; -- an example of value accepted
      Short_Explanation: Unbounded_String;
   end record;

   type Option_Setting_Array is array (Option_Title) of Option_Setting;

   function Get_Options(Option: in Option_Setting_Array;
			Help_Header, Help_Footer: in String)
		       return Option_Result_Array;

   function To_US(Source : in String) return Unbounded_String
     renames To_Unbounded_String;

-- Renamings from Ada.Command_Line:
   function Argument_Count return Natural renames Command_Line.Argument_Count;
   function Argument(Number : Positive) return String
     renames Command_Line.Argument;
   function Command_Name return String renames Command_Line.Command_Name;
   procedure Set_Exit_Status(Code: Command_Line.Exit_Status)
     renames Command_Line.Set_Exit_Status;
   subtype Exit_Status is Command_Line.Exit_Status;

   Parsing_Error,
   Bad_Grouped_Option_Error,
   End_Of_Program_With_Help_Menu: exception;
end Get_Option;
