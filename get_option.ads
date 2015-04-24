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

   type Options_Result_Array is array (Option_Title) of Option_Result;

   procedure Set_Option(title      : in Option_Title;
			Short_Name : in Character;
			Long_Name  : in String);

   function Get_Options return Options_Result_Array;

end Get_Option;
