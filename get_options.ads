--------------------------------------------------------------------------------
--  Get_Options								      --
--  									      --
--  Copyright © 2015 Sébastien Bianti					      --
--  									      --
--  This program is free software; you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License version 2 as	      --
--  published by the Free Software Foundation.				      --
--  									      --
--  This program is distributed in the hope that it will be useful,	      --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of	      --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	      --
--  GNU General Public License for more details.			      --
--  									      --
--  You should have received a copy of the GNU General Public License	      --
--  along with this program; if not, write to the Free Software	              --
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA --
--------------------------------------------------------------------------------


with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Command_Line;

generic
   type Option_Title is (<>);
package Get_Options is
   use Ada.Characters, Ada.Strings.Unbounded, Ada;

-- Simple dash (often use to mean reading from stdin):
   Null_Short_Name : constant Character := Latin_1.Nul;

-- Only a long name for this option:
   No_Short_Name   : constant Character := ' ';

   type Access_String is access String;

   type Option_Result is record
      Is_Set: Boolean := False;
      Value: Access_String;
   end record;

   type Option_Result_Array is array (Option_Title) of Option_Result;

   type Value_Needed_Type is (Yes, Optional, No);
   type Option_Setting is record
      Short_Name: Character;
      Needs_Value: Value_Needed_Type;

-- These values are needed to print a fancy help menu:
      Value_Form: Unbounded_String; -- an example of value accepted
      Short_Description: Unbounded_String;
   end record;

   type Option_Setting_Array is array (Option_Title) of Option_Setting;

   type Option_Multisetable is array (Option_Title) of Boolean;

   type US_Array_Type is array (Natural range <>) of Unbounded_String;

   type Help_Section_Array is array (Option_Title) of Unbounded_String;

   function Parse(Option: in Option_Setting_Array;
		  Help_Header, Help_Footer: in String;
		  Multiset: in Option_Multisetable := (others => False))
		 return Option_Result_Array;

   function Parse(Option: in Option_Setting_Array;
		  Help_Header, Help_Footer: in String;
		  Help_Sections: in Help_Section_Array;
		  Multiset: in Option_Multisetable := (others => False))
		 return Option_Result_Array;

   function Get_Number_Values(Result: in Option_Result) return Natural;

   function Get_Value(Result: in Option_Result; Number: Positive) return String;

   function Get_Values(Result: in Option_Result) return US_Array_Type;
   
   function No_Short_Option(Short_Description: in String) return Option_Setting;

   function No_Option_Value(Car: in Character;
			    Short_Description: in String) return Option_Setting;

-- Renamings from Ada.Strings.Unbounded:
   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

   Null_Unbounded_String: Unbounded_String
     renames Ada.Strings.Unbounded.Null_Unbounded_String;

   function To_US(Source: in String) return Unbounded_String
     renames To_Unbounded_String;

   function To_Str(Source: in Unbounded_String) return String renames To_String;

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
   End_Of_Program_With_Help_Menu,
   Redundant_Short_Option_Name_Error: exception;
end Get_Options;
