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


with Ada.Text_IO;
with Ada.Command_Line.Remove;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

package body Get_Options is
   use Ada.Text_IO, Ada.Command_Line, Ada.Strings.Unbounded, Ada.Strings.Fixed;

   Already_Warned_For_Multiple_Set: array (Option_Title) of Boolean :=
     (others => False);

   Multiple_Set_Allowed: Option_Multisetable;

   Help_Sections: Help_Section_Array;

   procedure Pl_Error(Item: String) is
   begin
      Put_Line(Standard_Error, "Parsing error: " & Item);
   end;
   pragma Inline(Pl_Error);

   function Long_Name(Opt: Option_Title) return String is
      use Ada.Characters.Handling;
      Name: String := Option_Title'Image(Opt);
   begin
      -- If a long option which name is a reserved word is needed,
      -- just suffix it with «_0»:
      if Name(Name'Length) = '0' and then Name(Name'Length - 1) = '_' then
	 return To_Lower(Name(1..Name'Length - 2));
      else
	 return To_Lower(Name);
      end if;
   end Long_Name;
   pragma Inline(Long_Name);

   function Get_Number_Values(Result: in Option_Result) return Natural is
      use Ada.Characters.Latin_1;
      Number: Natural;
      Value: String := Result.Value.all;
   begin
      if Value = "" then
	 return 0;
      end if;

      Number := 1;

      for I in Value'Range loop
	 if Value(I) = Nul then
	    Number := Number + 1;
	 end if;
      end loop;

      return Number;
   end Get_Number_Values;

   function Get_Value(Result: in Option_Result;
		      Number: in Natural) return String is
      use Ada.Characters.Latin_1;
      Start, Current: Natural;
      Value: String := Result.Value.all;
   begin
      if Number > Get_Number_Values(Result) then
	 return "";
      end if;

      Start := 1;
      Current := 0;
      for I in Value'Range loop
	 if Value(I) = Nul then
	    Current := Current + 1;

	    if Current = Number then
	       return Value(Start..I-1);
	    end if;

	    Start := I + 1;
	 end if;
      end loop;

      return Value(Start..Value'Last);
   end Get_Value;

   function Get_Values(Result: in Option_Result) return US_Array_Type is
      use Ada.Characters.Latin_1;
      Values: Us_Array_Type(1..Get_Number_Values(Result));
      Value: String := Result.Value.all;
      Number, Start: Natural := 1;
   begin
      for I in Value'Range loop
	 if Value(I) = Nul then
	    Values(Number) := To_US(Value(Start..I-1));
	    Start := I + 1;
	    Number := Number + 1;
	 end if;
      end loop;

      Values(Number) := To_US(Value(Start..Value'Last));

      return Values;
   end Get_Values;
   
   function No_Short_Option(Short_Description: in String)
			   return Option_Setting is
   begin
      return (No_Short_Name, No, Null_Unbounded_String,
	      To_US(Short_Description));
   end No_Short_Option;

   function No_Option_Value(Car: in Character; Short_Description: in String)
			   return Option_Setting is
   begin
      return (Car, No, Null_Unbounded_String, To_US(Short_Description));
   end No_Option_Value;

   function Parse(Option: in Option_Setting_Array;
		  Help_Header, Help_Footer: in String;
		  Help_Sections: in Help_Section_Array;
		  Multiset: in Option_Multisetable := (others => False))
		 return Option_Result_Array is
   begin
      Get_Options.Help_Sections := Help_Sections;
      return Parse(Option, Help_Header, Help_Footer, Multiset);
   end Parse;

   function Parse(Option: in Option_Setting_Array;
		  Help_Header, Help_Footer: in String;
		  Multiset: in Option_Multisetable := (others => False))
     
		 return Option_Result_Array is
      Lg: Natural;
      Result: Option_Result_Array;
      Found: Boolean;
      Value: Unbounded_String;
      Access_Value: String_Access;

      procedure Check_Parameter_Value(Title: in Option_Title;
				      Count: in Natural;
				      Value: out Unbounded_String) is
      begin
	 if Argument_Count = Count or else Argument(Count + 1)(1) = '-' then
	    if Option(Title).Needs_Value = Yes then
	       Pl_Error("Option " & Long_Name(Title) & " requires an argument");
	       raise Parsing_Error;
	    end if;
	    Value := Null_Unbounded_String;
	 else
	    Value := To_Unbounded_String(Argument(Count + 1));
	 end if;
      end Check_Parameter_Value;

      function Is_Already_Set(Title: Option_Title) return Boolean is
      begin
	 if not Result(Title).Is_Set then
	    return False;
	 end if;

	 if not Already_Warned_For_Multiple_Set(Title) then
	    Pl_Error("Option " & Option_Title'Image(Title) &
		       " set multiple times");
	    Already_Warned_For_Multiple_Set(Title) := True;
	 end if;

	 return True;
      end Is_Already_Set;

      function Short_Description(Item: in Option_Setting) return String is
      begin
	 return To_String(Item.Short_Description);
      end Short_Description;
      pragma Inline(Short_Description);

      function Value_Form(Item: in Option_Setting) return String is
      begin
	 return To_String(Item.Value_Form);
      end Value_Form;
      pragma Inline(Value_Form);

      procedure Print_Help is
	 -- Help is printed like this:
	 -- -j, --number-of-thread=???  Runs the number of thread specified.
	 Max_Width: Ada.Text_Io.Count := Option_Title'Width + 12;
      begin
	 Put_Line(Help_Header);
	 New_Line;

	 for Title in Option_Title'Range loop
	    if Help_Sections(Title) /= Null_Unbounded_String then
	       New_Line;
	       Put_Line(To_Str(Help_Sections(Title)));
	       New_Line;
	    end if;

	    if Option(Title).Short_Name = Null_Short_Name then
	       Put("-" & ",  --");
	    elsif Option(Title).Short_Name = No_Short_Name then
	       Put("    --");
	    else
	       Put("-" & Option(Title).Short_Name & ", --");
	    end if;

	    Put(Long_Name(Title));

	    case Option(Title).Needs_Value is
	       when Yes =>
		  Put("=" & Value_Form(Option(Title)) & ' ');
	       when Optional =>
		  Put("[=" & Value_Form(Option(Title)) & "] ");
	       when No => null;
	    end case;

	    Set_Col(Max_Width);
	    Put_Line(Short_Description(Option(Title)));
	 end loop;

	 if Help_Footer /= "" then
	    New_Line;
	    Put_Line(Help_Footer);
	 end if;
      end Print_Help;

      function Make_Value(Title: in Option_Title;
			  Value: in Unbounded_String) return String_Access is
	 Access_Value: String_Access;
      begin
	 if Result(Title).Value /= null then
	    Access_Value := new String(1..Length(Value) +
					 Result(Title).Value.all'Length + 1);
	    Access_Value.all := Result(Title).Value.all &
	      Character'Val(0) & To_String(Value);
	 else
	    Access_Value := new String(1..Length(Value));
	    Access_Value.all := To_String(Value);
	 end if;

	 return Access_Value;
      end Make_Value;

      procedure Check_Options_Validity is
	 Shorts: String(1..Option'Length);
	 I: Natural := 0;
      begin
	 for Title in Option'Range loop
	    if Option(Title).Short_Name /= No_Short_Name then
	       for J in 1..I loop
		  if Shorts(J) = Option(Title).Short_Name then
		     Pl_Error("short option name " &
				Character'Image(Option(Title).Short_Name) &
				" is present many times");
		     raise Redundant_Short_Option_Name_Error;
		  end if;
	       end loop;

	       I := I + 1;
	       Shorts(I) := Option(Title).Short_Name;
	    end if;
	 end loop;
      end Check_Options_Validity;

      Pos_Equal, Stop: Natural;
      End_Of_The_Options: Natural := Argument_Count;
      Help_Called: Boolean := False;
   begin
      Check_Options_Validity;

      Multiple_Set_Allowed := Multiset;

      for Num in reverse 1..Argument_Count loop
	 if Argument(Num) = "--" then
	    End_Of_The_Options := Num - 1;
	 elsif Argument(Num) = "--help" or Argument(Num) = "-h" then
	    Help_Called := True;
	 end if;
      end loop;

      if Help_Called then
	 Print_Help;
	 raise End_Of_Program_With_Help_Menu;
      end if;

      for Num in reverse 1..End_Of_The_Options loop
	 if Argument(Num)(1) /= '-' then
	    goto Continue;
	 end if;

	 Lg := Argument(Num)'Length;
	 Found := False;

	 if Lg = 1 then
	    for Title in Option'Range loop
	       if Option(Title).Short_Name = Null_Short_Name then
		  if not Is_Already_Set(Title) then
		     -- empty short option should not be set multiple times
		     Result(Title) := (True, null);
		  end if;

		  Found := True;

		  exit;
	       end if;
	    end loop;

	    if not Found then
	       Pl_Error("special void option «-» is not recognized");
	    end if;
	 elsif Argument(Num)(2) = '-' then
	    Pos_Equal := Index(Argument(Num), "=", 3);
	    if Pos_Equal = 0 then
	       Stop := Lg;
	    else
	       Stop := Pos_Equal - 1;
	    end if;

	    for Title in Option'Range loop

	       if Long_Name(Title) = Argument(Num)(3..Stop) then
		  Found := True;

		  if not Multiple_Set_Allowed(Title) and then
		    Is_Already_Set(Title) then
		     exit;
		  end if;

		  if Option(Title).Needs_Value /= No then
		     if Pos_Equal /= 0 then
			Access_Value :=
			  new String'(Argument(Num)(Pos_Equal+1..Lg));
		     else
			Check_Parameter_Value(Title, Num, Value);

			if Value /= Null_Unbounded_String then
			   Access_Value := Make_Value(Title, Value);
			   Remove.Remove_Argument(Num + 1);
			   End_Of_The_Options := End_Of_The_Options - 1;
			else
			   Access_Value := null;
			end if;
		     end if;
		  else
		     Access_Value := null;

		     if Pos_Equal /= 0 then
			Pl_Error("Option " & Long_Name(Title) &
				   " doesn't take a value");
		     end if;
		  end if;

		  Result(Title) := (Is_Set => True, Value => Access_Value);
		  exit;
	       end if;
	    end loop;

	    if not Found then
	       Pl_Error("Unknown option: «" & Argument(Num)(3..Lg) & "»");
	    end if;
	 else
	Short_Option_Loop:
	    for I in 2..Lg loop
	       for Title in Option'Range loop
		  if Option(Title).Short_Name = Argument(Num)(I) then
		     Found := True;

		     if not Multiple_Set_Allowed(Title) and then
		       Is_Already_Set(Title) then
			exit;
		     end if;

		     if Option(Title).Needs_Value /= No then
			if I > 2 then
			   Pl_Error("Short option that could take a value should not be grouped");
			   raise Bad_Grouped_Option_Error;
			elsif I /= Lg then
			   Access_Value :=
			     new String'(Argument(Num)(I+1..lg));
			   Result(Title) := (True, Access_Value);
			   exit Short_Option_Loop;
			else
			   Check_Parameter_Value(Title, Num, Value);

			   if Value /= Null_Unbounded_String then
			      Access_Value := Make_Value(Title, Value);
			      Remove.Remove_Argument(Num + 1);
			      End_Of_The_Options := End_Of_The_Options - 1;
			   else
			      Access_Value := null;
			   end if;
			end if;
		     else
			Access_Value := null;
		     end if;

		     Result(Title) := (Is_Set => True, Value => Access_Value);
		     exit;
		  end if;
	       end loop;

	       if not Found then
		  Pl_Error("Unknown option: «-" & Argument(Num)(I) & "»");
	       else
		  Found := False;
	       end if;
	    end loop Short_Option_Loop;
	 end if;

     <<Continue>> null;
      end loop;

      if End_Of_The_Options /= Argument_Count then
	 Remove.Remove_Argument(End_Of_The_Options + 1);
      end if;

      for N in reverse 1..End_Of_The_Options loop
	 if Argument(N)(1) = '-' then
	    Remove.Remove_Argument(N);
	 end if;
      end loop;

      return Result;
   end Parse;

end Get_Options;
