with Ada.Text_IO;
with Ada.Command_Line.Remove;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

package body Get_Option is
   use Ada.Text_IO, Ada.Command_Line, Ada.Strings.Unbounded;

   Option: Option_Setting_Array;

   type Access_String is access String;

   procedure Pl_Error(Item: String) is
   begin
      Put_Line(Standard_Error, Item);
   end;
   pragma Inline(Pl_Error);

   function Long_Name(Opt: Option_Title) return String is
      use Ada.Characters.Handling;
   begin
      return To_Lower(Option_Title'Image(Opt));
   end Long_Name;

   procedure Set_Options(Option_Settings: in Option_Setting_Array) is
      use Ada.Text_IO;
   begin
      Option := Option_Settings;
   end Set_Options;

   function Get_Options return Option_Result_Array is
      Lg: Natural;
      Result: Option_Result_Array;
      Found: Boolean;
      Value: Unbounded_String;
      Access_Value: Access_String;

      procedure Check_Parameter_Value(Title: in Option_Title;
				      Count: in Natural;
				      Value: out Unbounded_String) is
      begin
	 if Argument_Count = Count or else Argument(Count + 1)(1) = '-' then
	    if Option(Title).Needs_Value = Yes then
	       Pl_Error("Error: Option " &
			  Long_Name(Title) & " requires an argument");
	       raise Parsing_Error;
	    end if;
	    Value := Null_Unbounded_String;
	 else
	    Value := To_Unbounded_String(Argument(Count + 1));
	 end if;
      end Check_Parameter_Value;
   begin
      for Num in reverse 1..Argument_Count loop
	 Lg := Argument(Num)'Length;
	 if Argument(Num)(1) = '-' then
	    if Lg = 1 or (Lg = 2 and Argument(Num)(2) = '-') then
	       -- fin des options TODO
	       null;
	    elsif Argument(Num)(2) = '-' then
	       Found := False;
	       for Title in Option'Range loop
		  if Long_Name(Title) = Argument(Num)(3..Lg) then
		     if Option(Title).Needs_Value /= No then
			Check_Parameter_Value(Title, Num, Value);
			if Value /= Null_Unbounded_String then
			   Access_Value := new String(1..Length(Value));
			   Access_Value.all := To_String(Value);
			   Remove.Remove_Argument(Num + 1);
			else
			   Access_Value := null;
			end if;
		     else
			Access_Value := null;
		     end if;
		     Result(Title) := (Set => True, Value => Access_Value);
		     Found := True;
		  end if;
	       end loop;

	       if not Found then
		  Pl_Error("Unknown option: «" & Argument(Num)(3..Lg) & "»");
	       end if;
	    else
	       -- option(s) courte(s)
	       for I in 2..Lg loop
		  for Title in Option'Range loop
		     if Option(Title).Short_Name = Argument(Num)(I) then
			Found := True;
			if Option(Title).Needs_Value /= No then
			   if I > 2 then
			      Pl_Error("Short option that could take a value should not be grouped");
			      raise Bad_Grouped_Option_Error;
			   end if;
			   Check_Parameter_Value(Title, Num, Value);
			   if Value /= Null_Unbounded_String then
			      Access_Value := new String(1..Length(Value));
			      Access_Value.all := To_String(Value);
			      Remove.Remove_Argument(Num + 1);
			   else
			      Access_Value := null;
			   end if;
			else
			   Access_Value := null;
			end if;
			Result(Title).Set := True;
			Result(Title).Value := Access_Value;
			exit;
		     end if;
		  end loop;
		  if not Found then
		     Pl_Error("Unknown option: «-" & Argument(Num)(I) & "»");
		  else
		     Found := False;
		  end if;
	       end loop;
	    end if;
	    Remove.Remove_Argument(Num);
	 end if;
      end loop;

      return Result;
   end Get_Options;
end Get_Option;
