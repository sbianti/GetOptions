with Ada.Text_IO;
with Ada.Command_Line.Remove;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

package body Get_Option is
   use Ada.Text_IO, Ada.Command_Line, Ada.Strings.Unbounded, Ada.Strings.Fixed;

   Already_Warned_For_Multiple_Set: array (Option_Title) of Boolean :=
     (others => False);

   procedure Pl_Error(Item: String) is
   begin
      Put_Line(Standard_Error, "Parsing error: " & Item);
   end;
   pragma Inline(Pl_Error);

   function Long_Name(Opt: Option_Title) return String is
      use Ada.Characters.Handling;
   begin
      return To_Lower(Option_Title'Image(Opt));
   end Long_Name;
   pragma Inline(Long_Name);

   function Get_Options(Option: in Option_Setting_Array;
		       Help_Header, Help_Footer: in String)
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
	 if not Result(Title).Set then
	    return False;
	 end if;

	 if not Already_Warned_For_Multiple_Set(Title) then
	    Pl_Error("Option " & Option_Title'Image(Title) &
		       " set multiple times");
	    Already_Warned_For_Multiple_Set(Title) := True;
	 end if;

	 return True;
      end Is_Already_Set;

      function Short_Explanation(Item: in Option_Setting) return String is
      begin
	 return To_String(Item.Short_Explanation);
      end Short_Explanation;
      pragma Inline(Short_Explanation);

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
	    if Option(Title).Short_Name = Null_Short_Name then
	       Put("-" & ",  --");
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
	    Put_Line(Short_Explanation(Option(Title)));
	 end loop;

	 New_Line;
	 Put_Line(Help_Footer);
      end Print_Help;

      Pos_Equal, Stop: Natural;
      End_Of_The_Options: Natural := Argument_Count;
      Help_Called: Boolean := False;
   begin
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

		  if Is_Already_Set(Title) then
		     exit;
		  end if;

		  if Option(Title).Needs_Value /= No then
		     if Pos_Equal /= 0 then
			Access_Value :=
			  new String'(Argument(Num)(Pos_Equal+1..Lg));
		     else
			Check_Parameter_Value(Title, Num, Value);

			if Value /= Null_Unbounded_String then
			   Access_Value := new String(1..Length(Value));
			   Access_Value.all := To_String(Value);
			   Remove.Remove_Argument(Num + 1);
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

		  Result(Title) := (Set => True, Value => Access_Value);
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

		     if Is_Already_Set(Title) then
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
			      Access_Value := new String(1..Length(Value));
			      Access_Value.all := To_String(Value);
			      Remove.Remove_Argument(Num + 1);
			   else
			      Access_Value := null;
			   end if;
			end if;
		     else
			Access_Value := null;
		     end if;

		     Result(Title) := (Set => True, Value => Access_Value);
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
   end Get_Options;

end Get_Option;
