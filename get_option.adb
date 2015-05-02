with Ada.Text_IO;
with Ada.Command_Line.Remove;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

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

   function Get_Options(Option: in Option_Setting_Array)
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

      Pos_Equal, Stop: Natural;
      End_Of_The_Options: Natural := Argument_Count;
   begin
      for Num in 1..Argument_Count loop
	 if Argument(Num) = "--" then
	    End_Of_The_Options := Num - 1;
	    exit;
	 end if;
      end loop;

      for Num in reverse 1..End_Of_The_Options loop
	 Lg := Argument(Num)'Length;

	 if Argument(Num)(1) = '-' then
	    if Lg = 1 then
	       null;
	    elsif Argument(Num)(2) = '-' then
	       Found := False;

	       for Title in Option'Range loop

		  Pos_Equal := Index(Argument(Num), "=", 3);
		  if Pos_Equal = 0 then
		     Stop := Lg;
		  else
		     Stop := Pos_Equal - 1;
		  end if;

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
		  end if;
	       end loop;

	       if not Found then
		  Pl_Error("Unknown option: «" & Argument(Num)(3..Lg) & "»");
	       end if;
	    else
	       -- option(s) courte(s)
	   Short_Option_Loop:
	       for I in 2..Lg loop
		  Found := False;

		  for Title in Option'Range loop
		     if Option(Title).Short_Name = Argument(Num)(I) then
			if Is_Already_Set(Title) then
			   Found := True;
			   exit;
			end if;

			Found := True;
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

			Result(Title).Set := True;
			Result(Title).Value := Access_Value;

			exit;
		     end if;
		  end loop;

		  if not Found then
		     Pl_Error("Unknown option: «-" & Argument(Num)(I) & "»");
		  end if;
	       end loop Short_Option_Loop;
	    end if;
	 end if;
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
