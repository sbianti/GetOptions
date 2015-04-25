with Ada.Text_IO;
with Ada.Command_Line.Remove;
with Ada.Characters.Handling;

package body Get_Option is
   use Ada.Text_IO;

   Option: Option_Setting_Array;

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
      use Ada.Command_Line;
      Lg: Natural;
      Result: Option_Result_Array;
      Found: Boolean;
   begin
      for Num in reverse 1..Argument_Count loop
	 Lg := Argument(Num)'Length;
	 if Argument(Num)(1) = '-' then
	    if Lg = 1 or (Lg = 2 and Argument(Num)(2) = '-') then
	       -- fin des options TODO
	       null;
	    elsif Argument(Num)(2) = '-' then
	       Found := False;
	       for N in Option'Range loop
		  if Long_Name(N) = Argument(Num)(3..Lg) then
		     Result(N).Set := True;
		     Found := True;
		  end if;
	       end loop;

	       if not Found then
		  Pl_Error("Unknown option: «" & Argument(Num)(3..Lg) & "»");
	       end if;
	    else
	       -- option(s) courte(s)
	       for I in Argument(Num)'Range loop
		  for J in Option'Range loop
		     if Option(J).Short_Name = Argument(Num)(I) then
			Result(J).Set := True;
			exit;
		     end if;
		  end loop;
	       end loop;
	    end if;
	    Remove.Remove_Argument(Num);
	 end if;
      end loop;

      return Result;
   end Get_Options;
end Get_Option;
