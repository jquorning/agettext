
with GNAT.Command_Line;
with GNAT.Strings;

package A18n_Command_Line
is
   use GNAT.Strings;

   Invalid_Parameter : exception renames GNAT.Command_Line.Invalid_Parameter;

   Debug     : aliased Boolean       := False;
   Driver    : aliased String_Access := null;
   Force     : aliased Boolean       := False;
   Help      : aliased Boolean       := False;
   Output    : aliased String_Access := null;
   Project   : aliased String_Access := null;
   Verbose   : aliased Boolean       := False;
   Version   : aliased Boolean       := False;

   procedure Parse;
   procedure Display_Help;
   procedure Free;

end A18n_Command_Line;
