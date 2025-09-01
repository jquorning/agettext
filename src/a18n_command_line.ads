
with GNAT.Strings;

package A18n_Command_Line
is
   use GNAT.Strings;

   Debug     : aliased Boolean       := False;
   Driver    : aliased String_Access := new String'("intl");
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
