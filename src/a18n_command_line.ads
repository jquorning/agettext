
with GNAT.Command_Line;

package A18n_Command_Line
is
   Invalid_Parameter : exception renames GNAT.Command_Line.Invalid_Parameter;

   procedure Parse;
   procedure Display_Help;
   procedure Free;

end A18n_Command_Line;
