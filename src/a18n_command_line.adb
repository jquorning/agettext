
package body A18n_Command_Line
is
   use GNAT.Command_Line;

   Config : Command_Line_Configuration;

   -----------
   -- Parse --
   -----------

   procedure Parse
   is
   begin
      Define_Switch (Config, Debug'Access, "-d",
                     Help => "Display debug texts");
      Define_Switch (Config, Driver'Access,
                     Long_Switch => "--driver=",
                     Argument    => "DRIVER",
                     Help        => "Driver package for i18n. Default is 'Intl'");
      Define_Switch (Config, Force'Access, "-f",
                     Long_Switch => "--force",
                     Help        => "Force. (Overwrite output POT)");
      Define_Switch (Config, Help'Access, "-h",
                     Long_Switch => "--help",
                     Help        => "Display help and exit");
      Define_Switch (Config, Output'Access, "-o:",
                     Long_Switch => "--output=",
                     Argument    => "POT",
                     Help        => "Output POT file");
      Define_Switch (Config, Project'Access, "-P:",
                     Help     => "GNAT Project file (.gpr)",
                     Argument => "<PROJECT>");
      Define_Switch (Config, Verbose'Access, "-v",  Help => "Verbose");
      Define_Switch (Config, Version'Access, "-V",
                     Long_Switch => "--version",
                     Help        => "Display version and exit");
--         Set_Usage (Config, "AAA", "BBB", "CCC");
      Getopt (Config);
   exception
      when Exit_From_Command_Line =>
         null;  --  Help does this!

      when GNAT.Command_Line.Invalid_Parameter =>
         raise Invalid_Parameter;
--      when Invalid_Switch =>
--         Put_Line (Standard_Error, "Invalid switch");
   end Parse;

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help is
   begin
      Display_Help (Config);
   end Display_Help;

   ----------
   -- Free --
   ----------

   procedure Free is
   begin
      Free (Config);
   end Free;

end A18n_Command_Line;
