
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with GNATCOLL.Projects;

with GNAT.Strings;

with A18n_Analysis;
with A18n_Command_Line;
with A18n_Driver;
with A18n_Options;
with A18n_POT;

with Agettext_Config;

---------------
-- A18n_Main --
---------------

procedure A18n_Main
is
   package Command_Line renames A18n_Command_Line;
   package Option       renames A18n_Options;
   package POT          renames A18n_POT;

   use Ada.Text_IO;

   Program_Termination          : exception;

   procedure Handle_Help_And_Version;
   procedure Check_Driver;
   procedure Check_Project;
   procedure Check_Output;

   -----------------------------
   -- Handle_Help_And_Version --
   -----------------------------

   procedure Handle_Help_And_Version
   is
   begin
      if Option.Help then
         Command_Line.Display_Help;
         raise Program_Termination;

      elsif Option.Version then
         Put_Line (Agettext_Config.Crate_Name &
                   " version " & Agettext_Config.Crate_Version);
         raise Program_Termination;
      end if;
   end Handle_Help_And_Version;

   ------------------
   -- Check_Driver --
   ------------------

   procedure Check_Driver
   is
      use GNAT.Strings;

      Driver_Intl : constant String := "intl";

      Driver : String_Access renames Option.Driver_Str;
      Copy   : String_Access;
   begin
      if Driver.all in "" then
         Driver := new String'(Driver_Intl);
      end if;

      Copy := Driver;
      Driver := new String'(Ada.Characters.Handling.To_Lower (Driver.all));
      Free (Copy);

      A18n_Driver.Load (Driver.all);

   end Check_Driver;

   -------------------
   -- Check_Project --
   -------------------

   procedure Check_Project
   is
   begin
      if Option.Project.all in "" then
         Put_Line (Standard_Error,
                   "Unknown project, '" & Option.Project.all & "'. Exiting");
         raise Program_Termination;
      end if;
   end Check_Project;

   ------------------
   -- Check_Output --
   ------------------

   procedure Check_Output
   is
      use Ada.Directories;
      use type GNAT.Strings.String_Access;
   begin
      if Option.Output.all = "" then
         Option.Output
            := new String'(Base_Name (Option.Project.all) & ".pot");
      end if;

      if Option.Force then
         POT.Open (Option.Output.all);
         return;
      end if;

      if Exists (Option.Output.all) then
         Put_Line (Standard_Error,
                   "POT file exists. Exiting");
         raise Program_Termination;
      end if;

      POT.Open (Option.Output.all);
   end Check_Output;

begin
   Command_Line.Parse;
   Handle_Help_And_Version;
   Check_Driver;
   Check_Project;
   Check_Output;

   A18n_Analysis.Analyze_Project (Option.Project.all);

   Command_Line.Free;

exception
   when Program_Termination =>
      null;

   when GNATCOLL.Projects.Invalid_Project =>
      Put_Line (Standard_Error, "Invalid project. Exiting");

   when Command_Line.Invalid_Parameter =>
      Put_Line (Standard_Error, "Invalid parameter. Exiting");

   when A18n_Analysis.Missing_Driver_Specification =>
      Put_Line (Standard_Error,
                "'" & A18n_Driver.Package_Name &
                ".ads" & "' is not part of the project. Exiting");

   when Occ : A18n_Driver.XML_Error =>
      Put_Line (Standard_Error,
                Ada.Exceptions.Exception_Message (Occ));

end A18n_Main;
