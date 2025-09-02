
--
--  usage
--    a18n proj.gpr
--

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with GNAT.Strings;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Iterators;
with Libadalang.Project_Provider;

with A18n_Analysis;
with A18n_Command_Line;
with A18n_Config;
with A18n_Options;
with A18n_POT;

procedure A18n_Main
is
   package Command_Line renames A18n_Command_Line;
   package Option       renames A18n_Options;
   package POT          renames A18n_POT;

   use Ada.Text_IO;

   Program_Termination : exception;

   procedure Analyze_File      (Filename   : String;
                                Short_Name : String);
   procedure Analyze_Project   (Project_File : String);
   procedure Handle_Help_And_Version;
   procedure Check_Driver;
   procedure Check_Project;
   procedure Check_Output;

   ------------------
   -- Analyze_File --
   ------------------

   procedure Analyze_File (Filename   : String;
                           Short_Name : String)
   is
      use Libadalang.Analysis;
      use Libadalang.Common;
      use Libadalang.Iterators;

      Context : constant Analysis_Context := Create_Context;
      Unit    : constant Analysis_Unit    := Context.Get_From_File (Filename);
      Predic  : constant Ada_Node_Predicate
         := Kind_Is (Ada_Call_Expr)
         or Kind_Is (Ada_Un_Op);
--         or Kind_Is (Ada_String_Literal);
      Iter    : Traverse_Iterator'Class     := Find (Unit.Root, Predic);
      Node    : Ada_Node;
   begin
      while Iter.Next (Node) loop
         case Node.Kind is
         when Ada_Call_Expr => A18n_Analysis.Analyze_Call_Expr (Node, Short_Name);
         when Ada_Un_Op     => A18n_Analysis.Analyze_Un_Op     (Node, Short_Name);
--         when Ada_String_Literal => Analyze_Un_Op     (Node);
         when others        => null;
         end case;
      end loop;
   end Analyze_File;

   ---------------------
   -- Analyze_Project --
   ---------------------

   procedure Analyze_Project (Project_File : String)
   is
      use Ada.Directories;
      package VFS renames GNATCOLL.VFS;

      Tree  : GNATCOLL.Projects.Project_Tree_Access;
      Env   : GNATCOLL.Projects.Project_Environment_Access;
      Projs : Libadalang.Project_Provider.Provider_And_Projects_Array_Access;
      CWD   : constant String := Current_Directory;
   begin
      Tree := new GNATCOLL.Projects.Project_Tree;
      GNATCOLL.Projects.Initialize (Env);
      Tree.Load
        (Root_Project_Path => VFS.Create (VFS.Filesystem_String (Project_File)),
         Env               => Env);

      Projs := Libadalang.Project_Provider.Create_Project_Unit_Providers (Tree);

      for PAP of Projs.all loop

         declare
            use Ada.Strings.Unbounded;
            use Libadalang.Project_Provider;

            Sources : constant Filename_Vectors.Vector
               := Source_Files (Tree     => Tree.all,
                                Mode     => Root_Project);
         begin
            for Source_US of Sources loop
               declare
                  use Ada.Strings.Fixed;

                  Source   : constant String := To_String (Source_US);
                  Relative : constant String
                     := Tail (Source, Source'Length - CWD'Length - 1);
               begin
                  if Option.Verbose then
                     Put_Line (Relative);
                  end if;

                  Analyze_File (Filename   => Source,
                                Short_Name => Relative);
               end;
            end loop;
         end;
      end loop;
      Libadalang.Project_Provider.Free (Projs);

      GNATCOLL.Projects.Free (Tree);
      GNATCOLL.Projects.Free (Env);
   end Analyze_Project;

--    ------------------
--    -- Process_Unit --
--    ------------------

--    procedure Process_Unit (Context : Libadalang.Helpers.App_Job_Context;
--                            Unit    : Libadalang.Analysis.Analysis_Unit)
--    is
--       pragma Unreferenced (Context);

--       use Libadalang.Analysis;
--       use Libadalang.Common;
--       use Libadalang.Iterators;

--       Predic  : constant Ada_Node_Predicate
--          := Kind_Is (Ada_Call_Expr)
--          or Kind_Is (Ada_Un_Op)
--          or Kind_Is (Ada_String_Literal);
--       Iter    : Traverse_Iterator'Class     := Find (Unit.Root, Predic);
--       Node    : Ada_Node;
--    begin
--       while Iter.Next (Node) loop
--          case Node.Kind is
--          when Ada_Call_Expr      => Analyze_Call_Expr (Node);
--          when Ada_Un_Op          => Analyze_Un_Op     (Node);
--  --         when Ada_String_Literal => Analyze_Un_Op     (Node);
--          when others             => null;
--          end case;
--       end loop;
--    end Process_Unit;

--    -----------------
--    -- Application --
--    -----------------

--    package Application is new
--       Libadalang.Helpers.App
--         (Name         => A18n_Config.Crate_Name,
--          Description  => "Ada pendant to `gettext`",
--          Process_Unit => Process_Unit);

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
         Put_Line (A18n_Config.Crate_Name &
                   " version " & A18n_Config.Crate_Version);
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

      for A in Option.Drivers'Range loop
         if Driver.all = Option.Drivers (A).Driver_Name then
            Option.Used_Driver := A;
            return;
         end if;
      end loop;

      Put_Line (Standard_Error,
              "Unknown i18n driver, '" & Driver.all & "'. Exiting");
      raise Program_Termination;
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

--   POT.Put_Entry ("a18n_main.adb", 117, "Hello, World!", "Test");

   Analyze_Project (Option.Project.all);

   Command_Line.Free;
--   Application.Run;
exception
   when Program_Termination =>
      null;

   when GNATCOLL.Projects.Invalid_Project =>
      Put_Line (Standard_Error, "Invalid project. Exiting");

   when Command_Line.Invalid_Parameter =>
      Put_Line (Standard_Error, "Invalid parameter. Exiting");
end A18n_Main;
