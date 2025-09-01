
--
--  usage
--    a18n proj.gpr
--

with Ada.Characters.Handling;
with Ada.Directories;
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
with A18n_Intl;
with A18n_POT;

procedure A18n_Main
is
   package Command_Line renames A18n_Command_Line;
   package POT          renames A18n_POT;

   use Ada.Text_IO;

   Program_Termination : exception;

   procedure Analyze_File      (Filename : String);
   procedure Analyze_Project   (Project_File : String);
   procedure Handle_Help_And_Version;
   procedure Check_Driver;
   procedure Check_Project;
   procedure Check_Output;

   ------------------
   -- Analyze_File --
   ------------------

   procedure Analyze_File (Filename : String)
   is
      use Libadalang.Analysis;
      use Libadalang.Common;
      use Libadalang.Iterators;

      Context : constant Analysis_Context := Create_Context;
      Unit    : constant Analysis_Unit    := Context.Get_From_File (Filename);
--      Predic  : constant Ada_Node_Predicate := Kind_Is (Ada_Subp_Kind_Function);
      Predic  : constant Ada_Node_Predicate
         := Kind_Is (Ada_Call_Expr)
         or Kind_Is (Ada_Un_Op)
         or Kind_Is (Ada_String_Literal);
      Iter    : Traverse_Iterator'Class     := Find (Unit.Root, Predic);
      Node    : Ada_Node;
   begin
      while Iter.Next (Node) loop
         case Node.Kind is
         when Ada_Call_Expr => A18n_Analysis.Analyze_Call_Expr (Node, Filename);
         when Ada_Un_Op     => A18n_Analysis.Analyze_Un_Op     (Node, Filename);
--         when Ada_String_Literal => Analyze_Un_Op     (Node);
         when others        => null;
         end case;
      end loop;
--      Predic  : constant Ada_Node_Predicate := Kind_Is (Ada_Call_Expr);
--      Iter    : Traverse_Iterator'Class     := Find (Unit.Root, Predic);
--      Node    : Ada_Node;
--   begin
--      while Iter.Next (Node) loop
--         Analyze_Function (Node);
--      end loop;
   end Analyze_File;

   ---------------------
   -- Analyze_Project --
   ---------------------

   procedure Analyze_Project (Project_File : String)
   is
      package VFS renames GNATCOLL.VFS;

      Tree  : GNATCOLL.Projects.Project_Tree_Access;
      Env   : GNATCOLL.Projects.Project_Environment_Access;
      Projs : Libadalang.Project_Provider.Provider_And_Projects_Array_Access;
   begin
      Tree := new GNATCOLL.Projects.Project_Tree;
      GNATCOLL.Projects.Initialize (Env);
      Tree.Load
        (Root_Project_Path => VFS.Create (VFS.Filesystem_String (Project_File)),
         Env               => Env);

      Projs := Libadalang.Project_Provider.Create_Project_Unit_Providers (Tree);

--      if Command_Line.Verbose then
--         for P2 of Projs.all loop
--            for P of P2.Projects.all loop
--               Put_Line (Project_Path (P));
--            end loop;
--         end loop;
--      end if;

      for PAP of Projs.all loop

         declare
            use Ada.Strings.Unbounded;
            use Libadalang.Project_Provider;

            Sources : constant Filename_Vectors.Vector
               := Source_Files (Tree     => Tree.all,
                                Mode     => Root_Project);
         begin
            for Source of Sources loop

               if Command_Line.Verbose then
                  Put_Line (To_String (Source));
               end if;

               Analyze_File (Filename => To_String (Source));
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
      if Command_Line.Help then
         Command_Line.Display_Help;
         raise Program_Termination;

      elsif Command_Line.Version then
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

      Driver : String_Access renames A18n_Command_Line.Driver;
      Copy   : String_Access;
   begin
      if Driver.all in "" then
         Driver := new String'(Driver_Intl);
      end if;

      Copy := Driver;
      Driver := new String'(Ada.Characters.Handling.To_Lower (Driver.all));
      Free (Copy);

      if Driver.all not in Driver_Intl then
         Put_Line (Standard_Error,
                   "Unknown i18n driver, '" & Driver.all & "'. Exiting");
         raise Program_Termination;
      end if;
   end Check_Driver;

   -------------------
   -- Check_Project --
   -------------------

   procedure Check_Project
   is
   begin
      if Command_Line.Project.all in "" then
         Put_Line (Standard_Error,
                   "Unknown project, '" & Command_Line.Project.all & "'. Exiting");
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
      if Command_Line.Output.all = "" then
         Command_Line.Output
            := new String'(Base_Name (Command_Line.Project.all) & ".pot");
      end if;

      if Command_Line.Force then
         POT.Open (Command_Line.Output.all);
         return;
      end if;

      if Exists (Command_Line.Output.all) then
         Put_Line (Standard_Error,
                   "POT file exists. Exiting");
         raise Program_Termination;
      end if;

      POT.Open (Command_Line.Output.all);
   end Check_Output;

begin
   Command_Line.Parse;
   Handle_Help_And_Version;
   Check_Driver;
   Check_Project;
   Check_Output;

--   POT.Put_Entry ("a18n_main.adb", 117, "Hello, World!", "Test");

   Analyze_Project (Command_Line.Project.all);

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
