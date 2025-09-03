
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
with Langkit_Support.Text;

with A18n_Command_Line;
with A18n_Driver;
with A18n_Options;
with A18n_POT;
with A18n_Util;

with Agettext_Config;

---------------
-- A18n_Main --
---------------

procedure A18n_Main
is
   package A            renames Libadalang.Analysis;
   package C            renames Libadalang.Common;
   package Command_Line renames A18n_Command_Line;
   package Driv         renames A18n_Driver;
   package GP           renames GNATCOLL.Projects;
   package Option       renames A18n_Options;
   package POT          renames A18n_POT;
   package PP           renames Libadalang.Project_Provider;
   package T            renames Langkit_Support.Text;
   package VFS          renames GNATCOLL.VFS;
   package Util         renames A18n_Util;

   use Ada.Text_IO;

   Missing_Driver_Specification : exception;
   Program_Termination          : exception;

   procedure Analyze_Project (Project_File : String);
   function Find_Driver_Package (Projs : PP.Provider_And_Projects_Array_Access;
                                 Tree  : GP.Project_Tree_Access)
                                 return String;
   function Find_Definition (Context    : in out A.Analysis_Context;
                             Filename   : String;
                             Subprogram : String)
                             return A.Defining_Name'Class;
   function Project_Units (Context : in out A.Analysis_Context'Class;
                           Tree    : GP.Project_Tree_Access;
                           Projs   : PP.Provider_And_Projects_Array_Access)
                           return A.Analysis_Unit_Array;
   procedure Analyze (Node     : A.Base_Id'Class;
                      Filename : String);
   procedure Handle_Paren_Expr (Node     : A.Paren_Expr'Class;
                                Filename : String);
   procedure Handle_Call_Expr (Node     : A.Call_Expr'Class;
                               Filename : String);
   procedure Handle_Un_Op  (Node     : A.Un_Op'Class;
                            Filename : String);
   function Relative (Full : String;
                      Base : String) return String;
   procedure Handle_Help_And_Version;
   procedure Check_Driver;
   procedure Check_Project;
   procedure Check_Output;

   --------------------
   -- Find_Definiton --
   --------------------

   function Find_Definition (Context    : in out A.Analysis_Context;
                             Filename   : String;
                             Subprogram : String)
                             return A.Defining_Name'Class
   is
      use Libadalang.Iterators;

      Unit    : constant A.Analysis_Unit    := Context.Get_From_File (Filename);
      Predic  : constant Ada_Node_Predicate := Kind_Is (C.Ada_Defining_Name);
      Iter    : Traverse_Iterator'Class     := Find (Unit.Root, Predic);
      Node    : A.Ada_Node;
   begin
      while Iter.Next (Node) loop
         declare
            Defining : constant A.Defining_Name'Class := Node.As_Defining_Name;
         begin
            if Subprogram = T.Image (Defining.Text) then
               return Defining;
            end if;
         end;
      end loop;
      return A.No_Defining_Name;
   end Find_Definition;

   -------------------------
   -- Find_Driver_Package --
   -------------------------

   function Find_Driver_Package (Projs : PP.Provider_And_Projects_Array_Access;
                                 Tree  : GP.Project_Tree_Access)
                                 return String
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;

      Driver : Driv.Driver_Type'Class
         renames Option.Drivers (Option.Used_Driver).all;

      Spec_File : constant String := Driver.Package_Name & ".ads";
   begin
      for PAP of Projs.all loop
         declare
            Sources : constant PP.Filename_Vectors.Vector
               := PP.Source_Files (Tree => Tree.all,
                                   Mode => PP.Default);
         begin
            for Source_US of Sources loop
               declare
                  Source : constant String := To_String (Source_US);
               begin
                  if Tail (Source, Spec_File'Length) = Spec_File then
                     return Source;
                  end if;
               end;
            end loop;
         end;
      end loop;
      raise Missing_Driver_Specification;
   end Find_Driver_Package;

   -------------------
   -- Project_Units --
   -------------------

   function Project_Units (Context : in out A.Analysis_Context'Class;
                           Tree    : GP.Project_Tree_Access;
                           Projs   : PP.Provider_And_Projects_Array_Access)
                           return A.Analysis_Unit_Array
   is
      use Ada.Strings.Unbounded;

      Count : Natural := 0;
   begin
      for PAP of Projs.all loop
         declare
            Sources : constant PP.Filename_Vectors.Vector
               := PP.Source_Files (Tree     => Tree.all,
                                   Mode     => PP.Default);
         begin
            Count := Count + Natural (PP.Filename_Vectors.Length (Sources));
         end;
      end loop;

      declare
         Units : A.Analysis_Unit_Array (1 .. Count);
         Pos_U : Natural := 0;
      begin
         for PAP of Projs.all loop
            declare
               Sources : constant PP.Filename_Vectors.Vector
                  := PP.Source_Files (Tree     => Tree.all,
                                      Mode     => PP.Default);
            begin
               for S of Sources loop
                  Pos_U := Pos_U + 1;
                  Units (Pos_U) := Context.Get_From_File (Filename => To_String (S));
               end loop;
            end;
         end loop;

         pragma Assert (Count = Pos_U);

         return Units;
      end;
   end Project_Units;

   ----------------------
   -- Handle_Call_Expr --
   ----------------------

   procedure Handle_Call_Expr (Node     : A.Call_Expr'Class;
                               Filename : String)
   is
      use type C.Ada_Node_Kind_Type;

      First  : constant A.Ada_Node           := Node.First_Child;
      Last   : constant A.Ada_Node           := Node.Last_Child;
      Assoc  : constant A.Assoc_List         := Last.As_Assoc_List;
      Params : constant A.Param_Actual_Array := Assoc.P_Zip_With_Params;
   begin
      pragma Assert (Params'Length = 1);
      pragma Assert (First.Kind in C.Ada_Dotted_Name | C.Ada_String_Literal);

      declare
         Actual : constant A.Expr'Class
            := A.Actual (Params (Params'First));
      begin
         if Actual.Kind = C.Ada_String_Literal then
            POT.Put_Entry
                  (Source_Name   => Filename,
                   Text          => Util.Un_Quote (T.Image (Actual.Text)),
                   Line_Number   => Actual.Sloc_Range.Start_Line,
                   Column_Number => Actual.Sloc_Range.Start_Column,
                   Comment       => "");
         end if;
      end;

   end Handle_Call_Expr;

   -----------------------
   -- Handle_Paren_Expr --
   -----------------------

   procedure Handle_Paren_Expr (Node     : A.Paren_Expr'Class;
                                Filename : String)
   is
      First : constant A.Ada_Node := Node.First_Child;
      Last  : constant A.Ada_Node := Node.Last_Child;
   begin
      if Last.Kind in C.Ada_Paren_Expr then
         Handle_Paren_Expr (Last.As_Paren_Expr, Filename);
         return;
      end if;

      POT.Put_Entry
            (Source_Name   => Filename,
             Text          => Util.Un_Quote (T.Image (First.Text)),
             Line_Number   => First.Sloc_Range.Start_Line,
             Column_Number => First.Sloc_Range.Start_Column,
             Comment       => "");
   end Handle_Paren_Expr;

   ------------------
   -- Handle_Un_Op --
   ------------------

   procedure Handle_Un_Op (Node     : A.Un_Op'Class;
                           Filename : String)
   is
      use Ada.Strings;
      use type C.Ada_Node_Kind_Type;

      First  : constant A.Ada_Node           := Node.First_Child;
      Last   : constant A.Ada_Node           := Node.Last_Child;
   begin
      pragma Assert (First.Kind in C.Ada_Op_Minus
                                 | C.Ada_Op_Plus
                                 | C.Ada_Op_Abs
                                 | C.Ada_Op_Not);

      if Last.Kind = C.Ada_Paren_Expr then
         Handle_Paren_Expr (Last.As_Paren_Expr, Filename);
         return;

      elsif Last.Kind /= C.Ada_String_Literal then
         Put_Line (Standard_Error,
                   Filename &
                   ":" & Fixed.Trim (Node.Sloc_Range.Start_Line'Image,   Left) &
                   ":" & Fixed.Trim (Node.Sloc_Range.Start_Column'Image, Left) &
                   ": warning: Can not translate this");
         return;
      end if;

      declare
         Literal : constant A.String_Literal := Last.As_String_Literal;
      begin
         POT.Put_Entry
               (Source_Name   => Filename,
                Text          => Util.Un_Quote (T.Image (Literal.Text)),
                Line_Number   => Literal.Sloc_Range.Start_Line,
                Column_Number => Literal.Sloc_Range.Start_Column,
                Comment       => "");
      end;
   end Handle_Un_Op;

   -------------
   -- Analyze --
   -------------

   procedure Analyze (Node     : A.Base_Id'Class;
                      Filename : String)
   is
      use type C.Ada_Node_Kind_Type;

      Text : constant String := T.Image (Node.Text);
      Loc  : constant String := Util.Location_Of (Node);
   begin
      if Option.Debug then
         Put     (Node.Kind'Image);
         Set_Col (40);
         Put     (Text);
         Set_Col (60);
         Put     (Loc);
         New_Line;
      end if;

      if Node.Kind = C.Ada_String_Literal then
         declare
            Next    : constant A.Ada_Node := Node.Next_Sibling;
            Prev    : constant A.Ada_Node := Node.Previous_Sibling;
         begin
            if Next in A.No_Ada_Node then
               Handle_Call_Expr (Prev.Parent.Parent.As_Call_Expr, Filename);
            else
               Handle_Call_Expr (Next.Parent.As_Call_Expr, Filename);
            end if;
         end;
      else
         declare
            Parent : constant A.Ada_Node := Node.Parent;
         begin
            Handle_Un_Op (Parent.As_Un_Op, Filename);
         end;
      end if;
   end Analyze;

   --------------
   -- Relative --
   --------------

   function Relative (Full : String;
                      Base : String) return String
   is
   begin
      return Ada.Strings.Fixed.Tail (Full, Full'Length - Base'Length - 1);
   end Relative;

   ---------------------
   -- Analyze_Project --
   ---------------------

   procedure Analyze_Project (Project_File : String)
   is
      Driver : Driv.Driver_Type'Class
         renames Option.Drivers (Option.Used_Driver).all;

      CWD  : constant String         := Ada.Directories.Current_Directory;
      Tree : GP.Project_Tree_Access;
      Env  : GP.Project_Environment_Access;
   begin
      Tree := new GP.Project_Tree;
      GP.Initialize (Env);
      Tree.Load
        (Root_Project_Path => VFS.Create (VFS.Filesystem_String (Project_File)),
         Env               => Env);

      declare
         Projs : PP.Provider_And_Projects_Array_Access
            := PP.Create_Project_Unit_Providers (Tree);

         Provider : constant A.Unit_Provider_Reference
            := PP.Create_Project_Unit_Provider (Tree => Tree,
                                                Env  => Env);

         Context  : A.Analysis_Context
            := A.Create_Context (Unit_Provider => Provider);

         Defining : constant A.Defining_Name'Class
            := Find_Definition (Context    => Context,
                                Filename   => Find_Driver_Package (Projs, Tree),
                                Subprogram => Driver.Unary_Operator);

         Units : constant A.Analysis_Unit_Array
            := Project_Units (Context => Context,
                              Tree    => Tree,
                              Projs   => Projs);

         Results  : constant A.Ref_Result_Array
            := Defining.P_Find_All_Calls (Units              => Units,
                                          Follow_Renamings   => True,
                                          Imprecise_Fallback => False);
      begin

         if Option.Verbose then
            for U of Units loop
               Put_Line (U.Get_Filename);
            end loop;
         end if;

         for Result of Results loop
            Analyze (Node     => A.Ref (Result),
                     Filename => Relative (Full => A.Ref (Result).Unit.Get_Filename,
                                           Base => CWD));
         end loop;
         PP.Free (Projs);
      end;
--      GP.Free (Tree);
--      GP.Free (Env);
   end Analyze_Project;

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

   Analyze_Project (Option.Project.all);

   Command_Line.Free;

exception
   when Program_Termination =>
      null;

   when GNATCOLL.Projects.Invalid_Project =>
      Put_Line (Standard_Error, "Invalid project. Exiting");

   when Command_Line.Invalid_Parameter =>
      Put_Line (Standard_Error, "Invalid parameter. Exiting");

   when Missing_Driver_Specification =>
      Put_Line (Standard_Error,
                "'" & Option.Drivers (Option.Used_Driver).all.Package_Name &
                ".ads" & "' is not part of the project. Exiting");
end A18n_Main;
