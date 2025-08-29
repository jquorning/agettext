
--
--  usage
--    a18n proj.gpr
--

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Iterators;
with Libadalang.Project_Provider;

procedure A18n_Main
is
   use Ada.Text_IO;

   procedure Analyze_Function (Node : Libadalang.Analysis.Ada_Node'Class);
   procedure Analyze_File (Filename : String);
   procedure Analyze_Project (Project_File : String);

--   Output_File : constant String := "x.po";
--   Translator  : constant String := "intl";

   ----------------------
   -- Analyze_Function --
   ----------------------

   procedure Analyze_Function (Node : Libadalang.Analysis.Ada_Node'Class)
   is
--      use Libadalang.Common;
--      use Langkit_Support.Slocs;
--      use Langkit_Support.Text;
      use Langkit_Support;
   begin
      Put_Line ("Found: " & Node.Image);
      Put_Line (" Line: " &
                Slocs.Line_Number'Image (Node.Sloc_Range.Start_Line)          &
                ":"   & Slocs.Column_Number'Image (Node.Sloc_Range.Start_Column) &
                ".."  & Slocs.Column_Number'Image (Node.Sloc_Range.End_Column)   &
                ": " & Text.Image (Node.Text));
      Put_Line (" Kind: " & Node.Kind'Image);
--      Put_Line ("  P_Kind: " & Node.As_Call_Expr.P_Kind'Image);
--      Put_Line ("  Token: " & Libadalang.Common.Is_Token_Node (Node.Kind)'Image);
--      Put_Line ("  List : " & Libadalang.Common.Is_List_Node  (Node.Kind)'Image);
--      Put_Line ("  Error: " & Libadalang.Common.Is_Token_Node (Node.Kind)'Image);

--      Put_Line (" Subp: " & Node.As_Subp_Kind);

--      Put_Line (" Is_Call: " & Node.P_Is_Call'Image);
--      Put_Line (" Name: " & Node.P_Defining_Name'Image);
--      Put_Line (" Resolv: " & Node.P_Resolve_Names'Image);
--      Put_Line (" Name: " & Text.Image (Node.F_Name));
--      Put_Line (" Xref: " & Node.P_Xref_Entry_Point'Image);
--      Put_Line (" GNAT: " & Node.P_Gnat_Xref (False)'Image);

--      Put_Line ("  Fully: " & Text.Image (Node.P_Gnat_Xref (False).P_Fully_Qualified_Name));
--      Put_Line ("  Uniqu: " & Text.Image (Node.P_Gnat_Xref (False).P_Unique_Identifying_Name));
--      Put_Line (" Is_Call: " & Node.P_Is_Call'Image);
--      Put_Line ("Identifier: " & Node.F_Ids.Image);
   end Analyze_Function;

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
      Predic  : constant Ada_Node_Predicate := Kind_Is (Ada_Call_Expr);
      Iter    : Traverse_Iterator'Class     := Find (Unit.Root, Predic);
      Node    : Ada_Node;
   begin
      while Iter.Next (Node) loop
         Analyze_Function (Node);
      end loop;
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

      for PAP of Projs.all loop
         declare
            use Ada.Strings.Unbounded;
            use Libadalang.Project_Provider;

--   Context : constant Libadalang.Analysis.Analysis_Context
--     := Libadalang.Analysis.Create_Context (Unit_Provider => PAP.Provider);

            Sources : constant Filename_Vectors.Vector
               := Source_Files (Tree     => Tree.all,
                                Mode     => Root_Project);
         begin
            for Source of Sources loop
               Put_Line (To_String (Source));
               Analyze_File (Filename => To_String (Source));
            end loop;
         end;
      end loop;
      Libadalang.Project_Provider.Free (Projs);

      GNATCOLL.Projects.Free (Tree);
      GNATCOLL.Projects.Free (Env);
   end Analyze_Project;

begin
   Analyze_Project (Ada.Command_Line.Argument (1));
end A18n_Main;
