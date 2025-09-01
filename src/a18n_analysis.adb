
with Ada.Text_IO;

with Langkit_Support.Text;

with Libadalang.Common;

with A18n_Util;

package body A18n_Analysis is

   package A renames Libadalang.Analysis;

   use Ada.Text_IO;

   Verbose : constant Boolean := True;

   -----------------------
   -- Analyze_Call_Expr --
   -----------------------

   procedure Analyze_Call_Expr (Node : Libadalang.Analysis.Ada_Node'Class)
   is
      use Langkit_Support;
      use A18n_Util;
      use type Libadalang.Common.Ada_Node_Kind_Type;

      Expr : constant A.Call_Expr := Node.As_Call_Expr;
--         := (case Node.Kind is
--             when Libadalang.Common.Ada_Call_Expr =>
--             when Libadalang.Common.Ada_Un_Op     => Node.As_Un_Op,
--             when others                          => raise Program_Error);
   begin
      Put_Line ("Found: " & Node.Image);

      Put      ("  Line: " & Text.Image (Node.Text));
      if Verbose then
         Put (" at " & Location_Of (Node));
      end if;
      New_Line;

      Put_Line ("  Kind: " & Node.Kind'Image);
--      Put_Line ("    P_Kind: " & Expr.P_Kind'Image);
      Put_Line ("    E Kind: " & Expr.Kind'Image);

--      if Expr.P_Kind not in Libadalang.Common.Call then
      if Expr.Kind /= Libadalang.Common.Ada_Call_Expr then
         return;
      end if;

      declare
         Name : constant A.Name'Class := Expr.As_Name;
      begin
--         Put_Line ("  Direct: " & Name.P_Is_Direct_Call'Image);
--         Put_Line ("  Access: " & Name.P_Is_Access_Call'Image);

         if Name.P_Is_Access_Call then
            return;
         end if;

         Put_Line ("  Is_Dot  : " & Name.P_Is_Dot_Call (False)'Image);
         Put_Line ("  Defining: " & Name.P_Is_Defining'Image);
         Put_Line ("  Direct  : " & Name.P_Is_Direct_Call'Image);
         Put_Line ("  Call    : " & Name.P_Is_Call'Image);
         Put_Line ("  Operator: " & Name.P_Is_Operator_Name'Image);

         if Name.P_Is_Direct_Call then
            --  Is a procedure
            return;
         end if;

         if Name.P_Is_Operator_Name then
            --  "-" () type of call
            Put_Line ("    !! ""-"" () type of call");
         else
            --  Gettext () type of call
            Put_Line ("    !! gettext () type of call");

         if not Name.P_Is_Defining then
            return;
         end if;

         declare
            Relat  : constant A.Name                     := Name.P_Relative_Name;
--          Spec   : constant A.Base_Formal_Param_Holder := Name.P_Called_Subp_Spec;
            Params : constant A.Param_Actual_Array       := Name.P_Call_Params;
            Count  : Positive := 1;
         begin
            Put_Line (" Relat: " & Text.Image (Relat.Text));


            for Par of Params loop
               declare
                  Defining   : constant A.Defining_Name'Class
                     := A.Param (Par);
                  Param_Name : constant String := Text.Image (Defining.F_Name.Text);
--                  Basic_Decl : constant A.Basic_Decl := Defining.P_Basic_Decl;

--                  Type_Name  : constant String
--                     := Text.Image (Basic_Decl.P_Fully_Qualified_Name);

--                  Base_Type  : constant A.Base_Type_Decl
--                     := A.Base_Type_Decl (Basic_Decl); --  .As_Base_Type_Decl;
               begin
                  Put ("    " & Count'Image & "=> ");
                  Put (Param_Name);
--                  Put (" :: " & Type_Name);
                  New_Line;
                  Count := Count + 1;
               end;
            end loop;
         end;
         end if;
      end;
   end Analyze_Call_Expr;

   -------------------
   -- Analyze_Un_Op --
   -------------------

   procedure Analyze_Un_Op (Node : Libadalang.Analysis.Ada_Node'Class)
   is
      use Langkit_Support;
      use A18n_Util;
      use type A.Ada_Node;

      Un_Op  : constant A.Un_Op           := Node.As_Un_Op;
      Op     : constant A.Op              := Un_Op.F_Op;
      Next_2 : constant A.Ada_Node        := Node.Next_Sibling;
      Next   : constant A.Ada_Node        := (if Next_2 = A.No_Ada_Node
                                              then A.No_Ada_Node
                                              else Next_2.Next_Sibling);
--      List  : constant A.Param_Spec_List := Node.As_Param_Spec_List;
   begin
      Put_Line ("Found: " & Node.Image);

      Put      ("  Line: " & Text.Image (Node.Text));
      if Verbose then
         Put (" at " & Location_Of (Node));
      end if;
      New_Line;

      Put_Line ("  Kind: "      & Node.Kind'Image);
      Put_Line ("    Op Kind: " & Op  .Kind'Image);
      if Next /= A.No_Ada_Node then
         Put_Line ("    Next K:  " & Next.Kind'Image);
      end if;
   end Analyze_Un_Op;

end A18n_Analysis;
