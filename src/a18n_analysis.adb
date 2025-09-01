
with Ada.Text_IO;

with GNAT.Strings;

with Langkit_Support.Text;

with Libadalang.Common;

with A18n_Command_Line;
with A18n_Intl;
with A18n_POT;
with A18n_Util;

package body A18n_Analysis is

   package A   renames Libadalang.Analysis;
   package POT renames A18n_POT;

   use Ada.Text_IO;

   function Operator_In (Node : A.Ada_Node;
                         List : GNAT.Strings.String_List) return Boolean;

   Debug : Boolean renames A18n_Command_Line.Debug;

   -----------------------
   -- Analyze_Call_Expr --
   -----------------------

   procedure Analyze_Call_Expr (Node     : A.Ada_Node'Class;
                                Filename : String)
   is
      pragma Unreferenced (Filename);

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
      if Debug then
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

         if Debug then
            Put_Line ("  Is_Dot  : " & Name.P_Is_Dot_Call (False)'Image);
            Put_Line ("  Defining: " & Name.P_Is_Defining'Image);
            Put_Line ("  Direct  : " & Name.P_Is_Direct_Call'Image);
            Put_Line ("  Call    : " & Name.P_Is_Call'Image);
            Put_Line ("  Operator: " & Name.P_Is_Operator_Name'Image);
         end if;

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
--             Spec   : constant A.Base_Formal_Param_Holder := Name.P_Called_Subp_Spec;
               Params : constant A.Param_Actual_Array       := Name.P_Call_Params;
               Count  : Positive := 1;
            begin
               Put_Line (" Relat: " & Text.Image (Relat.Text));


               for Par of Params loop
                  declare
                     Defining   : constant A.Defining_Name'Class
                        := A.Param (Par);
                     Param_Name : constant String := Text.Image (Defining.F_Name.Text);
--                     Basic_Decl : constant A.Basic_Decl := Defining.P_Basic_Decl;

--                     Type_Name  : constant String
--                        := Text.Image (Basic_Decl.P_Fully_Qualified_Name);

--                     Base_Type  : constant A.Base_Type_Decl
--                        := A.Base_Type_Decl (Basic_Decl); --  .As_Base_Type_Decl;
                  begin
                     Put ("    " & Count'Image & "=> ");
                     Put (Param_Name);
--                     Put (" :: " & Type_Name);
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

   procedure Analyze_Un_Op (Node     : A.Ada_Node'Class;
                            Filename : String)
   is
      use Langkit_Support;
      use A18n_Util;
      use type A.Ada_Node;
      use type Libadalang.Common.Ada_Node_Kind_Type;

      First  : constant A.Ada_Node := Node.First_Child;
      Last   : constant A.Ada_Node := Node.Last_Child;
      Quoted : constant String     := Text.Image (Last.Text);
   begin
      Put_Line ("Found: " & Node.Image);

      Put      ("  Line: " & Text.Image (Node.Text));
      if Debug then
         Put (" at " & Location_Of (Node));
      end if;
      New_Line;

      Put_Line ("  Kind: "      & Node.Kind'Image);

      if Debug then
         Put_Line ("  Child F: " & Node.First_Child.Kind'Image);
         Put_Line ("  Child L: " & Node.Last_Child.Kind'Image);
         Put_Line ("  L Text : " & Text.Image (Node.Last_Child.Text));
      end if;

      if
        Last.Kind = Libadalang.Common.Ada_String_Literal and then
        Operator_In (First, A18n_Intl.Unary_Operators)
      then
         POT.Put_Entry
           (Source_Name => Filename,
            Line_Number => Natural (Node.Sloc_Range.Start_Line),
            Text        => Un_Quote (Quoted),
            Comment     => "");
      end if;

   end Analyze_Un_Op;

   -----------------
   -- Operator_In --
   -----------------

   function Operator_In (Node : A.Ada_Node;
                         List : GNAT.Strings.String_List) return Boolean
   is
      use Libadalang.Common;

      function In_List (Kind : Ada_Node_Kind_Type;
                        Op   : String) return Boolean;

      function In_List (Kind : Ada_Node_Kind_Type;
                        Op   : String) return Boolean
      is
         use type GNAT.Strings.String_Access;
      begin
         if Node.Kind /= Kind then
            return False;
         end if;

         for E of List loop
            return E.all = Op;
         end loop;
         return False;
      end In_List;

   begin
      return     In_List (Ada_Op_Minus, "-")
         or else In_List (Ada_Op_Plus,  "+")
         or else In_List (Ada_Op_Not,   "not")
         or else In_List (Ada_Op_Abs,   "abs")
         or else In_List (Ada_Op_Mod,   "mod")
         or else In_List (Ada_Op_Rem,   "ram");
   end Operator_In;

end A18n_Analysis;
