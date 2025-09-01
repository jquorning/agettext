
with Libadalang.Analysis;

package A18n_Analysis is

   package A renames Libadalang.Analysis;

   procedure Analyze_Call_Expr (Node : A.Ada_Node'Class);
   procedure Analyze_Un_Op     (Node : A.Ada_Node'Class);

end A18n_Analysis;
