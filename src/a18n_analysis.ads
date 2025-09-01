
with Libadalang.Analysis;

package A18n_Analysis is

   procedure Analyze_Call_Expr (Node     : Libadalang.Analysis.Ada_Node'Class;
                                Filename : String);
   procedure Analyze_Un_Op     (Node     : Libadalang.Analysis.Ada_Node'Class;
                                Filename : String);

end A18n_Analysis;
