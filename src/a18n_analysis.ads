
with GNATCOLL.Projects;

with Libadalang.Analysis;
with Libadalang.Project_Provider;

package A18n_Analysis is

   package A  renames Libadalang.Analysis;
   package GP renames GNATCOLL.Projects;
   package PP renames Libadalang.Project_Provider;

   Missing_Driver_Specification : exception;

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

end A18n_Analysis;
