with Libadalang.Analysis;

package A18n_Util is

   function Location_Of (Node : Libadalang.Analysis.Ada_Node'Class) return String;
   function Un_Quote (Item : String) return String;
   function Quote (Item : String) return String;

end A18n_Util;
