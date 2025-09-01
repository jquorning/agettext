with Ada.Strings.Fixed;

with Langkit_Support.Slocs;

package body A18n_Util is

   -----------------
   -- Location_Of --
   -----------------

   function Location_Of (Node : Libadalang.Analysis.Ada_Node'Class) return String
   is
      use Langkit_Support.Slocs;
      use Ada.Strings;

      Rang      : constant Source_Location_Range := Node.Sloc_Range;
      Line      : constant String := Fixed.Trim (Rang.Start_Line'Image,   Left);
      Col_Start : constant String := Fixed.Trim (Rang.Start_Column'Image, Left);
      Col_End   : constant String := Fixed.Trim (Rang.End_Column'Image,   Left);
   begin
      return Line & ":" & Col_Start & ".." & Col_End;
   end Location_Of;


end A18n_Util;
