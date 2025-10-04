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

   --------------
   -- Un_Quote --
   --------------

   function Un_Quote (Item : String) return String is
   begin
      return Item (Item'First + 1 .. Item'Last - 1);
   end Un_Quote;

   -----------
   -- Quote --
   -----------

   function Quote (Item : String) return String is
   begin
      return """" & Item & """";
   end Quote;

   --------------
   -- Relative --
   --------------

   function Relative (Full : String;
                      Base : String) return String
   is
   begin
      return Ada.Strings.Fixed.Tail (Full, Full'Length - Base'Length - 1);
   end Relative;

end A18n_Util;
