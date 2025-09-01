
--  with A18n_Util;

package body A18n_Intl is

--   package Util renames A18n_Util;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name return String
   is ("intl");

   -----------------------
   -- Regular_Functions --
   -----------------------

   function Regular_Functions return GNAT.Strings.String_List is
   begin
      return (1 => new String'("gettext"));
   end Regular_Functions;

   ---------------------
   -- Unary_Operators --
   ---------------------

   function Unary_Operators return GNAT.Strings.String_List is
   begin
      return (1 => new String'("-"));
   end Unary_Operators;

end A18n_Intl;
