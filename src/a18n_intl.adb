
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

   function Regular_Function return String is
   begin
      return "gettext";
   end Regular_Function;

   ---------------------
   -- Unary_Operators --
   ---------------------

   function Unary_Operator return String is
   begin
      return "-";
   end Unary_Operator;

end A18n_Intl;
