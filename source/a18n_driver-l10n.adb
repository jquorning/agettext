
with A18n_Util;

package body A18n_Driver.L10n is

   package Util renames A18n_Util;

   -----------------
   -- Driver_Name --
   -----------------

   function Driver_Name (Driver : Driver_Type) return String
   is ("l10n");

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (Driver : Driver_Type) return String
   is ("l10n");

   ----------------------
   -- Regular_Function --
   ----------------------

   function Regular_Function (Driver : Driver_Type) return String
   is ("gettext");

   --------------------
   -- Unary_Operator --
   --------------------

   function Unary_Operator (Driver : Driver_Type) return String
   is (Util.Quote ("abs"));

end A18n_Driver.L10n;
