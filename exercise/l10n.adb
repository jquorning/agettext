package body L10n is

   function "abs" (Message : String) return String
   is (Message);

   function Gettext (Message : String) return String
      renames "abs";

end L10n;
