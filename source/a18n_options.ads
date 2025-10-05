
with GNAT.Strings;

package A18n_Options is

   use GNAT.Strings;

   ---------------------------
   -- Command line switches --
   ---------------------------

   Columns    : aliased Boolean       := False;
   Debug      : aliased Boolean       := False;
   Driver_Str : aliased String_Access := null;
   Force      : aliased Boolean       := False;
   Help       : aliased Boolean       := False;
   Output     : aliased String_Access := null;
   Project    : aliased String_Access := null;
   Verbose    : aliased Boolean       := False;
   Version    : aliased Boolean       := False;

end A18n_Options;
