
with GNAT.Strings;

with A18n_Driver.Intl;
with A18n_Driver.L10n;

package A18n_Options is

   package Driv renames A18n_Driver;
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

   -----------------
   -- I18n driver --
   -----------------

   type Driver_Kind is (
      Driver_Intl,       --  By Stephane Carrez
      Driver_L10n        --  By Jesper Quorning
      );

   --  'factory'
   Drivers : constant array (Driver_Kind) of Driv.Driver_Access :=
     (Driver_Intl => new A18n_Driver.Intl.Driver_Type,
      Driver_L10n => new A18n_Driver.L10n.Driver_Type);

   Used_Driver : Driver_Kind := Driver_Kind'First;

end A18n_Options;
