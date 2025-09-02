
with A18n_Driver.Intl;
with A18n_Driver.L10n;

package A18n_Options is

   package Driv renames A18n_Driver;

   type Driver_Kind is (
      Driver_Intl,       --  By Stephen Carrez
      Driver_L10n        --  By Jesper Quorning
      );

   --  'factory'
   Drivers : constant array (Driver_Kind) of Driv.Driver_Access :=
     (Driver_Intl => new A18n_Driver.Intl.Driver_Type,
      Driver_L10n => new A18n_Driver.L10n.Driver_Type);

   Used_Driver : Driver_Kind := Driver_Kind'First;

   Driver : Driv.Driver_Type'Class renames Drivers (Used_Driver).all;

end A18n_Options;
