
package A18n_Driver.Intl is

   type Driver_Type is new A18n_Driver.Driver_Type
      with null record;

   function Driver_Name (Driver : Driver_Type) return String;
   function Package_Name (Driver : Driver_Type) return String;
   function Regular_Function (Driver : Driver_Type) return String;
   function Unary_Operator (Driver : Driver_Type) return String;

end A18n_Driver.Intl;
