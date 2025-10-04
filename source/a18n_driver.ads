
package A18n_Driver is

   type Driver_Type is interface;
   type Driver_Access is access all Driver_Type'Class;

   function Driver_Name (Driver : Driver_Type) return String      is abstract;
   function Package_Name (Driver : Driver_Type) return String     is abstract;
   function Regular_Function (Driver : Driver_Type) return String is abstract;
   function Unary_Operator (Driver : Driver_Type) return String   is abstract;

end A18n_Driver;
