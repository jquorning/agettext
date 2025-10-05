
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package A18n_Driver is

   XML_Error : exception;

   type Subprogram_Kind is (Sk_Function, Sk_Procedure, Sk_Unary);

   type Subprogram_Record is
      record
         Name      : Ada.Strings.Unbounded.Unbounded_String;
         Kind      : Subprogram_Kind;
         Arg_Count : Natural;
      end record;

   package Subprogram_Vectors is new
      Ada.Containers.Vectors (Index_Type   => Positive,
                              Element_Type => Subprogram_Record);

   Subprograms : Subprogram_Vectors.Vector;

   function Driver_Name  return String;
   function Package_Name return String;
   --  function Regular_Function (Driver : Driver_Type) return String is abstract;
   --  function Unary_Operator (Driver : Driver_Type) return String   is abstract;
   procedure Load (Driver : String);

end A18n_Driver;
