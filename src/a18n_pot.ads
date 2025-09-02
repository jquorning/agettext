
with Langkit_Support.Slocs;

package A18n_POT is

   subtype Line_Number   is Langkit_Support.Slocs.Line_Number;
   subtype Column_Number is Langkit_Support.Slocs.Column_Number;

   procedure Open (Filename : String);

   procedure Close;

   procedure Put_Entry (Source_Name   : String;
                        Line_Number   : A18n_POT.Line_Number;
                        Column_Number : A18n_POT.Column_Number;
                        Text          : String;
                        Comment       : String := "");

end A18n_POT;
