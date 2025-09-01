package A18n_POT is


   procedure Open (Filename : String);

   procedure Close;

   procedure Put_Entry (Source_Name : String;
                        Line_Number : Natural;
                        Text        : String;
                        Comment     : String := "");

end A18n_POT;
