
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body A18n_POT
is
   use Ada.Text_IO;

   File : File_Type;

   ----------
   -- Open --
   ----------

   procedure Open (Filename : String)
   is
      use Ada.Directories;
   begin
      if Exists (Filename) then
         Open (File, Out_File, Filename);
      else
         Create (File, Out_File, Filename);
      end if;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Close (File);
   end Close;

   ---------------
   -- Put_Entry --
   ---------------

   procedure Put_Entry (Source_Name : String;
                        Line_Number : Natural;
                        Text        : String;
                        Comment     : String := "")
   is
      use Ada.Strings;
   begin
      Put_Line (File, "");
      Put_Line (File, "#: " & Source_Name &
                      ": " & Fixed.Trim (Line_Number'Image, Left));
      if Comment /= "" then
         Put_Line (File, "#  " & Comment);
      end if;

      Put_Line (File, "msgid  " & """" & Text & """");
      Put_Line (File, "msgstr " & """" & Text & """");
   end Put_Entry;

end A18n_POT;
