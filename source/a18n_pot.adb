
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with A18n_Options;

package body A18n_POT
is
   package Option renames A18n_Options;

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

   procedure Put_Entry (Source_Name   : String;
                        Line_Number   : A18n_POT.Line_Number;
                        Column_Number : A18n_POT.Column_Number;
                        Text          : String;
                        Comment       : String := "")
   is
      use Ada.Strings;
   begin
      Put_Line (File, "");

      Put (File, "#: " & Source_Name &
                 ":" & Fixed.Trim (Line_Number'Image, Left));
      if Option.Columns then
         Put (File, ":" & Fixed.Trim (Column_Number'Image, Left));
      end if;
      New_Line (File);

      if Comment /= "" then
         Put_Line (File, "#  " & Comment);
      end if;

      Put_Line (File, "msgid  " & """" & Text & """");
      Put_Line (File, "msgstr " & """" & Text & """");
   end Put_Entry;

end A18n_POT;
