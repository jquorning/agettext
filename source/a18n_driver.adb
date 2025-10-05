
with Ada.Directories;
with Ada.Text_IO;

with Resources;
with Agettext_Config;

with Input_Sources.File;

with DOM.Readers;
with DOM.Core.Documents;
with DOM.Core.Nodes;

package body A18n_Driver
is
   use Ada.Strings.Unbounded;

   package Resource is new
      Resources (Agettext_Config.Crate_Name);

   Driver_Name_US  : Unbounded_String;
   Package_Name_US : Unbounded_String;

   procedure Walk_Interface (Node : DOM.Core.Node);
   procedure Walk_Driver (Node : DOM.Core.Node);
   procedure Walk_Root (Node : DOM.Core.Node);

   --------------------
   -- Walk_Interface --
   --------------------

   procedure Walk_Interface (Node : DOM.Core.Node)
   is
      use type DOM.Core.Node;

      Nw : DOM.Core.Node := Node;
   begin
      while Nw /= null loop
         case Nw.Node_Type is

         when DOM.Core.Element_Node =>
            if "subprogram" /= DOM.Core.Nodes.Node_Name (Nw) then
               raise XML_Error with "expected subprogram";
            end if;
            declare
               use DOM.Core;

               Sr            : Subprogram_Record;
               Got_Name      : Boolean := False;
               Got_Kind      : Boolean := False;
               Got_Arg_Count : Boolean := False;
               As : constant Named_Node_Map := Nodes.Attributes (Nw);
            begin
               for A in 0 .. Nodes.Length (As) - 1 loop
                  declare
                     Key   : constant String := Nodes.Node_Name  (Nodes.Item (As, A));
                     Value : constant String := Nodes.Node_Value (Nodes.Item (As, A));
                  begin
                     if Key = "name" then
                        Sr.Name  := To_Unbounded_String (Value);
                        Got_Name := True;

                     elsif Key = "type" then
                        Sr.Kind  := Subprogram_Kind'Value ("sk_" & Value);
                        Got_Kind := True;

                     elsif Key = "arg_count" then
                        Sr.Arg_Count  := Natural'Value (Value);
                        Got_Arg_Count := True;
                     end if;
                  end;
               end loop;

               if Got_Name and Got_Kind and Got_Arg_Count then
                  Subprograms.Append (Sr);
               else
                  raise XML_Error with "missing attribute";
               end if;
            end;

         when others => null;
         end case;
         Nw := DOM.Core.Nodes.Next_Sibling (Nw);
      end loop;
   end Walk_Interface;

   -----------------
   -- Walk_Driver --
   -----------------

   procedure Walk_Driver (Node : DOM.Core.Node)
   is
      use type DOM.Core.Node;

      Nw : DOM.Core.Node := Node;
   begin
      while Nw /= null loop
         case Nw.Node_Type is

         when DOM.Core.Element_Node =>
            declare
               use DOM.Core;

               As : constant Named_Node_Map := Nodes.Attributes (Nw);
            begin
               for A in 0 .. Nodes.Length (As) - 1 loop
                  declare
                     Key   : constant String := Nodes.Node_Name  (Nodes.Item (As, A));
                     Value : constant String := Nodes.Node_Value (Nodes.Item (As, A));
                  begin
                     if Key = "package" then
                        Package_Name_US := To_Unbounded_String (Value);
                     end if;
                  end;
               end loop;
            end;
            Walk_Interface (DOM.Core.Nodes.First_Child (Nw));

         when others => null;
         end case;
         Nw := DOM.Core.Nodes.Next_Sibling (Nw);
      end loop;
   end Walk_Driver;

   ---------------
   -- Walk_Root --
   ---------------

   procedure Walk_Root (Node : DOM.Core.Node)
   is
      use type DOM.Core.Node;

      Nw : DOM.Core.Node := Node;
   begin
      while Nw /= null loop
         case Nw.Node_Type is

         when DOM.Core.Element_Node =>
            declare
               use DOM.Core;

               As : constant Named_Node_Map := Nodes.Attributes (Nw);
            begin
               for A in 0 .. Nodes.Length (As) - 1 loop
                  declare
                     Key   : constant String := Nodes.Node_Name  (Nodes.Item (As, A));
                     Value : constant String := Nodes.Node_Value (Nodes.Item (As, A));
                  begin
                     if Key = "name" then
                        Driver_Name_US := To_Unbounded_String (Value);
                     end if;
                  end;
               end loop;
            end;
            Walk_Driver (DOM.Core.Nodes.First_Child (Nw));

         when others =>
            null;
         end case;
         Nw := DOM.Core.Nodes.Next_Sibling (Nw);
      end loop;
   end Walk_Root;

   ----------
   -- Load --
   ----------

   procedure Load (Driver : String)
   is
      use Ada.Directories;

      Filename_2 : constant String :=
        Resource.Resource_Path & "/driver/" & Driver & ".drv";

      Filename : constant String :=
        (if Exists (Filename_2)
         then Filename_2
         else Driver);

      File : Input_Sources.File.File_Input;
      Tree : DOM.Readers.Tree_Reader;
   begin
      Input_Sources.File.Open (Filename, File);
      DOM.Readers.Parse (Tree, File);

      declare
         Doc  : constant DOM.Core.Document := DOM.Readers.Get_Tree (Tree);
         Root : constant DOM.Core.Element  := DOM.Core.Documents.Get_Element (Doc);
      begin
         Walk_Root (Root);
      end;

      DOM.Readers.Free (Tree);
      Input_Sources.File.Close (File);

   exception
      when Ada.Text_IO.Name_Error =>
         raise XML_Error with "could not open file";
   end Load;

   -----------------
   -- Driver_Name --
   -----------------

   function Driver_Name return String
      is (Ada.Strings.Unbounded.To_String (Driver_Name_US));

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name return String
      is (Ada.Strings.Unbounded.To_String (Package_Name_US));

end A18n_Driver;
