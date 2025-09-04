
with Ada.Text_IO;

with Intl;
with L10n;

procedure Exercise
is
   use Ada.Text_IO;

   use Intl;
   use L10n;

   function "+" (Item : String) return String
      renames L10n."abs";

   B : constant String           := "World";
   D : constant String           := -"Computer 1";
   G : constant String           := "-"("Computer 2");
   H : constant String           := Intl."-"("Computer 3");
   R : constant String           := -B;
   E : constant String           := L10n.Gettext ("macOS 1");
   K : constant String           := Intl.Gettext ("macOS 2");
   L : constant String           := abs "Base text";
   M : constant String           := abs ("Enhanced text 1");
   P : constant String           := L10n."abs" ("Enhanced text 2");
   N : constant String           := +"Long stretch";
   S : constant String           := L10n.Gettext ("dotted l10n");
begin
   Put_Line (L10n.Gettext (B));
   Put_Line (-B);
   Put_Line (-"Ada 1");
   Put_Line ("-"(B));
   Put_Line ("-"("Ada 2"));
   Put_Line (Intl."-" ("Summa sumarum 1"));
   Put_Line (Standard.Intl."-" ("Summa sumarum 2"));
   Put_Line (abs "Et andet sprog");
   Put_Line (abs "..og igen");
   Put_Line (abs ("Endnu engang 1"));
   Put_Line (abs ((("Endnu engang 2"))));
end Exercise;
