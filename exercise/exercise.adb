
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Text_IO;

with Intl;
--  with L10n;

procedure Exercise
is
   package L10n renames Intl;

   use Intl;


   function "abs" (Item : String) return String
      renames Intl.Gettext;

   procedure Put_Line (Item : String);

   procedure Put_Line (Item : String)
   is
      use Ada.Strings.UTF_Encoding.Strings;
   begin
      Ada.Text_IO.Put_Line (Decode (Item));
   end Put_Line;

   B : constant String           := "Apple";
   D : constant String           := -"Pear";
   G : constant String           := "-"("Banana");
   H : constant String           := Intl."-"("Cherry");
   R : constant String           := -B;
   E : constant String           := L10n.Gettext ("House");
   K : constant String           := Intl.Gettext ("Flat");
   L : constant String           := abs "Base text";
   M : constant String           := abs ("Enhanced text 1");
   P : constant String           := abs ("Enhanced text 2");
   N : constant String           := abs "Long stretch";
   S : constant String           := L10n.Gettext ("dotted l10n");
begin
   Intl.Initialize ("exercise", ".");
   Put_Line (Intl.Current_Locale);
   Put_Line (L10n.Gettext (B));
   Put_Line (-B);
   Put_Line (-"Cucumber");
   Put_Line ("-"(B));
   Put_Line ("-"("Leek"));
   Put_Line (Intl."-" ("Cale"));
   Put_Line (Standard.Intl."-" ("Sprout"));
   Put_Line (abs "Another language");
   Put_Line (abs "..and again");
   Put_Line (abs ("Another time 1"));
   Put_Line (abs ((("Another time 2"))));
   Put_Line (D);
   Put_Line (G);
   Put_Line (H);
   Put_Line (R);
   Put_Line (E);
   Put_Line (K);
   Put_Line (L);
   Put_Line (M);
   Put_Line (P);
   Put_Line (N);
   Put_Line (S);
end Exercise;
