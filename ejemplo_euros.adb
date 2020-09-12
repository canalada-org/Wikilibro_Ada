with Ada.Text_IO.Editing;

procedure Ejemplo_Euros is

   type T_Precio_En_Euros is delta 0.01 digits 6;

   package Euros_IO is new Ada.Text_IO.Editing.Decimal_Output
     (Num                => T_Precio_En_Euros,
      Default_Currency   => "EUR ",
      Default_Fill       => ' ',
      Default_Separator  => '.',
      Default_Radix_Mark => ',');

   Un_Precio : constant T_Precio_En_Euros := 5873.26;
begin

   Ada.Text_IO.Put_Line
      (Euros_IO.Image
         (Item => Un_Precio,
          Pic  => Ada.Text_IO.Editing.To_Picture
                    ("#_###_###_##9.99")));
end Ejemplo_Euros;
