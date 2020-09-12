with Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

-- Imprime los argumentos pasados por línea de comandos

procedure Imprimir_Argumentos is

begin

  Ada.Text_IO.Put_Line ("Imprimiendo argumentos pasados a " &
      Command_Name & '.');

  for I in 1 .. Argument_Count loop

      Ada.Text_IO.Put_Line
        ("Argumento nº" & Integer'Image (I) & ": " &
         Argument (I));

  end loop;

  Set_Exit_Status (Success);

exception
   when others =>
      Set_Exit_Status (Failure);
end Imprimir_Argumentos;
