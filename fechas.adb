with Ada.Strings.Fixed;

use Ada.Strings.Fixed;
use Ada.Strings;

package body Fechas is

   function Imagen (F: Fecha) return String_fecha is

      procedure Mover_Imagen_Positive (N: Positive; S: in out String) is
      begin

         -- Move copia un string en otro de otro tamaño, añadiendo un
         -- padding opcionalmente. Trim elimina los blancos a izquierda o
         -- derecha (en este caso el blanco que pone el 'Image).
         Move
           (Source  => Trim (Positive'Image (N), Left),
            Target  => S,
            Justify => Right,
            Pad     => '0');
      end Mover_Imagen_Positive;

      S_Fecha : String_Fecha;
   begin

      Mover_Imagen_Positive (F.Dia, S_Fecha (1..2));
      S_Fecha (3) := '-';
      Mover_Imagen_Positive (F.Mes, S_Fecha (4..5));
      S_Fecha (6) := '-';
      Mover_Imagen_Positive (F.Anno, S_Fecha (7..10));

      return S_Fecha;

   end Imagen;

end Fechas;
