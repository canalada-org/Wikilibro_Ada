package Fechas is

   type Fecha is
      record
         Dia : Positive range 1 .. 31;
         Mes : Positive range 1 .. 12;
         Anno : Positive range 1 .. 3000;
      end record;

   subtype String_Fecha is String (1..10);

   -- Pasa la fecha a string en formato "dd-mm-aaaa".
   --
   function Imagen (F: Fecha) return String_Fecha;

end Fechas;
