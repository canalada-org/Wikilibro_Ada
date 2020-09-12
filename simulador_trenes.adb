with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random;

with Semaforos;

procedure Simulador_Trenes is

   Num_Estaciones : constant := 5;
   Num_Trenes : constant := 3;

   type Num_Estacion is range 1 .. Num_Estaciones;
   type Num_Tren is range 1 .. Num_Trenes;

   package Num_Estacion_IO is new Ada.Text_IO.Integer_IO (Num_Estacion);
   use Num_Estacion_IO;
   package Num_Tren_IO is new Ada.Text_IO.Integer_IO (Num_Tren);
   use Num_Tren_IO;

   package Semaforos_Inicial_1 is new
     Semaforos (Valorinicial => 1);
   use Semaforos_Inicial_1;

   Semaforos_Estaciones : array (Num_Estacion) of TSemaforo;

   task type Tren is
      entry Comenzar (Tu_Num : in Num_Tren);
   end Tren;

   Lista_Trenes : array (Num_Tren) of Tren;

   task body Tren is

      Mi_Num: Num_Tren;

      procedure Pon_Nombre is
      begin
         Put ("Tren nÂº"); Put (Mi_Num); Put (": ");
      end Pon_Nombre;

      Espera_En_Estacion: constant Duration := 5.0;
      Duracion_Minima: constant Duration := 2.0;
      Factor_Duracion: constant Duration := 10.0;
      Azar_Gen: Ada.Numerics.Float_Random.Generator;

      Actual, Siguiente: Num_Estacion;
   begin

      Ada.Numerics.Float_Random.Reset (Azar_Gen);

      accept Comenzar (Tu_Num : in Num_Tren) do
         Mi_Num := Tu_Num;
      end Comenzar;

      Pon_Nombre;
      Put_Line ("Comienzo el trayecto");
      Actual := 1;

      loop

         Pon_Nombre; Put ("En estaciÃ³n "); Put (Actual); New_Line;

         delay Espera_En_Estacion;

         if Actual = Num_Estaciones then
            Siguiente := 1;
         else
            Siguiente := Actual + 1;
         end if;

         Wait (Semaforos_Estaciones (Siguiente));

         Pon_Nombre;
         Put ("Trayecto hacia estaciÃ³n ");
         Put (Siguiente);
         New_Line;

         Signal (Semaforos_Estaciones (Actual));
         delay Duration (Ada.Numerics.Float_Random.Random (Azar_Gen))
           * Factor_Duracion + Duracion_Minima;

         Actual := Siguiente;

      end loop;
   end Tren;


begin

   for I in Lista_Trenes'Range loop
      Lista_Trenes (I).Comenzar (Tu_Num => I);
   end loop;

end Simulador_Trenes;
