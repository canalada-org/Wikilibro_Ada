with Ada.Text_IO; use Ada.Text_IO;

with Ada.Numerics.Discrete_Random;

procedure Barberia is

   type Rango_Demora is range 1 .. 30;
   type Duracion_Afeitado is range 5 .. 10;
   type Nombre_Cliente is (Jose, Juan, Aitor, Antonio, Camilo);

   package Demora_Al_Azar is new Ada.Numerics.Discrete_Random
     (Rango_Demora);
   package Afeitado_Al_Azar is new Ada.Numerics.Discrete_Random
     (Duracion_Afeitado);

   task Barbero is
      entry Afeitar (Cliente : in Nombre_Cliente);
   end Barbero;

   task type Cliente is
      entry Comenzar (Nombre : in Nombre_Cliente);
   end Cliente;

   Lista_Clientes : array (Nombre_Cliente) of Cliente;

   task body Barbero is
      Generador : Afeitado_Al_Azar.Generator;
      Espera_Maxima_Por_Cliente : constant Duration := 30.0;
   begin
      Afeitado_Al_Azar.Reset (Generador);
      Put_Line ("Barbero: Abro la barbería.");
      loop
         Put_Line ("Barbero: Miro si hay cliente.");
         select
            accept Afeitar (Cliente : in Nombre_Cliente) do
               Put_Line ("Barbero: Afeitando a " & Nombre_Cliente'Image
                         (Cliente));
               delay Duration (Afeitado_Al_Azar.Random (Generador));
               Put_Line ("Barbero: Termino con " & Nombre_Cliente'Image
                         (Cliente));
            end Afeitar;
         or
            delay Espera_Maxima_Por_Cliente;
            Put_Line ("Barbero: Parece que ya no viene nadie,"
                      & " cierro la barbería.");
            exit;
         end select;
      end loop;
   end Barbero;

   task body Cliente is
      Generador : Demora_Al_Azar.Generator;
      Mi_Nombre : Nombre_Cliente;
   begin
      accept Comenzar (Nombre : in Nombre_Cliente) do
         Mi_Nombre := Nombre;
      end Comenzar;

      Demora_Al_Azar.Reset (Gen       => Generador,
                            Initiator => Nombre_Cliente'Pos (Mi_Nombre));

      delay Duration (Demora_Al_Azar.Random (Generador));

      Put_Line (Nombre_Cliente'Image (Mi_Nombre) &
                ": Entro en la barbería.");
      Barbero.Afeitar (Cliente => Mi_Nombre);
      Put_Line (Nombre_Cliente'Image (Mi_Nombre) &
                ": Estoy afeitado, me marcho.");
   end Cliente;

begin
   for I in Lista_Clientes'Range loop
      Lista_Clientes (I).Comenzar (Nombre => I);
   end loop;
end Barberia;
