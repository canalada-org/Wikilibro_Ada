--                              -*- Mode: Ada -*-
--  Filename        : problema_filosofos.adb
--  Description     : Problema de los filósofos pensantes
--  Author          : Manuel
--  Created On      : Sat Dec 10 23:19:00 2011
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Cubiertos; use Cubiertos;

procedure Problema_Filosofos is

   type PCubierto is access Cubierto;

   task type TFilosofo(Id: Character; Cubierto1: PCubierto; Cubierto2: PCubierto);

   task body TFilosofo is

      procedure Comer is
      begin
         Coger(Cubierto1.all);
         Coger(Cubierto2.all);
         for i in 1..10 loop
            Put(Id & "c ");
            delay 1.0;
         end loop;
         Soltar(Cubierto2.all);
         Soltar(Cubierto1.all);
      end Comer;

      Procedure Pensar is
      begin
         for i in 1..10 loop
            Put(Id & "p ");
            delay 1.0;
         end loop;
      end Pensar;

   begin
      loop
         Comer;
         Pensar;
      end loop;
   end TFilosofo;

   Num_Cubiertos: Positive;

begin

   Put("Introduce el numero de cubiertos: "); Get(Num_Cubiertos); New_line;

   declare
      type PTFilosofo is access TFilosofo;
      P: PTFilosofo;
      C: Character := 'A';
      Cuberteria: array (1..Num_Cubiertos) of PCubierto;
   begin
      for i in 1..Num_Cubiertos loop
         Cuberteria(i) := new Cubierto;
      end loop;

      for i in 1..Num_Cubiertos-1 loop
        P := new TFilosofo(C, Cuberteria(i), Cuberteria(i+1));
        C := Character'Succ(C);
      end loop;
      P := new TFilosofo(C, Cuberteria(1), Cuberteria(Num_Cubiertos));
   end;

end Problema_Filosofos;
