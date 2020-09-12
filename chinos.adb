-- Chinos2: Otra implementación concurrente en Ada
-- Tomás Javier Robles Prado
-- tjavier@usuarios.retecal.es


-- Uso: ./chinos <numero_jugadores>

-- El juego consiste en jugar sucesivas partidas a los chinos. Si un
--  jugador acierta, no paga y queda excluido de las siguientes
--  rondas. El último que quede paga los vinos


--   Copyright (C) 2003 T. Javier Robles Prado
--
--   This program is free software; you can redistribute it and/or modify
--   it under the terms of the GNU General Public License as published by
--   the Free Software Foundation; either version 2 of the License, or
--   (at your option) any later version.
--
--   This program is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
--   GNU General Public License for more details.
--
--   You should have received a copy of the GNU General Public License
--   along with this program; if not, write to the Free Software
--   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


with Ada.Text_IO; use Ada.Text_IO;

with Ada.Numerics.Discrete_Random;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Exceptions;

procedure Chinos is

   -- Número Máximo Jugadores que pueden participar (aforo máximo del bar)
   MAX   : constant Natural := 20;

   -- Posibles mensajes que recibe un jugador tras una partida
   type Estados is (NO_SIGUES_JUGANDO, SIGUES_JUGANDO, HAS_PERDIDO);

   -- Subtipo que modela el número de jugadores posibles

   subtype NumMaxJugadores is Natural range 0..MAX;

   -- Modela la máxima apuesta que puede darse
   subtype MAX_APUESTA is Natural range 0..3*MAX;

   -- Nombres posibles para los jugadores. El 0 se utilizará para

   --  controlar el caso de que no haya ganador en una partida
   subtype TNombre is Integer range -1..MAX;

   -- Paquete para Numeros aleatorios:
   package Integer_Random is new Ada.Numerics.Discrete_Random(MAX_APUESTA);

   -- Apuesta de cada Jugador

   Subtype TApuesta is  Integer range -1..3*MAX;

   -- Mano de cada jugador
   subtype TMano is Natural range 0..3;


   -- Ficha de cada jugador que guardara el arbitro

   type TFicha is record
      Nombre       : TNombre;
      Apuesta      : TApuesta := -1;
      Mano         : TMano;
      SigueJugando : Boolean;
   end record;

   -- Array de Fichas
   type TTablon is array(1..MAX) of TFicha;


   -- Se define el tipo jugador

   task type Jugador;

   task  Arbitro is
      -- El árbitro controla las partidas y sincroniza a los jugadores

      entry FijaNumeroJugadores (Num : in NumMaxJugadores);
      -- Recoge el argumento de la línea de comandos para saber

      --  cuántos jugadores van a participar

      entry AsignaNombre (Nombre: out TNombre; NumJug: out NumMaxJugadores);
      -- Asigna Nombres (de 1 a NumerosJugadores) a los jugadores que

      --  van a participar. A los que no, les asigna un -1 como
      --  indicación de que finalicen.

      entry SiguesJugando
        (Nombre: in TNombre;
         JugadorSigueJugando : out Estados;
         HuboGanador : out boolean);
      -- Mensaje que envía el árbitro a cada jugador tras una

      --  partida, comunicándole si ha ganado y deja de jugar, si
      --  sigue jugando o si ha perdido y tiene que pagar

      entry EnviaApuesta (Nombre: in TNombre ; Apuesta: in TApuesta);
      -- El árbitro recibe la apuesta de un jugador


      entry ConfirmaApuesta (Confirmada : out Boolean);
      -- Respuesta del árbitro sobre si la apuesta es válida (no la
      --  ha hecho otro antes)


      entry ReEnviaApuesta (Apuesta: in TApuesta);
      -- Si la apuesta no es válida se reenvia hasta que lo sea

      entry EnviaMano (Nombre: in TNombre ; Mano: in TMano);
      -- El jugador envía el número de manos que saca al árbitro


   end Arbitro;




   task body Arbitro is

      -- Funciones y Procedimientos

      function NumeroJugadores return NumMaxJugadores is

         -- Devuelve el número de jugadores
      begin
         return 5;
      end NumeroJugadores;

      function EsApuestaValida (Apuesta: in TApuesta; Tablon: in TTablon)
                               return Boolean is

         -- Devuelve verdadero si la apuesta no ha sido realizada
         --  antes por algún otro jugador

         Valida : Boolean := True ;
         I      : TNombre := 1;
      begin

         for I in 1..MAX loop

            if Tablon(I).SigueJugando then
               if Tablon(I).Apuesta = Apuesta  then
                  -- Ya está dicha, la apuesta NO es válida

                  Valida := False ;
               end if;
            end if;
         end loop;

         return Valida;
      end EsApuestaValida;

      function ResultadoGanador (Tablon: in TTablon) return TApuesta is

         -- Devuelve el número de monedas que sacaron los jugadores
         Suma : TApuesta := 0 ;
      begin
         for I in 1..MAX loop

            if Tablon(I).SigueJugando then
               Suma := Suma + Tablon(I).Mano ;
            end if;
         end loop;
         return Suma;
      end ResultadoGanador;

      procedure ImprimeGanador (Tablon: in TTablon) is

         -- Imprimer el nombre del ganador
         I         : TNombre := 1 ;
         Resultado : TApuesta ;
         Terminar  : Boolean := False;
      begin
         Resultado := ResultadoGanador(Tablon);
         while not Terminar loop

            if Tablon(I).Apuesta = Resultado and Tablon(I).SigueJugando then

               Put_Line("Ha Ganado el Jugador " & I'Img);
               Terminar := True ;
            else
               if I = MAX then
                  Put_Line("No ha habido Ganador");
                  Terminar := True;
               else

                  I :=  I + 1;
               end if;
            end if;

         end loop;
      end ImprimeGanador;



      function JugadorEliminado (Tablon: in TTablon) return NumMaxJugadores is

         -- Devuelve el jugador que cuya apuesta sea la correcta

         Resultado : TApuesta;


         Ganador   : NumMaxJugadores := 0;
      begin
         Resultado := ResultadoGanador(Tablon);

         for I in 1..MAX loop

            if Tablon(I).SigueJugando then

               if Resultado =  Tablon(I).Apuesta then
                  Ganador := I ;
               end if;
            end if;
         end loop;

         return Ganador;
      end JugadorEliminado;



      procedure ImprimeTablon(Tablon: in TTablon) is

         -- Imprime las apuestas y monedas de los jugadores
      begin

         for I in 1..MAX loop
            if Tablon(I).SigueJugando then

               Put_Line("Nombre =" & Tablon(I).Nombre'Img &
                                " | Apuesta =" & Tablon(I).Apuesta'Img &
                                " | Mano =" &Tablon(I).Mano'Img );
            end if;
         end loop;
         Put_Line
           ("Resultado ganador: " & ResultadoGanador(Tablon)'Img);

      end ImprimeTablon;


      procedure SeparaPartidas (NumPar :in Natural)  is

         -- Un simple separador para aumentar la claridad
      begin
         New_Line;
         Put_Line("******************************************");
         Put_Line("Partida número " & NumPar'Img);
         Put_Line("******************************************");
      end SeparaPartidas;



      -- Variables


      -- Número de jugadores de la partida
      N : NumMaxJugadores;
      Permitidos : NumMaxJugadores;

      -- Partida Actual
      PartidaActual : NumMaxJugadores;

      -- Tablón
      Tablon : TTablon;

      NombreActual : NumMaxJugadores;
      ApuestaValida : Boolean;
      Ganador : NumMaxJugadores;

      NumeroPartida :  Natural;

   begin

      -- Averigua número de jugadores

      accept FijaNumeroJugadores (Num : in NumMaxJugadores) do
         N := Num;
      end FijaNumeroJugadores;

      -- Nombra solo a aquellos que vayan a jugar, a los que no, los

      --  nombra como -1
      Permitidos := N;


      for I in 1..MAX loop
         accept AsignaNombre

           (Nombre: out TNombre ; NumJug: out NumMaxJugadores) do
            if Permitidos > 0 then

               Nombre := I;
               NumJug := N;
               Tablon(I).Nombre  := I ;
               Tablon(I).SigueJugando  := True;
               Permitidos := Permitidos - 1;
            else
               Nombre := -1;
               Tablon(I).Nombre := -1;
               Tablon(I).SigueJugando := False;


            end if;
         end AsignaNombre;
      end loop;

      NumeroPartida := 1;

      while N /= 1 loop

         -- Para separar las diferentes partidas
         SeparaPartidas(NumeroPartida);


         -- Recibe las apuestas de cada jugador
         for I in 1..N loop
            accept EnviaApuesta (Nombre: in TNombre; Apuesta: in TApuesta) do

               NombreActual := Nombre;
               ApuestaValida := EsApuestaValida(Apuesta,Tablon);
               if ApuestaValida then
                  Tablon(Nombre).Apuesta := Apuesta ;
               end if;
            end EnviaApuesta;

            -- La Apuesta es Válida, se confirma y a otra cosa

            if ApuestaValida then
               accept ConfirmaApuesta(Confirmada: out Boolean) do

                  Confirmada := True;
               end ConfirmaApuesta;

            else
               -- La apuesta no es válida. Se comunica esto al jugador para
               -- que envíe una nueva apuesta
               accept ConfirmaApuesta(Confirmada: out Boolean) do

                  Confirmada := False;
               end ConfirmaApuesta;
               while not ApuestaValida loop
                  -- Aceptará diferentes apuestas hasta q sea válida.

                  accept ReEnviaApuesta (Apuesta: in TApuesta) do
                     if EsApuestaValida(Apuesta,Tablon) then

                        ApuestaValida := True;
                        Tablon(NombreActual).Apuesta := Apuesta ;
                     end if;
                  end ReEnviaApuesta;
                  accept ConfirmaApuesta(Confirmada: out Boolean) do

                     Confirmada := ApuestaValida;
                  end ConfirmaApuesta;
               end loop;

            end if;
         end loop;

         -- Recibe lo q saca cada jugador

         for I in 1..N loop
            accept EnviaMano(Nombre: in TNombre; Mano: in TMano) do

               Tablon(Nombre).Mano := Mano ;
            end EnviaMano;
         end loop;

         -- ImprimeResultados de la partida
         ImprimeTablon(Tablon);
         ImprimeGanador(Tablon);


         -- Envía a cada jugador su nuevo estado
         Ganador  := JugadorEliminado (Tablon);
         if Ganador = 0 then

            -- Nadie acertó
            for I in 1..N loop
               accept SiguesJugando

                 (Nombre: in TNombre;
                  JugadorSigueJugando : out Estados;
                  HuboGanador : out boolean) do

                  JugadorSigueJugando := SIGUES_JUGANDO;
                  Tablon(Nombre).SigueJugando := True;
                  HuboGanador := false ;
               end SiguesJugando;
            end loop;

         else
            -- Hay ganador
            for I in 1..N loop

               accept SiguesJugando
                 (Nombre: in TNombre;
                  JugadorSigueJugando : out Estados;
                  HuboGanador : out boolean) do

                  HuboGanador := true;
                  if Nombre = Ganador then
                     JugadorSigueJugando := NO_SIGUES_JUGANDO;
                     Tablon(Nombre).SigueJugando := False;
                  else
                     if N /= 2 then

                        JugadorSigueJugando := SIGUES_JUGANDO;
                        Tablon(Nombre).SigueJugando := True;
                     else
                        JugadorSigueJugando := HAS_PERDIDO;
                        Tablon(Nombre).SigueJugando := False;
                     end if;
                  end if;
               end SiguesJugando;
            end loop;
         end if;


         NumeroPartida := NumeroPartida + 1;
         if Ganador /= 0 then

            N := N - 1;
         end if;


      end loop;

   end Arbitro;


   task body Jugador is

      MiNombre : TNombre;
      NumJug   : NumMaxJugadores;
      Apuesta : TApuesta;
      ApuestaValidada : Boolean;
      Mano    : Tmano;
      G : Integer_Random.Generator;
      YoSigo : Estados;
      Terminar : Boolean := False;
      HuboGanador : boolean;

   begin
      Arbitro.AsignaNombre(MiNombre, NumJug);


      -- Si MiNombre es -1, entonces termina su ejecución. Se sigue
      --  este método para ceñirnos a los jugadores que quiere el
      --  usuario
      if MiNombre /= -1 then

         -- Semillas aleatorias
         Integer_Random.Reset(G);

         while not Terminar loop

            -- Envia Apuesta
            for I in 1..MiNombre loop

               Apuesta := Integer_Random.Random(G) mod (NumJug * 3);
            end loop;
            Arbitro.EnviaApuesta(MiNombre, Apuesta);

            -- Proceso de confirmación de apuesta
            ApuestaValidada := False ;
            while not ApuestaValidada loop

               Arbitro.ConfirmaApuesta(ApuestaValidada);
               if not ApuestaValidada then
                  -- Genera Nueva apuesta
                  for I in 1..MiNombre loop

                     Apuesta := Integer_Random.Random(G) mod (NumJug * 3) ;
                  end loop;
                  Arbitro.ReEnviaApuesta(Apuesta);
               end if;

            end loop;



            -- Envía Mano

            for I in 1..MiNombre loop
               Mano := Integer_Random.Random(G) mod 4;
            end loop;
            Arbitro.EnviaMano(MiNombre, Mano);

            -- Comprueba su estado, si sigue jugando, si ha perdido o

            --  si ha ganado y deja de jugar
            Arbitro.SiguesJugando(MiNombre, YoSigo, HuboGanador);
            if YoSigo = SIGUES_JUGANDO then
               Terminar := False;
            else
               if YoSigo = NO_SIGUES_JUGANDO then

                  Terminar := True;
               else
                  -- Ha perdido
                  Put_Line("Jugador " & MiNombre'Img &
                                   ": He perdido, tengo que pagar :_(");
               end if;

            end if;
            if HuboGanador then

               NumJug := NumJug - 1;
            end if;
         end loop;


      end if;

   end Jugador;


   Jugadores : array (1..MAX) of Jugador;
   NumJug : Natural;


begin
   if Ada.Command_Line.Argument_Count /= 1 then
      -- Número incorrecto de parámetros
      Put_Line("Uso: ./chinos <num_jugadores>");
      NumJug := 1;

   else

      NumJug := Integer'Value(Ada.Command_Line.Argument(1));

      if NumJug < 2 then
         -- Número mínimo de jugadores
         Put_Line("El número de jugadores ha de ser mayor que 1." &

                          NumJug'Img & " no es mayor que 1");
         Put_Line("Seleccione un valor mayor o igual que 2");
         NumJug := 1;
      end if;

      if NumJug > MAX then

         -- Número máximo de jugadores
         Put_Line(NumJug'Img & " es mayor que " & MAX'Img);
         Put_Line("Seleccione un valor menor o igual que " &
                              MAX'Img);
         NumJug := 1;
      end if;


   end if;


   Arbitro.FijaNumeroJugadores(NumJug);

   -- Por si nos intentan colar algún valor no válido

exception
   when Constraint_Error =>
      NumJug := 1;
      Arbitro.FijaNumeroJugadores(NumJug);
      Put_Line("El Valor Introducido no es correcto.");
      Put_Line("Uso: ./chinos <num_jugadores>");

end Chinos;
