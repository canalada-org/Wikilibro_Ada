package body Semaforos is
  procedure Wait (Sem: in out TSemaforo) is
    begin
      Sem.Wait;  -- Llamada a punto de entrada de la tarea.
     end Wait;
  procedure Signal (Sem: in out TSemaforo) is
    begin
      Sem.Signal;  -- Llamada a punto de entrada de la tarea.
     end Signal;
  task body TSemaforo is
    S: Natural := ValorInicial;  -- Es el contador del semaforo.
   begin
    loop
      select
        when S > 0 =>
          accept Wait;
          S := S - 1;
      or
        accept Signal;
        S := S + 1;
      or
        terminate;
      end select;
    end loop;
  end TSemaforo;
end Semaforos;
