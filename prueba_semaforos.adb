with Semaforos;

procedure Prueba_Semaforos is
  package Paquete_Semaforos is new Semaforos;
  use Paquete_Semaforos;
  Semaforo: TSemaforo;
begin  -- Aquí se inicia la tarea de tipo TSemaforo (objeto Semaforo).
   -- ...
   Wait (Semaforo);
   -- ...
   Signal (Semaforo);
   -- ...
end Prueba_Semaforos;
