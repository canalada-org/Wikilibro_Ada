package Cubiertos is

  type Cubierto is limited private;
   
   procedure Coger(C: in out Cubierto);
   procedure Soltar(C: in out Cubierto);

private
   
   type Status is (LIBRE, OCUPADO);
   
   protected type Cubierto(Estado_Cubierto: Status := LIBRE) is
      entry Coger;
      entry Soltar;
   private
      Estado: Status := Estado_Cubierto;
   end Cubierto;
   
end Cubiertos;