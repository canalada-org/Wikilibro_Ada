package body Cubiertos is
 
   procedure Coger (C : in out Cubierto) is
   begin
      C.Coger;
   end Coger;
   
   procedure Soltar (C : in out Cubierto) is
   begin
      C.Soltar;
   end Soltar;

   protected body Cubierto is

      entry Coger when Estado = LIBRE is
      begin
	 Estado := OCUPADO;
      end Coger;


      entry Soltar when Estado = OCUPADO is
      begin
         Estado := LIBRE;
      end Soltar;

   end Cubierto;

end Cubiertos;
