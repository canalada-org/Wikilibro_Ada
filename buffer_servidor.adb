package body Buffer_servidor is
   task body TBuffer is
      subtype TCardinalBuffer is Natural range 0 .. Longitud;
      subtype TRangoBuffer is TCardinalBuffer range 0 .. Longitud - 1;
      Buf: array (TRangoBuffer) of TElemento;
      Cima, Base: TRangoBuffer := 0;
      NumElementos: TCardinalBuffer := 0;
   begin
      loop
         select
            when NumElementos < Longitud =>
               accept Escribir (E: TElemento) do
                  Buf(Cima) := E;
               end Escribir;
               Cima := TRangoBuffer(Integer(Cima + 1) mod Longitud);
               NumElementos := NumElementos + 1;
         or
            when NumElementos > 0 =>
               accept Leer (E: out TElemento) do
                  E := Buf(Base);
               end Leer;
               Base := TRangoBuffer(Integer(Base + 1) mod Longitud);
               NumElementos := NumElementos - 1;
         or
            terminate;
         end select;
      end loop;
   end TBuffer;

   procedure EscribirBuf (B: in out TBuffer; E: TElemento) is
   begin
      B.Escribir (E);
   end EscribirBuf;

   procedure LeerBuf (B: in out TBuffer; E: out TElemento) is
   begin
      B.Leer (E);
   end LeerBuf;
end Buffer_servidor;
