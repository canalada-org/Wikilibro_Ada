generic
   type TElemento is private;
   Longitud: Positive := 32;
package Buffer_servidor is
   type TBuffer is limited private;
   procedure EscribirBuf (B: in out TBuffer; E: TElemento);
   procedure LeerBuf (B: in out TBuffer; E: out TElemento);
private
   task type TBuffer is
      entry Escribir (E: TElemento);
      entry Leer (E: out TElemento);
   end TBuffer;
end Buffer_servidor;
