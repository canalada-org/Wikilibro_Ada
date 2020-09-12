with Text_IO, Buffer_servidor;
use Text_IO;
procedure Buffer is
  Clave_Salida : constant String := "Salir";
  type TMensaje is
    record
      NumOrden: Positive;
      Contenido: String (1..20);
    end record;
  package Cola_mensajes is new Buffer_servidor (TElemento => TMensaje);
  use Cola_mensajes;
  Cola: TBuffer;
  task Emisor;
  task Receptor;
  task body Emisor is
    M: TMensaje := (NumOrden => 1, Contenido => (others => ' '));
    Ultimo: Natural;
  begin
    loop
      Put ("[Emisor] Mensaje: ");
      Get_Line (M.Contenido, Ultimo);
      M.Contenido (Ultimo + 1 .. M.Contenido'Last) := (others => ' ');
      EscribirBuf (Cola, M);
      M.NumOrden := M.NumOrden + 1;
      exit when M.Contenido(Clave_Salida'range) = Clave_Salida;
    end loop;
  end Emisor;
  task body Receptor is
    package Ent_IO is new Text_IO.Integer_IO(Integer);
    use Ent_IO;
    M: TMensaje;
  begin
    loop
      LeerBuf (Cola, M);
      exit when M.Contenido(Clave_Salida'range) = Clave_Salida;
      Put ("[Receptor] Mensaje número ");
      Put (M.NumOrden);
      Put (": ");
      Put (M.Contenido);
      New_Line;
    end loop;
  end Receptor;

begin
  null;
end Buffer;
