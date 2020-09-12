with Ada.Command_Line,Ada.Text_Io;
 use Ada.Command_Line,Ada.Text_Io;

 procedure Visor_texto is

  -- Lee de un fichero de texto cuya ruta se pasa por parámetro o se pregunta
  -- explícitamente, y se visualiza por pantalla, tomando como 'estandard' una
  -- consola de 24 líneas de largo y 80 caracteres de ancho

   Caracteres_Por_Linea : constant Natural                            := 79;
   Lineas_Por_Pantalla  : constant Natural                            := 24;
   F                    :          File_Type;
   Linea                :          String (1 .. Caracteres_Por_Linea);
   Indice               :          Natural;
   Contador             :          Natural                            := 0;

    procedure Esperar_Tecla  is
      C : Character;
   begin
      Get_Immediate(C);
   end Esperar_Tecla;

begin
  if Argument_Count>0 then
  -- Si hay parametros, usamos el primero como ruta del archivo
     Open(F,In_File,Argument(1));
  else
  -- Si no hay parámetros, preguntamos explícitamente la ruta del archivo
     Put_Line("Introduzca la ruta del archivo a abrir: ");
     Get_Line(Linea,Indice);
     Open(F,In_File,Linea(1..Indice));
     New_Line(3);
  end if;

  Put_Line("-----------------------------------");
  Put_Line("- Visor de texto - " & Name(F));
  Put_Line("-----------------------------------");
  -- La función Name() nos devuelve la ruta del archivo
  New_Line(2);

  while not End_Of_File(F) loop     -- Leemos hasta llegar al final del fichero
     -- Si llegamos al final e intentamos leer, dará error, por lo que hay que prevenirlo
     if Contador>=Lineas_Por_Pantalla-2 then
        New_Line;
        Put_Line("---- Presione una tecla para continuar");
        Esperar_Tecla;
        New_Line;
        Contador:=0;
     end if ;
     -- Leemos una linea desde el archivo, tomando su longitud en 'Indice'
     -- y guardando la linea en un string
     Get_Line(F,Linea,Indice);
     -- Visualizamos la linea obtenida por pantalla, pero solo hasta la longitud obtenida
     Put_Line(Linea(1..Indice));
     Contador:=Contador+1;
  end loop ;
  Close(F);

-- Controlamos posibles errores que puede haber con ficheros
exception
  when Name_Error=>
     New_line(2);
     Put_Line("**** Error ****");
     Put_Line("Nombre de archivo no valido");
  when Use_Error=>
     New_line(2);
     Put_Line("**** Error ****");
     Put_Line("Archivo ya abierto o inaccesible");
end Visor_texto;
