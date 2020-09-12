with Ada.Text_IO, Ada.Sequential_IO;

 --  Ejemplo sencillo de uso Ada.Sequential_IO;
 --  Copiamos un archivo para hacer una copia de seguridad


 procedure Copiador is

   type Byte is mod 2**8;
   package Pkg_Byte is new Ada.Sequential_Io(Byte);

  C : Character := Ascii.Nul;
  B : Byte;

  Archivo_Original,
  Archivo_Copia    : Pkg_Byte.File_Type;
  Ruta_Entrada     : String (1 .. 32);
  Long_Ent         : Integer            := 0;

begin

   Ada.Text_Io.Put_Line("Introduzca el nombre del fichero de a copiar (maximo 32 caracteres)");
   Ada.Text_Io.Get_Line(Ruta_Entrada,Long_Ent);

 --  Abrimos el fichero en modo In_file, modo lectura,
 --  con la ruta especificada por el usuario
   Pkg_Byte.Open(Archivo_Original,Pkg_Byte.In_File,Ruta_Entrada(1..Long_Ent));

 --  Creamos un fichero del mismo nombre con terminación '.backup'
   Pkg_Byte.Create(Archivo_Copia,Pkg_Byte.Out_File,Ruta_Entrada(1..Long_Ent) & ".backup");

  --  Copiamos el contenido del fichero original al recién creado
  --  Nótese el uso de End_of_file para prevenir la lectura de final de fichero
   while not Pkg_Byte.End_Of_File(Archivo_Original) loop
      Pkg_Byte.Read(Archivo_Original,B);
      Pkg_Byte.Write(Archivo_Copia,B);
   end loop ;

 --  Importante: ¡No hay que olvidarse de cerrar los ficheros cuando no los usemos!
   Pkg_Byte.Close(Archivo_Original);
   Pkg_Byte.Close(Archivo_Copia);

   Ada.Text_Io.Put_Line("Operacion realizada con exito");
   Ada.Text_Io.Put_Line("Presione cualquier tecla para finalizar");
   Ada.Text_Io.Get_Immediate(C);

exception
   when Pkg_Byte.Name_Error=>
      Ada.Text_Io.Put_Line("Error: Nombre de archivo o ruta incorrectos");
   when Pkg_Byte.Use_Error=>
      Ada.Text_Io.Put_Line("Error: El archivo ya esta abierto");
end Copiador;
