 with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Sequential_IO;

 --  Ejemplo avanzado de uso Ada.Sequential_IO;
 --  Programa de 'cifrado' César
 --  Leemos de un fichero, hacemos un desplazamiento mediante
 --  una contraseña (método César) y escribimos en otro fichero.

 -- Evidentemente la seguridad es casi nula, basta ir probando las
 -- 255 posibilidades para sacarlo. Y si cifras dos veces consecutivas
 -- de forma que sumen 256 también se saca.

procedure Cesar is

  type Byte is mod 2**8;
  package Pkg_Byte is new Ada.Sequential_Io(Byte);


  C : Character := Ascii.Nul;
  B : Byte;

  Archivo_Entrada,
  Archivo_Salida  : Pkg_Byte.File_Type;
  Ruta_Entrada,
  Ruta_Salida     : String (1 .. 32);
  Password,
  Long_Ent,
  Long_Sal,
  Aux             : Integer            := 0;

begin

   while C/='c' and C/='d' loop
      Ada.Text_Io.Put_Line("Cifrar (c) o Descifrar (d)?");
      Ada.Text_Io.Get_Immediate (C);
      if C/='c' and C/='d' then
         Ada.Text_Io.New_Line;
         Ada.Text_Io.Put_Line("Error:Pulse la C o la D");
      end if;
   end loop;

  Ada.Text_Io.Put("Ha elegido: ");
  if C='c' then
     Ada.Text_Io.Put("Cifrar");
     Ada.Text_Io.New_Line(2);
  else
     Ada.Text_Io.Put("Descifrar");
     Ada.Text_Io.New_Line(2);
  end if;

  Ada.Text_Io.Put_Line("Introduzca el nombre del fichero de entrada (maximo 32 caracteres)");
  Ada.Text_Io.Get_Line(Ruta_Entrada,Long_Ent);
  Pkg_Byte.Open(Archivo_Entrada,Pkg_Byte.In_File,Ruta_Entrada(1..Long_Ent));


  Ada.Text_Io.Put_Line("Introduzca el nombre del fichero de salida (maximo 32 caracteres)");
  Ada.Text_Io.Put_Line("Ojo, sobreescribira el fichero sin preguntar!");
  Ada.Text_Io.Get_Line(Ruta_Salida,Long_Sal);
  Pkg_Byte.Create(Archivo_Salida,Pkg_Byte.Out_File,Ruta_Salida(1..Long_Sal));

  while Password<1 or Password>255 loop
     Ada.Text_Io.Put_Line("Elija un password (numero del 1 al 255)");
     Ada.Integer_Text_Io.Get(Password);
     if Password<1 or Password>255 then
        Ada.Text_Io.New_Line;
        Ada.Text_Io.Put_Line("Error: El valor no es correcto. Debe estar entre 1 y 255");
     end if;
  end loop;


  while not Pkg_Byte.End_Of_File(Archivo_Entrada) loop
     Pkg_Byte.Read(Archivo_Entrada,B);
     if C='c' then
        Aux:=(Integer(B)+Password) mod 256;
     else
        Aux:=(Integer(B)-Password) mod 256;
     end if;
     Pkg_Byte.Write(Archivo_Salida,Byte(Aux));
  end loop;

  Pkg_Byte.Close(Archivo_Entrada);
  Pkg_Byte.Close(Archivo_Salida);

  Ada.Text_Io.Put_Line("Operacion realizada con exito");
  Ada.Text_Io.Put_Line("Presione cualquier tecla para finalizar");
  Ada.Text_Io.Get_Immediate(C);

exception
  when Pkg_Byte.Name_Error=>
     Ada.Text_Io.Put_Line("Error: Nombre de archivo o ruta incorrectos");
  when Pkg_Byte.Use_Error=>
     Ada.Text_Io.Put_Line("Error: El archivo ya esta abierto");
  when Ada.Text_IO.Data_Error=>
     Ada.Text_Io.Put_Line("Error: La contraseña debe estar entre 1 y 255");
end Cesar;
