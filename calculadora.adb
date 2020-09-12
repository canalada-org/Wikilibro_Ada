
        package body Calculadora is

            function Sumar (Operador1 : in Integer;
                            Operador2 : in Integer)
                            return Integer is
            begin
               return Operador1 + Operador2;
            end Sumar;


            function Restar (Operador1 : in Integer;
                             Operador2 : in Integer)
                             return Integer is
            begin
               return Operador1 - Operador2;
            end Restar;

     end Calculadora;

