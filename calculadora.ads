
 package Calculadora is

 pragma Remote_Call_Interface;

 function Sumar (Operador1 : in Integer;
                 Operador2 : in Integer)
                     return Integer;

 function Restar (Operador1 : in Integer;
                  Operador2 : in Integer)
                      return Integer;

 end Calculadora;
