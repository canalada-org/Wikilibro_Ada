

        with Text_IO; use Text_IO;
        with Calculadora;

        procedure Cliente is
        begin

           Put_Line ("- Calculadora, ¿cuanto es 321+123? = " &
                     Integer'Image (Calculadora.Sumar (321,123)));

           Put_Line ("- Calculadora, ¿cuanto es 321-123? = " &
                     Integer'Image (Calculadora.Restar (321,123)));

        end Cliente;

