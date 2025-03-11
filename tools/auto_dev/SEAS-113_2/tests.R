#### Unit testing agent runs ####


##### Run #####


#Acción a realizar: write_new_tests.
#Detalles: Se requiere crear un conjunto de tests nuevos desde cero para la función seasonder_exportRadialMetrics, ya que actualmente no existen tests. Los tests deben cubrir los siguientes casos:
#
#1. Caso de solución single:
#   - Simular una tabla MUSIC que contenga solamente una solución single en la lista DOA_solutions, con valores para 'bearing' y 'P'.
#   - Verificar que la función retorna una única fila con MSEL igual a 1, que las columnas LOND, LATD, VELU, RNGE y SPDC se copien correctamente, y que MSA1 y BEAR sean iguales al valor de la solución single.
#   - Comprobar que MSP1 se calcula como 10*log10(Mod(P)) y que las columnas MEGR, MPKR, MOFR y los eigenvalues (MEI1, MEI2, MEI3) se asignan usando los valores correspondientes de la fila MUSIC.
#
#2. Caso de solución dual:
#   - Simular una tabla MUSIC donde la entrada tenga una solución dual en DOA_solutions con dos bearings y dos valores de P.
#   - Verificar que se generen dos filas de salida: la primera con MSEL = 2 (y MDA1 y BEAR con el primer bearing) y la segunda con MSEL = 3 (y MDA2 y BEAR con el segundo bearing).
#   - Comprobar que en la fila dual se calcule MDP1 y MDP2 correctamente (usando 10*log10(Mod(P[1])) y 10*log10(Mod(P[2])) respectivamente), y que también se asignen las columnas MEGR, MPKR, MOFR y los eigenvalues.
#
#3. Caso en el que existan ambas soluciones (single y dual):
#   - Si la estructura MUSIC contiene ambos tipos de soluciones, asegurarse de que la función retorne filas para cada solución, manteniendo la consistencia en la copia de los datos básicos y asignando correctamente los campos específicos de cada tipo.
#
#4. Validar que, en ausencia de alguna información (por ejemplo, si P es NULL o faltan datos en DOA_solutions), la función maneja la situación apropiadamente (dejando NA en los campos correspondientes).
#
#Se recomienda utilizar el framework testthat para la realización de estos tests, creando datos de entrada simulados que respeten la estructura descrita para la tabla MUSIC. Estos tests ayudarán a garantizar que la función cumpla con las especificaciones del formato RDM1, en particular la generación de filas separadas para cada solución DOA y el correcto rellenado de las columnas a partir de MEGR en adelante.
#

