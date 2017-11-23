# Proyecto de inferencia deliverativa.

Proyecto para el curso de inteligencia artificial de maestría en
Ciencias e Ingeniería en Computación en IIMAS, UNAM. Implementada en
el lenguaje Prolog, provee una implementación de una base de
conocimiento no monotónica y predicados para realizar búsquedas en
torno al diagnóstico, toma de decisión y planeación de un robot de
servicio. Este proyecto incluye un simulador simple de un robot de
servicio.

## Base de conocimiento

Implementación de una base de conocimiento jerárquica no monotónica en
el lenguaje Prolog. Esta base de conocimiento tiene el soporte para
las siguientes operaciones:

* Implementar predicados de consulta para:
  * Obtener la extensión de una clase
  * Obtener la extensión de una propiedad
  * La extensión de una relación
  * Todas las clases a las que pertenece un objeto
  * Todas las propiedades de un objeto o clase
  * Todas las relaciones de un objeto o clase
* Implementar predicados que permitan añadir:
  * Clases u objetos
  * Propiedades de clases u objetos
  * Relaciones de clases u objetos
* Implementar predicados que permitan eliminar:
  * Clases u objetos
  * Propiedades específicas de clases u objetos
  * Relaciones específicas de clases u objetos
* Implementar predicados que permitan modificar:
  * El nombre de una clase u objeto
  * El valor de una propiedad específica de una clase u objeto
  * Con quien mantiene una relación específica una clase u objeto

## Inferencia oportunista

Este módulo provee herramientas para que un robot de servicio realice
las operaciones de diagnóstico de su entorno observado, una toma de
decisión (selección de tareas a realizar por el robot) y un plan de
acción (acciones para resolver las tareas que el robot a decidido).

En el módulo de diagnóstico tenemos el predicado principal:

* **diagnóstico(KB, Diagnostico, NuevaKB):** Devuelve el diagnóstico
  de las acciones del asistente humano y devuelve una nueva base de
  conocimiento con la información actualizada a partir del diagnóstico
  realizado.

Dentro del módulo de decisión tenemos el predicado principal:

* **decision(KB, Decisiones):** Devuelve la lista de decisiones que
  toma el robot, generándolas a partir del estado de la base de
  conocimiento.

En el módulo de planeación:

* **planeacion(KB, Plan):** Devuelve el plan de acción a ejecutar por
  el robot.

En el módulo de simulación existen tres predicados principales:

* **simulador(KB):** Ejecuta una simulación completa a partir del
  estado de la base de conocimiento. Termina hasta que no haya tareas
  pendientes.
* **simula_un_paso(Archivo, Estante, Productos):** Ejecuta una
  simulación de un paso, actualizando la posición del robot a Estante
  y actualizando el contenido del estante por la lista de
  Productos. Posteriormente ejecuta el diagnóstico, la toma de
  decisión y la planeación.
* **simula_dos_pasos(Archivo, [Estante1, Estante2], [Prods1,
  Prods2]):** Ejecuta una simulación de dos pasos, actualizando la
  observación en Estante1 por Prods1 y después actualizando la
  posición del robot a Estante2 y actualizando la observación en ese
  punto por Prods2. Posteriormente ejecuta el diagnóstico, la toma de
  decisión y la planeación.

## Modo de uso

La forma de evaluar el funcionamiento del proyecto, basta con ejecutar
alguno de los tres predicados del módulo de simulación, por ejemplo,
para evaluar la simulación de un paso, podemos hacer:

```prolog
?- simula_un_paso('ejercicios/kb_tmp.txt', drink_shelf, [coca,heineken]).
```

De forma similar para la simulación de dos pasos:

```prolog
?- simula_dos_pasos('ejercicios/kb_tmp.txt', [drink_shelf,food_shelf], [[heineken], [coca, noodles, kellogs]]).
```

En el caso del simulador, hay que cargar la base de conocimiento con open_kb previamente:

```prolog
?- open_kb('kb_proy2.txt',KB), simulador(KB).
```


## Desarrolladores

* Paulina Hernández Contreras
* Miguel Angel Piña Avelino
* Diego Alejandro Velázquez Cervantes
