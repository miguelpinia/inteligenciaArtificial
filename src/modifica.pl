/*
* Módulo que provee predicados wrapper para modificar:
*
* a) El nombre de una clase u objeto.
* b) El valor de una propiedad específica de una clase u objeto
* c) Con quien mantiene una relación específica con una clase u objeto.
*/

/*
* modifica_nombre_de_clase(KB, NuevaClase, Clase, NuevaKB):
*
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Modifica el nombre de la clase Clase por NuevaClase, afectando a
* todos los descendientes directos de la clase, así como a todos los
* objetos declarados directamente de la clase Clase.
* NuevaKB es la KB resultante después de los cambios.
*
*/
modifica_nombre_de_clase(KB, NuevaClase, Clase, NuevaKB) :-
    reloadKB(KB),
    modifica_nombre_de_clase(NuevaClase, Clase),
    kb(NuevaKB).

/*
* modifica_id_de_objeto(KB, Referencia, NuevaReferencia, NuevaKB)
*
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Sustituye el id Referencia del objeto por NuevaReferencia. Esto sólo
* lo realiza si no se encuentra declarado previamente la
* NuevaReferencia como extensión de la clase top.
* NuevaKB es la KB resultante después de los cambios.
*
*/
modifica_id_de_objeto(KB, Referencia, NuevaReferencia, NuevaKB) :-
    reloadKB(KB),
    modifica_id_de_objeto(Referencia, NuevaReferencia),
    kb(NuevaKB).

/*
* modifica_propiedad_de_clase(KB, Clase, Propiedad, Valores, NuevaKB):
*
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Modifica la propiedad Propiedad modificando la lista o el valor
* asociado, reemplazándolo por Valores. En caso de que se quiera
* modificar una propiedad sin valores por su negada, basta con agregar
* en la misma posiciń que Valores, el valor por el que se quiere
* reemplazar.
* NuevaKB es la KB resultante después de los cambios.
*
*/
modifica_propiedad_de_clase(KB, Clase, Propiedad, Valores, NuevaKB) :-
    reloadKB(KB),
    modifica_propiedad_de_clase(Clase, Propiedad, Valores),
    kb(NuevaKB).

/*
* modifica_propiedad_de_objeto(KB, Referencia, Propiedad, Valores, NuevaKB).
*
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Modifica la propiedad Propiedad modificando la lista o el valor
* asociado, reemplazándolo por Valores. En caso de que se quiera
* modificar una propiedad sin valores por su negada, basta con agregar
* en la misma posición que Valores, el valor por el que se quiere
* reemplazar.
* Recarga la KB en caso de diferir con la que se tiene actualmente.
*
*/
modifica_propiedad_de_objeto(KB, Referencia, Propiedad, Valores, NuevaKB) :-
    reloadKB(KB),
    modifica_propiedad_de_objeto(Referencia, Propiedad, Valores),
    kb(NuevaKB).

/*
* modifica_relacion_de_clase(KB, Clase, Relacion, Valores, NuevaKB)
*
* Modifica la relación Relacion de la clase Clase, actualizando los
* valores de la relacion con Valores.
*/
modifica_relacion_de_clase(KB, Clase, Relacion, Valores, NuevaKB) :-
    reloadKB(KB),
    modifica_relacion_de_clase(Clase, Relacion, Valores),
    kb(NuevaKB).

/*
* modifica_relacion_de_objeto(KB, Referencia, Relacion, Valores, NuevaKB)
*
* Modifica la relación Relacion del objeto asociado a la referencia
* Referencia, actualizando los valores de la relacion con Valores.
*/
modifica_relacion_de_objeto(KB, Referencia, Relacion, Valores, NuevaKB) :-
    reloadKB(KB),
    modifica_relacion_de_objeto(Referencia, Relacion, Valores),
    kb(NuevaKB).