:- [consulta].

/*
 * Predicados principales:
 * - agrega_objeto(KB, Nombre, Clase, Propiedades, Relaciones, NuevaKB).
 * - agrega_clase(KB, Clase, SuperClase, Propiedades, Relaciones, Objetos, NuevaKB).
 */

/*
 * Agrega el objeto a la lista de objetos de la clase Clase,
 * modificando el estado de kb KB en NuevaKB.
 */
agrega_objeto_a_clase(KB, Clase, Objeto, NuevaKB) :-
    obten_clase(KB, Clase, clase(Cls, SCls, Props, Rels, Objs)),
    append(Objs, [Objeto], NuevosObjs),
    reemplaza(KB, clase(Cls, SCls, Props, Rels, Objs),
              clase(Cls, SCls, Props, Rels, NuevosObjs),
              NuevaKB).

/*
 * Agrega un objeto a la base de conocimiento. Si ya existe un objeto
 * con ese nombre, no lo agrega y falla el predicado.
 * %?- kb(KB), agrega_objeto(KB, fido, perro, [], [], NKB), obten_clase(NKB, perro, Clase), obten_objeto(NKB, fido, Objeto).
 * %?- kb(KB), agrega_objeto(KB, abc, perro, [], [], NKB). <- Debe fallar.
 */
agrega_objeto(KB, Nombre, Clase, Propiedades, Relaciones, NuevaKB) :-
    obten_clase(KB, Clase, _), % Para garantizar que siempre hay una clase a la que agregar.
    identidad(objeto(Nombre, Clase, Propiedades, Relaciones), NuevoObjeto),
    es_objeto(NuevoObjeto),
    not(obten_objeto(KB, Nombre, _)),
    agrega_objeto_a_clase(KB, Clase, Nombre, NKB),
    append(NKB, [NuevoObjeto], NuevaKB).

/*
 * Agrega una clase a la base de conocimiento, si la clase ya existe,
 * no la agrega y falla el predicado.
 * %?- kb(KB), agrega_clase(KB, pug, perro, [], [], [], NKB), obten_clase(NKB, pug, Clase).
 * %?- kb(KB), agrega_clase(KB, perro, top, [], [], [], NKB), obten_clase(NKB, pug, Clase). <- debe fallar.
 */
agrega_clase(KB, Clase, SuperClase, Propiedades, Relaciones, Objetos, NuevaKB) :-
    identidad(clase(Clase, SuperClase, Propiedades, Relaciones, Objetos), NuevaClase),
    es_clase(NuevaClase),
    not(obten_clase(KB, Clase, _)),
    append(KB, [NuevaClase], NuevaKB).