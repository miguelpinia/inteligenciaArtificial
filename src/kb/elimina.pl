/*
 * Dado un objeto y una clase, elimina el objeto de la lista de
 * objetos de la clase dada.
 */
elimina_objeto_de_clase(KB, Objeto, Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    select(Objeto, Objs, NObjs),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, Props, Rels, NObjs),
              NuevaKB).

/*
 * Elimina un objeto de la base de conocimiento. Cuando un objeto se
 * elimina, también se elimina de la lista de objetos de la clase ala
 * que pertenece.
 * %?- kb(KB), elimina_objeto(KB, r1, NKB), obten_objeto(NKB, r1, Obj).
 * %?- kb(KB), elimina_objeto(KB, r1, NKB), obten_clase(NKB, rosa, Obj).
*/
elimina_objeto(KB, Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    elimina_objeto_de_clase(KB, Objeto, Clase, NKB),
    select(objeto(Objeto, Clase, Props, Rels), NKB, NuevaKB).
