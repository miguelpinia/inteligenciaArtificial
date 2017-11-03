:- [util, consulta].

/*
 * Modifica el nombre de la clase a un obejto. Dado el nombre del
 * objeto Objeto, busca el objeto completo y reemplaza la clase Clase
 * por la clase NuevaClase, así como mover la referencia del objeto de
 * la clase Clase en los objetos de la clase NuevaClase. Estos cambios
 * se guardan en NuevaKB.
 *
 * ?-  kb(KB), modifica_nombre_de_clase_a_objeto(KB, top, r2, NKB), modifica_nombre_de_clase_a_objeto(NKB, top, r1, NKB1), obten_objeto(NKB1, r2, R2), obten_clase(NKB1, top, Top), obten_clase(NKB1, rosa, Rosa).
 */
modifica_nombre_de_clase_a_objeto(KB, NuevaClase, Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    obten_clase(KB, Clase, clase(Clase, SuperClase, CProps, CRels, Objs)),
    obten_clase(KB, NuevaClase, clase(NuevaClase, NSuperClase, NProps, NRels, NObjs)),
    delete(Objs, Objeto, OldObjs),
    append([Objeto], NObjs, NewNObjs),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, NuevaClase, Props, Rels),
              NKB),
    reemplaza(NKB, clase(Clase, SuperClase, CProps, CRels, Objs),
              clase(Clase, SuperClase, CProps, CRels, OldObjs),
              NKB1),
    reemplaza(NKB1, clase(NuevaClase, NSuperClase, NProps, NRels, NObjs),
              clase(NuevaClase, NSuperClase, NProps, NRels, NewNObjs),
              NuevaKB), !.

/*
 * Modifica el nombre de la clase a una lista de objetos.
 */
modifica_nombre_de_clase_a_objetos(KB, _, [], KB) :- !.
modifica_nombre_de_clase_a_objetos(KB, NuevaClase, [Objeto|Objetos], NuevaKB) :-
    modifica_nombre_de_clase_a_objeto(KB, NuevaClase, Objeto, NKB),
    modifica_nombre_de_clase_a_objetos(NKB, NuevaClase, Objetos, NuevaKB), !.
