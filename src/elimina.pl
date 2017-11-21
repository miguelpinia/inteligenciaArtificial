:- [consulta, agrega, modifica].

/*
 * Predicados principales:
 *
 * ?- elimina_objeto(KB, Objeto, NuevaKB)
 * ?- elimina_clase(KB, AEliminar, NuevaKB)
 * ?- elimina_propiedad_de_objeto(KB, Prop, Objeto, NuevaKB)
 * ?- elimina_propiedad_de_clase(KB, Prop, Clase, NuevaKB)
 * ?- elimina_relacion_de_objeto(KB, Rel, Objeto, NuevaKB)
 * ?- elimina_relacion_de_clase(KB, Rel, Clase, NuevaKB)
 */

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
    elimina_objeto_de_relaciones(NKB, Objeto, NKB1),
    select(objeto(Objeto, Clase, Props, Rels), NKB1, NuevaKB), !.


elimina_objeto_de_relaciones_de_objeto(KB, Objeto, AEliminar, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    elimina_elemento_de_lista(Rels,AEliminar, NRels),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, Props, NRels),
              NuevaKB).
elimina_objeto_de_relaciones_de_objetos(KB, [], _, KB) :- !.
elimina_objeto_de_relaciones_de_objetos(KB, [Objeto|Objetos], AEliminar, NuevaKB) :-
    elimina_objeto_de_relaciones_de_objeto(KB, Objeto, AEliminar, NKB),
    elimina_objeto_de_relaciones_de_objetos(NKB, Objetos, AEliminar, NuevaKB).
elimina_objeto_de_relaciones_de_clase(KB, Clase, AEliminar, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    elimina_elemento_de_lista(Rels, AEliminar, NRels),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, Props, NRels, Objs),
              NuevaKB).
elimina_objeto_de_relaciones_de_clases(KB, [], _, KB) :- !.
elimina_objeto_de_relaciones_de_clases(KB, [Clase|Clases], AEliminar, NuevaKB) :-
    elimina_objeto_de_relaciones_de_clase(KB, Clase, AEliminar, NKB),
    elimina_objeto_de_relaciones_de_clases(NKB, Clases, AEliminar, NuevaKB).
elimina_objeto_de_relaciones(KB, AEliminar, NuevaKB) :-
    extension_de_clase(KB, top, Objetos),
    elimina_objeto_de_relaciones_de_objetos(KB, Objetos, AEliminar, NKB),
    subclases(NKB, top, SubClases),
    elimina_clase_de_relaciones_de_clases(NKB, SubClases, AEliminar, NuevaKB).


/*
 * Dada una clase y una lista de relaciones, elimina la relación de la lista de relaciones
 * ?- elimina_elemento_de_lista([odia=>[gato]], gato, X).
 * ?- elimina_elemento_de_lista([odia=>[gato, foo], not(ama=>[gato, perro, avestruz]), alimenta=>[gato]], gato, X).
 * ?- elimina_elemento_de_lista([odia=>[gato, foo], not(ama=>[gato, perro, avestruz])], gato, X).
 * ?- elimina_elemento_de_lista([odia=>[gato, foo], ama=>[gato, perro, avestruz]], gato, X).
 * ?- elimina_elemento_de_lista([odia=>[gato, foo], ama=>[gato, perro]], gato, X).
 * ?- elimina_elemento_de_lista([not(odia=>[gato]), odia=>[ardilla]], gato, X).
*/
elimina_elemento_de_lista([], _, []) :- !.
elimina_elemento_de_lista([_=>[Elemento]], Elemento, []) :- !.
elimina_elemento_de_lista([Rel=>Vals], Elemento, [Result]) :-
    elimina_de_valores_de_relacion(Rel=>Elemento, Rel=>Vals, Result).
elimina_elemento_de_lista([not(_=>[Elemento])], Elemento, []) :- !.
elimina_elemento_de_lista([not(Rel=>Vals)], Elemento, [Result]) :-
    elimina_de_valores_de_relacion(not(Rel=>Elemento), not(Rel=>Vals), Result).
elimina_elemento_de_lista([_=>[Elemento]|OtrasRels], Elemento, Result) :-
    elimina_elemento_de_lista(OtrasRels, Elemento, Result).
elimina_elemento_de_lista([Rel=>Vals|OtrasRels], Elemento, [R|Result]) :-
    elimina_de_valores_de_relacion(Rel=>Elemento, Rel=>Vals, R),
    elimina_elemento_de_lista(OtrasRels, Elemento, Result).
elimina_elemento_de_lista([not(_=>[Elemento])|OtrasRels], Elemento, Result) :-
    elimina_elemento_de_lista(OtrasRels, Elemento, Result).
elimina_elemento_de_lista([not(Rel=>Vals)|OtrasRels], Elemento, [R|Result]) :-
    elimina_de_valores_de_relacion(not(Rel=>Elemento), not(Rel=>Vals), R),
    elimina_elemento_de_lista(OtrasRels, Elemento, Result).
elimina_elemento_de_lista([OtraRel|Rels], _, [OtraRel|Rels]) :- !.
elimina_elemento_de_lista([X], _, [X]) :- !.
/*
 * Dada una clase y un objeto, elimina la clase de la lista de
 * relaciones de un objeto.
 * kb(KB), elimina_clase_de_relaciones_de_objeto(KB, def, gato, NKB), obten_objeto(NKB, def, Objeto).
 */
elimina_clase_de_relaciones_de_objeto(KB, Objeto, AEliminar, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    elimina_elemento_de_lista(Rels,AEliminar, NRels),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, Props, NRels),
              NuevaKB).

/*
 * Elimina la clase de las relaciones de una lista de objetos.
 *
 * kb(KB), extension_de_clase(KB, top, Objetos), elimina_clase_de_relaciones_de_objetos(KB, Objetos, gato, NKB),obten_objeto(KB, def, Def1), obten_objeto(NKB, def, Def), obten_objeto(KB, gira1, Gira10), obten_objeto(NKB, gira1, Gira11).
 */
elimina_clase_de_relaciones_de_objetos(KB, [], _, KB) :- !.
elimina_clase_de_relaciones_de_objetos(KB, [Objeto|Objetos], AEliminar, NuevaKB) :-
    elimina_clase_de_relaciones_de_objeto(KB, Objeto, AEliminar, NKB),
    elimina_clase_de_relaciones_de_objetos(NKB, Objetos, AEliminar, NuevaKB).

/*
 * Elimina una clase de la lista de relaciones de una clase dada.
 * kb(KB), elimina_clase_de_relaciones_de_clase(KB, perro, gato, NuevaKB), obten_clase(KB, perro, Cls), obten_clase(NuevaKB, perro, Clase).
 */
elimina_clase_de_relaciones_de_clase(KB, Clase, AEliminar, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    elimina_elemento_de_lista(Rels, AEliminar, NRels),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, Props, NRels, Objs),
              NuevaKB).

/*
 * Elimina una clase de la lista de relaciones de una lista de clases.
 */
elimina_clase_de_relaciones_de_clases(KB, [], _, KB) :- !.
elimina_clase_de_relaciones_de_clases(KB, [Clase|Clases], AEliminar, NuevaKB) :-
    elimina_clase_de_relaciones_de_clase(KB, Clase, AEliminar, NKB),
    elimina_clase_de_relaciones_de_clases(NKB, Clases, AEliminar, NuevaKB).


/*
 * Dada una clase, la elimina de todas las relaciones existentes de
 * la base de conocimiento.
 * kb(KB), elimina_clase_de_relaciones(KB, gato, NKB), obten_clase(KB, perro, Cls), obten_clase(NKB, perro, Clase), obten_objeto(KB, def, Def1), obten_objeto(NKB, def, Def), obten_objeto(KB, gira1, Gira10), obten_objeto(NKB, gira1, Gira11).
 */
elimina_clase_de_relaciones(KB, AEliminar, NuevaKB) :-
    extension_de_clase(KB, top, Objetos),
    elimina_clase_de_relaciones_de_objetos(KB, Objetos, AEliminar, NKB),
    subclases(NKB, top, SubClases),
    elimina_clase_de_relaciones_de_clases(NKB, SubClases, AEliminar, NuevaKB).

/*
 * Elimina la clase AEliminar de la base de conocimiento KB y devuelve el
 * resultado de la eliminación en NuevaKB.
 * kb(KB), elimina_clase(KB, gato, NKB), obten_clase(KB, perro, Cls), obten_clase(NKB, perro, Clase), obten_objeto(KB, def, Def1), obten_objeto(NKB, def, Def), obten_objeto(KB, gira1, Gira10), obten_objeto(NKB, gira1, Gira11), extension_de_relacion(KB, odia=>gato, Objetos), extension_de_relacion(NKB, odia=>gato, ObjetosSin).
*/
elimina_clase(_, top, _) :- fail.
elimina_clase(KB, AEliminar, NuevaKB) :-
    obten_clase(KB, AEliminar, clase(AEliminar, SuperClase, Props, Rels, Objs)),
    modifica_nombre_de_clase_a_objetos(KB, SuperClase, Objs, NKBObjs),
    subclases_directas(NKBObjs, AEliminar, Hijos),
    modifica_nombre_de_superclase_a_clases(NKBObjs, SuperClase, Hijos, NKB),
    elimina_clase_de_relaciones(NKB, AEliminar, NKB1),
    select(clase(AEliminar, SuperClase, Props, Rels, []), NKB1, NuevaKB), !.

/*
 * Elimina la propiedad Prop de un objeto.
 * kb(KB), elimina_propiedad_de_objeto(KB, color=>blanco, r1, NKB), obten_objeto(KB, r1, Anterior), obten_objeto(NKB, r1, Nuevo). <- No debe cambiar el objeto.
 * ?- kb(KB), elimina_propiedad_de_objeto(KB, color=>rojo, r1, NKB), obten_objeto(KB, r1, Anterior), obten_objeto(NKB, r1, Nuevo).
 * ?- kb(KB), elimina_propiedad_de_objeto(KB, alias=>rojito, gira1, NKB), obten_objeto(KB, gira1, Anterior), obten_objeto(NKB, gira1, Nuevo).
 * ?- kb(KB), elimina_propiedad_de_objeto(KB, not(color=>amarillo), gira1, NKB), obten_objeto(KB, gira1, Anterior), obten_objeto(NKB, gira1, Nuevo).
 * ?- kb(KB), elimina_propiedad_de_objeto(KB, alias=>udf, gira2, NKB), obten_objeto(KB, gira2, Anterior), obten_objeto(NKB, gira2, Nuevo).
 * ?- kb(KB), elimina_propiedad_de_objeto(KB, not(movil), def, NKB), obten_objeto(KB, def, Anterior), obten_objeto(NKB, def, Nuevo). <- no hacerle nada al objeto.
 * ?- kb(KB), elimina_propiedad_de_objeto(KB, not(agresivo), def, NKB), obten_objeto(KB, def, Anterior), obten_objeto(NKB, def, Nuevo).
 * ?- kb(KB), elimina_propiedad_de_objeto(KB, movil, def, NKB), obten_objeto(KB, def, Anterior), obten_objeto(NKB, def, Nuevo).
 * ?- kb(KB), elimina_propiedad_de_objeto(KB, movil, r2, NKB), obten_objeto(KB, r2, Anterior), obten_objeto(NKB, r2, Nuevo).
 */
elimina_propiedad_de_objeto(KB, Prop, Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    elimina_propiedad_de_propiedades(Prop, Props, NProps),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, NProps, Rels),
              NuevaKB), !.

/*
 * Elimina la propiedad Prop de la clase Clase, y almacena el
 * resultado en NuevaKB.
 * kb(KB), elimina_propiedad_de_clase(KB, bonita, rosa, NKB), obten_clase(KB, rosa, Anterior), obten_clase(NKB, rosa, Nuevo).
 */
elimina_propiedad_de_clase(KB, Prop, Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    elimina_propiedad_de_propiedades(Prop, Props, NProps),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, NProps, Rels, Objs),
              NuevaKB), !.
/*
 * Elimina la relación Rel del objeto Objeto y almacena el resultado
 * en NuevaKB.
 * ?- kb(KB), elimina_relacion_de_objeto(KB, mas_bella_que=>gira2, gira1, NKB), obten_objeto(KB, gira1, Anterior), obten_objeto(NKB, gira1, Nuevo).
 * ?- kb(KB), elimina_relacion_de_objeto(KB, odiada_por=>r1, r3, NKB), obten_objeto(KB, r3, Anterior), obten_objeto(NKB, r3, Nuevo).
 */
elimina_relacion_de_objeto(KB, Rel, Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    elimina_relacion_de_relaciones(Rel, Rels, NRels),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, Props, NRels),
              NuevaKB), !.

elimina_relacion_de_clase(KB, Rel, Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objetos)),
    elimina_relacion_de_relaciones(Rel, Rels, NRels),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objetos),
              clase(Clase, SuperClase, Props, NRels, Objetos),
              NuevaKB), !.