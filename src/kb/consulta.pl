/*
 * Agrega operador =>.
 */
:- op(800,xfx,'=>').


/*
 * Define base de conocimiento para pruebas.
 */
kb([clase(top, none, [], [], []),
    clase(animal, top, [movil], [], []),
    clase(perro, animal, [canino], [odia=>[gato]], [abc, def]),
    objeto(abc, perro, [agresivo, alias=>[diablo]], [muerde=>[def]]),
    objeto(def, perro, [not(agresivo), alias=>[fido, bueno]], [not(odia=>[gato]), odia=>[ardilla]]),
    clase(gato, animal, [felino], [huye=>[perro]], []),
    clase(vegetal,top,	[not(movil), not(color=>negro)], [], []),
    clase(girasol,vegetal, [movil, color=>[amarillo, rojo]],[], [gira1, gira2]),
    objeto(gira1,girasol,[color=>rojo,alias=>[rojito,chulote]],[]),
    objeto(gira2,girasol,[alias=>[udf]],[]),
    clase(rosa, vegetal, [bonita,color=>udf], [], [r1,r2,r3]),
    objeto(r1,rosa,[color=>rojo],[]),
    objeto(r2,rosa,[movil],[]),
    objeto(r3,rosa,[color=>blanco, not(color=>rojo)],[])]).

/*
 * Obtiene la cabeza de una lista.
 */
cabeza([X|_], X).

/*
 * Obtiene el functor de clase asociado al nombre Nombre.
 * %?- kb(KB), obten_clase(KB, perro, clase(A, B, C, D, E)).
 * %?- kb(KB), obten_clase(KB, perro, KB).
 */
obten_clase(KB, Nombre, Clase) :-
    findall(clase(Nombre, Padre, Preds, Rels, Objs),
            member(clase(Nombre, Padre, Preds, Rels, Objs), KB), L),
    cabeza(L, Clase).


/*
 * Obtiene el functor de objeto asociado al nombre Nombre.
 * %?- kb(KB), obten_objeto(KB, abc, Objeto).
 * %?- kb(KB), obten_objeto(KB, def, objeto(N, C, P, R)).
 */
obten_objeto(KB, Nombre, Objeto) :-
    findall(objeto(Nombre, Clase, Preds, Rels),
            member(objeto(Nombre, Clase, Preds, Rels), KB), L),
    cabeza(L, Objeto).

/*
 * Obtiene la lista de functores de objetos asociada a la lista de
 * nombres dada. Si algún nombre no se encuentra en la base de
 * conocimiento, sólo devuelve los que si estén.
 * %?- kb(KB), obten_objetos(KB, [abc, def, r1], Objetos).
 */
obten_objetos(_, [], []) :- !.

obten_objetos(KB, [Nombre|RestoNombres], Objetos) :-
    not(member(objeto(Nombre, _, _, _), KB)),
    !,
    obten_objetos(KB, RestoNombres, Objetos).

obten_objetos(KB, [Nombre|RestoNombres], Objetos) :-
    findall(objeto(Nombre, C, P, R),
            member(objeto(Nombre, C, P, R), KB),
            L),
    cabeza(L, Objeto),
    obten_objetos(KB, RestoNombres, RestoObjetos),
    append([Objeto], RestoObjetos, Objetos).

/*
 * Obtiene todos los functores de los objetos asociados a la clase con
 * nombre Nombre.
 * %?- kb(KB), objetos_de_clase(KB, rosa, Objetos).
 */
objetos_de_clase(KB, Nombre, Objetos) :-
    obten_clase(KB, Nombre, clase(Nombre, _, _, _, Objs)),
    obten_objetos(KB, Objs, Objetos).

/*
 * Verdadero si X unifica con un objeto.
 * %?- es_objeto(objeto(_, _, _, _)).
 */
es_objeto(X) :- X = objeto(_, _, _, _).

/*
 * Verdadero si X unifica con una clase.
 * %?- es_clase(clase(_, _, _, _, _)).
 */
es_clase(X) :- X = clase(_, _, _, _, _).

/*
 * Filtra las clases de KB.
 * %?- kb(KB), lista_de_clases(KB, Clases).
 */
lista_de_clases(KB, Clases) :- include(es_clase, KB, Clases).

/*
 * Filtra los objetos de KB.
 * %?- kb(KB), lista_de_objetos(KB, Objetos).
 */
lista_de_objetos(KB, Objetos) :- include(es_objeto, KB, Objetos).

/*
 * Obtiene todos los hijos directos de la clase Clase.
 * kb(KB), hijos(KB, top, Hijos).
 */
hijos(KB, Clase, Hijos) :-
    findall(clase(Nombre, Clase, Preds, Rels, Objs),
            member(clase(Nombre, Clase, Preds, Rels, Objs), KB),
            Hijos).

/*
 * Obtiene el nombre de las clases a partir de una lista de funtores
 * de clase.
 */
nombre_clases([], []) :- !.
nombre_clases([clase(Nombre, _, _, _, _)|Otras], [Nombre|Otros]) :- nombre_clases(Otras, Otros).

/*
 * Obtiene el nombre de los objetos a partir de una lista de functores
 * de objetos.
 */
nombre_objetos([], []) :- !.
nombre_objetos([objeto(Nombre, _, _, _)|OtrosObjs], [Nombre|OtrosNombres]) :- nombre_objetos(OtrosObjs, OtrosNombres).

s_(_, [], []) :- !.
s_(KB, [Clase|OtrasClases], SubClases) :-
    hijos(KB, Clase, Hijos),
    nombre_clases(Hijos, Nombres),
    append(OtrasClases, Nombres, NombreClases),
    s_(KB, NombreClases, NSubclases),
    append(Hijos, NSubclases, SubClases).

/*
 * Obtiene todas las subclases de la clase Clase.
 * %?- kb(KB), subclases(KB, top, Subclases).
 */
subclases(KB, Clase, SubClases) :-
    s_(KB, [Clase], Scls),
    nombre_clases(Scls, SubClases).

/*
 * Obtiene todas las superclases de la clase Clase.
 * %?- kb(KB), superclases(KB, rosa, SuperClases).
 */
superclases(_, Clase, []) :- Clase = top, !.
superclases(KB, Clase, SuperClases) :-
    obten_clase(KB, Clase, clase(Clase, Padre, _, _, _)),
    superclases(KB, Padre, SCls),
    append([Padre], SCls, SuperClases).

ext_clase(_, [], []) :- !.
ext_clase(KB, [Clase|Clases], Objetos) :-
    objetos_de_clase(KB, Clase, Objs),
    ext_clase(KB, Clases, RestObjs),
    append(Objs, RestObjs, Objetos).

/*
 * Obtiene la extensión de la clase Clase.
 * %?-kb(KB), extension_de_clase(KB, top, Objetos).
 */
extension_de_clase(KB, Clase, Objetos) :-
    subclases(KB, Clase, SubClases),
    append([Clase], SubClases, SCls),
    ext_clase(KB, SCls, Objs),
    nombre_objetos(Objs, Objetos).
