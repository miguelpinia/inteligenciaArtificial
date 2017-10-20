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
    clase(girasol,vegetal, [movil, color=>amarillo],[],	[gira1, gira2]),
    objeto(gira1,girasol,[color=>rojo,alias=>[rojito,chulote]],[]),
    objeto(gira2,girasol,[alias=>[udf]],[]),
    clase(rosa, vegetal, [bonita,color=>udf], [], [r1,r2,r3]),
    objeto(r1,rosa,[color=>rojo],[]),
    objeto(r2,rosa,[],[]),
    objeto(r3,rosa,[color=>blanco],[])]).

/*
 * Obtiene la cabeza de una lista.
 */
cabeza([X|_], X).

/*
 * Obtiene el functor de clase asociado al nombre Nombre.
 * %?- kb(X), obten_clase(X, perro, clase(A, B, C, D, E)).
 * %?- kb(X), obten_clase(X, perro, X).
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
