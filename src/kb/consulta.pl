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
    objeto(gira1,girasol,[color=>rojo,not(color=>amarillo),alias=>[rojito,chulote]],[]),
    objeto(gira2,girasol,[alias=>[udf]],[]),
    clase(rosa, vegetal, [bonita,color=>udf], [], [r1,r2,r3]),
    objeto(r1,rosa,[color=>rojo],[]),
    objeto(r2,rosa,[movil],[]),
    objeto(r3,rosa,[color=>blanco, not(color=>rojo), not(bonita)],[])]).

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
 * Obtiene una lista de functores asociados a la lista de nombres de
 * clase.
 */
obten_clases(_, [], []) :- !.
obten_clases(KB, [Clase|Clases], ClasesComp) :-
    obten_clase(KB, Clase, Cls),
    obten_clases(KB, Clases, OtherCls),
    append([Cls], OtherCls, ClasesComp).

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

/*
 * Obtiene la lista de clases de un objeto.
 */
clases_de_objeto(KB, Objeto, Clases) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, _, _)),
    superclases(KB, Clase, SuperClases),
    append([Clase], SuperClases, Clases).

/*
 * Obtiene los objetos de una lista de clases.
 */
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

/*
 * Obtiene la identidad de una propiedad.
 * %?- prop_identidad(not(not(movil)), Prop).
 * %?- prop_identidad(not(movil), Prop).
 * %?- prop_identidad(movil, Prop).
 */
prop_identidad(not(not(Prop)), Prop) :- !.
prop_identidad(Prop, Prop).

/*
 * Obtiene la propiedad negada de una propiedad.
 * %?- prop_negada(movil, Prop).
 * %?- prop_negada(not(movil), Prop).
 * %?- prop_negada(not(not(movil)), Prop).
 */
prop_negada(not(Prop), Prop) :- !.
prop_negada(Prop, not(Prop)).

/*
 * Obtiene una propiedad con su valor a partir de su nombre.
 * %?- obten_propiedad_completa(alias, [color=>rojo,alias=>[rojito,chulote]], Prop).
 * %?- obten_propiedad_completa(not(color), [not(movil), not(color=>negro)], Prop).
 */
obten_propiedad_completa(_, [], _) :- fail.
obten_propiedad_completa(Prop, [Prop=>Valor|_], Prop=>Valor) :- !.
obten_propiedad_completa(not(Prop), [not(Prop=>Valor)|_], not(Prop=>Valor)) :- !.
obten_propiedad_completa(Prop, [Prop|_], Prop) :- !.
obten_propiedad_completa(Prop, [_|Props], X) :- obten_propiedad_completa(Prop, Props, X).

/*
 * Para el caso cuando hay una lista de valores con tamaño mayor a 1,
 * elimina el valor de la lista.
 * ?- elimina_de_valores_de_propiedad(alias=>rojito, alias=>[rojito,chulote], R).
 * ?- elimina_de_valores_de_propiedad(not(color=>negro), not(color=>[amarillo,negro]), R).
*/
elimina_de_valores_de_propiedad(not(Prop=>Valor), not(Prop=>Valores), Resultado) :-
    member(Valor, Valores),
    select(Valor, Valores, Res),
    identidad(not(Prop=>Res), Resultado).
elimina_de_valores_de_propiedad(Prop=>Valor, Prop=>Valores, Resultado) :-
    member(Valor, Valores),
    select(Valor, Valores, Res),
    identidad(Prop=>Res, Resultado), !.

/*
 * Dada una lista, reemplaza, el elemento X, por el elemento Y, y
 * devuelve la lista con el elemento reemplazado.
 */
reemplaza([],_,_,[]):-!.
reemplaza([X|Resto],X,Y,[Y|RestoReemplazado]):-
	!,
	reemplaza(Resto,X,Y,RestoReemplazado).
reemplaza([W|Resto],X,Y,[W|RestoReemplazado]):-
	!,
	reemplaza(Resto,X,Y,RestoReemplazado).

/*
 * Dada una propiedad con su valor y una lista de propiedades, elimina
 * la propiedad de la lista de propiedades y devuelve el resultado de
 * la eliminación.
 * ?- elimina_propiedad_de_propiedades(alias=>rojito, [color=>rojo,not(color=>[amarillo, negro]),alias=>[rojito,chulote]], R).
 * ?- elimina_propiedad_de_propiedades(color=>rojo, [color=>rojo,not(color=>[amarillo, negro]),alias=>[rojito,chulote]], R).
 * ?- elimina_propiedad_de_propiedades(not(color=>amarillo), [color=>rojo,not(color=>[amarillo, negro]),alias=>[rojito,chulote]], R).
 * ?- elimina_propiedad_de_propiedades(not(color=>amarillo), [color=>rojo,not(color=>[amarillo]),alias=>[rojito,chulote]], R).
 * ?- elimina_propiedad_de_propiedades(movil, [movil, color=>rojo,not(color=>[amarillo]),alias=>[rojito,chulote]], R).
 * ?- elimina_propiedad_de_propiedades(movil, [movil, color=>rojo,not(color=>[amarillo]),alias=>[rojito,chulote]], R).
 * ?- elimina_propiedad_de_propiedades(not(movil), [not(movil), color=>rojo,not(color=>[amarillo]),alias=>[rojito,chulote]], R).
 */
elimina_propiedad_de_propiedades(not(Prop=>Valor), Props, Resultado) :-
    (member(not(Prop=>Valor), Props),
     select(not(Prop=>Valor), Props, Resultado));
    (member(not(Prop=>[Valor]), Props),
     select(not(Prop=>[Valor]), Props, Resultado));
    (obten_propiedad_completa(not(Prop), Props, PropFound),
     elimina_de_valores_de_propiedad(not(Prop=>Valor), PropFound, PropMod),
     reemplaza(Props, PropFound, PropMod, Resultado)).
elimina_propiedad_de_propiedades(Prop=>Valor, Props, Resultado) :-
    (member(Prop=>Valor, Props),
     select(Prop=>Valor, Props, Resultado));
    (member(Prop=>[Valor], Props),
     select(Prop=>[Valor], Props, Resultado));
    (obten_propiedad_completa(Prop, Props, PropFound),
     elimina_de_valores_de_propiedad(Prop=>Valor, PropFound, PropMod),
     reemplaza(Props, PropFound, PropMod, Resultado)).
elimina_propiedad_de_propiedades(not(Prop), Props, Resultado) :-
    member(not(Prop), Props), select(not(Prop), Props, Resultado).
elimina_propiedad_de_propiedades(Prop, Props, Resultado) :-
    member(Prop, Props), select(Prop, Props, Resultado).

/*
 * Elimina las propiedades negadas, que aparecen después de la
 * declaración de una propiedad.
 * ?- elimina_propiedades_negadas([color=>blanco, not(color=>rojo), bonita, color=>udf, not(movil), not(color=>[blanco])], X).
 * ?- elimina_propiedades_negadas([color=>blanco, not(color=>rojo), bonita, not(movil), not(color=>[blanco]), color=>[rojo, azul]], X).
 * ?- elimina_propiedades_negadas([movil, color=>blanco, not(color=>rojo), bonita, not(movil), not(color=>[blanco]), color=>[rojo, azul]], X).
 */
elimina_propiedades_negadas([], []) :- !.
elimina_propiedades_negadas([P|O], Res) :-
    prop_negada(P, NotP),
    (elimina_propiedad_de_propiedades(NotP, O, Os); identidad(O, Os)),
    elimina_propiedades_negadas(Os, R),
    append([P], R, Res).

/*
 * Obtiene las propiedades de una lista de clases.
 */
propiedades_de_clases(_, [], []) :- !.
propiedades_de_clases(KB, [Clase|Clases], Propiedades) :-
    obten_clase(KB, Clase, clase(Clase, _, Props, _, _)),
    propiedades_de_clases(KB, Clases, PropsSup),
    append(Props, PropsSup, Propiedades).

/*
 * Obtiene la lista de propiedades de una clase, ya sea por
 * declaración o por herencia.
 * ?- kb(KB), propiedades_de_clase(KB, girasol, Props).
 * ?- kb(KB), propiedades_de_clase(KB, vegetal, Props).
 * ?- kb(KB), propiedades_de_clase(KB, rosa, Props).
 */
propiedades_de_clase(KB, Clase, Propiedades) :-
    obten_clase(KB, Clase, clase(Clase, _, Props, _, _)),
    superclases(KB, Clase, SuperClases),
    propiedades_de_clases(KB, SuperClases, PropsSup),
    append(Props, PropsSup, PropsF),
    elimina_propiedades_negadas(PropsF, Propiedades),
    !.

/*
 * Obtiene una lista de las propiedades que tiene un objeto.
 * ?- kb(KB), propiedades_de_objeto(KB, r3, Props).
 * ?- kb(KB), propiedades_de_objeto(KB, gira2, Props).
 * ?- kb(KB), propiedades_de_objeto(KB, gira1, Props).
 * ?- kb(KB), propiedades_de_objeto(KB, r1, Props).
 * ?- kb(KB), propiedades_de_objeto(KB, r2, Props).
 * ?- kb(KB), propiedades_de_objeto(KB, r3, Props).
 */
propiedades_de_objeto(KB, Objeto, Propiedades) :-
    obten_objeto(KB, Objeto, objeto(_, Clase, Props, _)),
    propiedades_de_clase(KB, Clase, ClsProps),
    append(Props, ClsProps, PropsFin),
    elimina_propiedades_negadas(PropsFin, Propiedades).

/*
 *
 * Verifica si dado el nombre de una propiedad y un valor, estos
 * forman parte de la lista de propiedades.
 * %?- existe_valor_en_propiedades(alias, chulote, [color=>rojo,alias=>[rojito,chulote]]).
 * %?- existe_valor_en_propiedades(color, rojo, [color=>rojo,alias=>[rojito,chulote]]).
 * %?- existe_valor_en_propiedades(color, azul, [color=>rojo,alias=>[rojito,chulote]]).
 * %?- existe_valor_en_propiedades(alias, azul, [color=>rojo,alias=>[rojito,chulote]]).
 * %?- existe_valor_en_propiedades(not(color), negro, [not(movil), not(color=>negro)]).
 * %?- existe_valor_en_propiedades(color, negro, [not(movil), not(color=>negro)]).
 */
existe_valor_en_propiedades(not(Prop), Valor, Propiedades) :-
    member(not(Prop=>_), Propiedades),
    obten_propiedad_completa(not(Prop), Propiedades, not(Prop=>Valores)),
    (member(Valor, Valores); Valor = Valores), !.
existe_valor_en_propiedades(Prop, Valor, Propiedades) :-
    member(Prop=>_, Propiedades),
    obten_propiedad_completa(Prop, Propiedades, Prop=>Valores),
    (member(Valor, Valores); Valor = Valores), !.


/*
 * Verifica si un objeto o clase tiene una propiedad
 * determinada. Puede preguntar de tres formas.
 *
 * - El objeto o clase tiene la propiedad con valor o forma parte de
 * la lista de valores.
 * - El objeto o clase tiene la propiedad con o sin valor (no importa el
 *   valor).
 * - El objeto o clase no tiene la propiedad con valor o forma parte
 * de la lista de valores.
 * - El objeto o clase no tiene la propiedad con o sin valor (no importa
 *   el valor).
 */
tiene_propiedad(not(Prop=>Val), clase(_, _, Props, _, _)) :-
    member(not(Prop=>Val), Props);
    existe_valor_en_propiedades(not(Prop), Val, Props).
tiene_propiedad(Prop=>Val, clase(_, _, Props, _, _)) :-
    member(Prop=>Val, Props); existe_valor_en_propiedades(Prop, Val, Props).
tiene_propiedad(Prop, clase(_, _, Props, _, _)) :-
    member(Prop, Props); member(Prop=>_, Props).
tiene_propiedad(not(Prop), clase(_, _, Props, _, _)) :-
    member(not(Prop), Props); member(not(Prop=>_), Props).
tiene_propiedad(not(Prop=>Val), objeto(_, _, Props, _)) :-
    member(not(Prop=>Val), Props);
    existe_valor_en_propiedades(not(Prop), Val, Props).
tiene_propiedad(Prop=>Val, objeto(_, _, Props, _)) :-
    member(Prop=>Val, Props); existe_valor_en_propiedades(Prop, Val, Props).
tiene_propiedad(Prop, objeto(_, _, Props, _)) :-
    member(Prop, Props); member(Prop=>_, Props).
tiene_propiedad(not(Prop), objeto(_, _, Props, _)) :-
    member(not(Prop), Props), member(not(Prop=>_), Props).


/*
 * Remueve duplicados de una lista.
 * %?- rem_dups([a, b, b, a, c, c, d, d, d, e], Result).
 */
rem_dups([],[]).
rem_dups([X|Rest], Result) :- member(X,Rest), rem_dups(Rest,Result), !.
rem_dups([X|Rest], [X|Rest1]) :- not(member(X,Rest)), rem_dups(Rest,Rest1), !.


/*
 * Encuentra todos los objetos de la base de conocimiento que
 * satisfacen la propiedad Prop.
 * %?- kb(KB), objetos_con_propiedad(KB, color, Objetos).
 * %?- kb(KB), objetos_con_propiedad(KB, color=>rojo, Objetos).
 * %?- kb(KB), objetos_con_propiedad(KB, alias, Objetos).
 * %?- kb(KB), objetos_con_propiedad(KB, alias=>rojito, Objetos).
 * %?- kb(KB), objetos_con_propiedad(KB, not(color=>rojo), Objetos).
 */
objetos_con_propiedad(KB, Prop, Objetos) :-
    lista_de_objetos(KB, Objs),
    include(tiene_propiedad(Prop), Objs, Objetos).

/*
 * Dada una lista de clases, obtiene la lista de todas las clases que
 * tienen una propiedad negada.
 * %?- kb(KB), lista_de_clases(KB, Clases), clases_con_propiedad_negada(movil, Clases, ClsPropNeg).
 */
clases_con_propiedad_negada(Prop, Clases, ClasesPropNegada) :-
    prop_negada(Prop, NotProp),
    include(tiene_propiedad(NotProp), Clases, ClasesPropNegada).

/*
 * Encuentra todas las clases de la base de conocimiento que
 * satisfacen la propiedad Prop.
 * %?- kb(KB), clases_con_propiedad(KB, movil, Clases).
 * %?- kb(KB), clases_con_propiedad(KB, color, Clases).
 * %?- kb(KB), clases_con_propiedad(KB, not(movil), Clases).
 * %?- kb(KB), clases_con_propiedad(KB, alias, Clases).
 * %?- kb(KB), clases_con_propiedad(KB, color=>amarillo, Clases).
 * %?- kb(KB), clases_con_propiedad(KB, not(color=>negro), Clases).
 */
clases_con_propiedad(KB, Prop, Clases) :-
    lista_de_clases(KB, Cls),
    include(tiene_propiedad(Prop), Cls, ClsProp),
    nombre_clases(ClsProp, NomClsProp),
    s_(KB, NomClsProp, SubClases),
    clases_con_propiedad_negada(Prop, SubClases, NotSubClases),
    subtract(SubClases, NotSubClases, NSubClases),
    append(ClsProp, NSubClases, Clases).

/*
 * Dada una lista de clases, obtiene todos los objetos asociados a las
 * clases.
 * %?- kb(KB), extrae_objetos_de_clases(KB, [clase(perro, animal, [canino], [odia=>[gato]], [abc, def])], Objetos).
 */
extrae_objetos_de_clases(_, [], []) :- !.
extrae_objetos_de_clases(KB, [clase(_, _, _, _, NomObjs)|Otros], Objetos) :-
    obten_objetos(KB, NomObjs, Objs),
    extrae_objetos_de_clases(KB, Otros, NObjs),
    append(Objs, NObjs, Objetos).

/*
 * Dada una lista de objetos, obtiene todos los objetos que tienen una
 * propiedad negada.
 */
objetos_con_propiedad_negada(Prop, Objetos, ObjetosPropNegada) :-
    prop_negada(Prop, NotProp),
    include(tiene_propiedad(NotProp), Objetos, ObjetosPropNegada).

/*
 * Dada una propiedad, obtiene todos los objetos con esa propiedad a
 * partir de la información de la base de conocimiento.
 */
objetos_de_clase_con_propiedad(KB, Prop, Objetos) :-
    clases_con_propiedad(KB, Prop, ClsProp),
    extrae_objetos_de_clases(KB, ClsProp, ObjsCls),
    objetos_con_propiedad_negada(Prop, ObjsCls, ObjsPropNegada),
    subtract(ObjsCls, ObjsPropNegada, Objetos).

/*
 * Dada una propiedad, obtiene todos los objetos que están en la
 * extensión de la propiedad de una base de conocimiento.
 * %?- kb(KB), extension_propiedad(KB, movil, Objetos).
 * %?- kb(KB), extension_propiedad(KB, not(movil), Objetos).
 */
% FIXME: Arreglar el caso esquinado de la extensión de propiedad, la
% cual no debería de mostrar objetos que tienen negaciones
% particulares de una lista de valores de propiedad. Por ejemplo, el
% caso de la clase girasol que tiene multiples colores y tiene un
% objeto que niega uno de esos colores.
extension_de_propiedad(KB, Prop, Objetos) :-
    objetos_con_propiedad(KB, Prop, ObjsProp),
    objetos_de_clase_con_propiedad(KB, Prop, ObjsCls),
    append(ObjsProp, ObjsCls, ObjsMerge),
    rem_dups(ObjsMerge, Objetos).

/*
 * Sugar function para definir una función identidad.
 */
identidad(X, X).

/*
 * Dado el nombre de una propiedad y una lista de propiedades, obtiene
 * la propiedad completa.
 */
obten_propiedad(_, [], _) :- fail.
obten_propiedad(not(Prop=>Valor), _, not(Prop=>Valor)) :- !.
obten_propiedad(not(Prop), [not(Prop=>Valor)|_], not(Prop=>Valor)) :- !.
obten_propiedad(not(Prop), [not(Prop)|_], not(Prop)) :- !.
obten_propiedad(not(Prop), [_|OtrasProps], P) :- obten_propiedad(not(Prop), OtrasProps, P).
obten_propiedad(Prop=>Valor, _, Prop=>Valor) :- !.
obten_propiedad(Prop, [Prop=>Valor|_], Prop=>Valor) :- !.
obten_propiedad(Prop, [Prop|_], Prop) :- !.
obten_propiedad(Prop, [_|OtrasProps], P) :- obten_propiedad(Prop, OtrasProps, P).


/*
 * Dada una lista de objetos y una propiedad, construye una lista con
 * el nombre y la propiedad.
 */
nombre_de_objeto_con_propiedad(_, [], _, []) :- !.
nombre_de_objeto_con_propiedad(KB, [objeto(Nombre, Clase, Props, _)|OtrosObjs],Prop,[Resultado|OtrosRes]) :-
    (obten_propiedad(Prop, Props, Val); (propiedades_de_clase(KB, Clase, Propiedades),
                                         obten_propiedad(Prop, Propiedades, Val))),
    identidad(Nombre=>(Val), Resultado),
    nombre_de_objeto_con_propiedad(KB, OtrosObjs, Prop, OtrosRes).

/*
 * Imprime de forma "bonita" la extensión de una propiedad.
 * ?- kb(KB), pprint_extension_de_propiedad(KB, color=>amarillo, Objs).
 * kb(KB), pprint_extension_de_propiedad(KB, color, Objs).
 * kb(KB), pprint_extension_de_propiedad(KB, color=>rojo, Objs).
 * kb(KB), pprint_extension_de_propiedad(KB, color=>[amarillo, rojo], Objs). <- gira1 no debería aparecer.
 * kb(KB), pprint_extension_de_propiedad(KB, not(movil), Objs).
 * kb(KB), pprint_extension_de_propiedad(KB, movil, Objs).
 * kb(KB), pprint_extension_de_propiedad(KB, canino, Objs).
 * kb(KB), pprint_extension_de_propiedad(KB, nono, Objs).
 */
pprint_extension_de_propiedad(KB, Prop, Objetos) :-
    extension_de_propiedad(KB, Prop, ObjsComp),
    nombre_de_objeto_con_propiedad(KB, ObjsComp, Prop, Objetos).

/*
 * Dada una propiedad, obtiene el nombre de todos los objetos que
 * están bajo la extension de la propiedad.
 */
extension_de_propiedad_nombres(KB, Prop, NombreObjetos) :-
    extension_de_propiedad(KB, Prop, Objetos),
    nombre_objetos(Objetos, NombreObjetos).
