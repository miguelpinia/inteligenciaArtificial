:- [consulta].

/*
 * Predicados principales:
 * - agrega_objeto(KB, Nombre, Clase, Propiedades, Relaciones, NuevaKB).
 * - agrega_clase(KB, Clase, SuperClase, Propiedades, Relaciones, Objetos, NuevaKB).
*/

% TODO: Al agregar la negación de un predicado o relación, eliminar el predicado que no estaba negado de la lista de propiedades o relaciones.
% TODO: Al agregar propiedades o relaciones, checar el tipo.

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

/*
 * Dado un valor Val, agrega dicho valor al principio una lista de valores Vals y
 * guarda el resultado en NVals.
 * ?- agrega_valor_a_lista(5, [1, 2, 3, 4], Res).
 * ?- agrega_valor_a_lista([5], [1, 2, 3, 4], Res).
 */
agrega_valor_a_lista(Val, Vals, NVals) :-
    (not(is_list(Val)), ((is_list(Vals), append([Val], Vals, NVals));
                         (not(is_list(Vals)), append([Val], [Vals], NVals))), !);
    (is_list(Val), ((is_list(Vals), append(Val, Vals, NVals));
                     (not(is_list(Vals)), append(Val, [Vals], NVals))), !).

/*
 * Dado un elemento de la forma not(foo=>bar), foo=>bar ó foo, agrega
 * dicho elemento a la lista Elementos y guarda el resultado en
 * NElementos.
 * ?- agrega_elemento_a_lista(not(hate=>foo), [hate=>[human]], Res).
 * ?- agrega_elemento_a_lista(hate=>foo, [hate=>[human]], Res).
 * ?- agrega_elemento_a_lista(not(hate), [hate=>[human]], Res).
 * ?- agrega_elemento_a_lista(hate, [hate=>[human]], Res).
 */
agrega_elemento_a_lista(not(Elemento=>Val), Elementos, NElementos) :-
    is_list(Val), append([not(Elemento=>Val)], Elementos, NElementos), !.
agrega_elemento_a_lista(Elemento=>Val, Elementos, NElementos) :-
    is_list(Val), append([Elemento=>Val], Elementos, NElementos), !.
agrega_elemento_a_lista(not(Elemento), Elementos, NElementos) :-
    append([not(Elemento)], Elementos, NElementos), !.
agrega_elemento_a_lista(Elemento, Elementos, NElementos) :-
    append([Elemento], Elementos, NElementos).

/*
 * Agrega la relación dada a la clase Clase. Si el valor de la
 * relación ya existe, no agrega la relación. Si el valor no existe,
 * pero ya hay valores previos para la relación, entonces agrega el
 * nuevo valor a la lista de valores.
 *
 * %?- kb(KB), agrega_relacion_a_clase(KB, not(ama=>avestruz), perro, NKB), obten_clase(NKB, perro, Clase).
 * %?- kb(KB), agrega_relacion_a_clase(KB, not(ama=>[avestruz]), perro, NKB), obten_clase(NKB, perro, Clase).
 * %?- kb(KB), agrega_relacion_a_clase(KB, not(odia=>avestruz), perro, NKB), obten_clase(NKB, perro, Clase).
 * %?- kb(KB), agrega_relacion_a_clase(KB, odia=>avestruz, perro, NKB), obten_clase(NKB, perro, Clase).
 * %?- kb(KB), agrega_relacion_a_clase(KB, odia=>[avestruz], perro, NKB), obten_clase(NKB, perro, Clase).
 * %?- kb(KB), agrega_relacion_a_clase(KB, ama=>avestruz, perro, NKB), obten_clase(NKB, perro, Clase).
 * %?- kb(KB), agrega_relacion_a_clase(KB, odia=>gato, perro, NKB), obten_clase(NKB, perro, Clase). <- Debe fallar.
 */
agrega_relacion_a_clase(KB, not(Rel=>Val), Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    tiene_relacion_nombre(not(Rel), Rels),
    not(existe_valor_en_relaciones(not(Rel), Val, Rels)),
    obten_relacion_completa(not(Rel), Rels, not(Rel=>Vals)),
    agrega_valor_a_lista(Val, Vals, NVals),
    reemplaza(Rels, not(Rel=>Vals), not(Rel=>NVals), NRels),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, Props, NRels, Objs),
              NuevaKB), !.
agrega_relacion_a_clase(KB, not(Rel=>Val), Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    not(tiene_relacion_nombre(not(Rel), Rels)),
    agrega_elemento_a_lista(not(Rel=>Val), Rels, NRels),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, Props, NRels, Objs),
              NuevaKB), !.
agrega_relacion_a_clase(KB, Rel=>Val, Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    tiene_relacion_nombre(Rel, Rels),
    not(existe_valor_en_relaciones(Rel, Val, Rels)),
    obten_relacion_completa(Rel, Rels, Rel=>Vals),
    agrega_valor_a_lista(Val, Vals, NVals),
    reemplaza(Rels, Rel=>Vals, Rel=>NVals, NRels),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, Props, NRels, Objs),
              NuevaKB), !.
agrega_relacion_a_clase(KB, Rel=>Val, Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    not(tiene_relacion_nombre(Rel, Rels)),
    agrega_elemento_a_lista(Rel=>Val, Rels, NRels),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, Props, NRels, Objs),
              NuevaKB), !.

/*
 * Agrega la relación dada al objeto Objeto. Sie el valor de la
 * relación ya existe, no agrega la relación. Si el valor no existe,
 * pero ya hay valores previos para la relación, entonces agregar el
 * nuevo valor a la lista de valores.
 * %?- kb(KB), agrega_relacion_a_objeto(KB, muerde=>r3, abc, NKB), obten_objeto(NKB, abc, Obj).
 * %?- kb(KB), agrega_relacion_a_objeto(KB, muerde=>[r3], abc, NKB), obten_objeto(NKB, abc, Obj).
 * %?- kb(KB), agrega_relacion_a_objeto(KB, not(odia=>[r3]), def, NKB), obten_objeto(NKB, def, Obj).
 * %?- kb(KB), agrega_relacion_a_objeto(KB, not(odia=>r3), def, NKB), obten_objeto(NKB, def, Obj).
 * %?- kb(KB), agrega_relacion_a_objeto(KB, not(ama=>r3), def, NKB), obten_objeto(NKB, def, Obj).
 * %?- kb(KB), agrega_relacion_a_objeto(KB, ama=>abc, def, NKB), obten_objeto(NKB, def, Obj).
 */
agrega_relacion_a_objeto(KB, not(Rel=>Val), Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    tiene_relacion_nombre(not(Rel), Rels),
    not(existe_valor_en_relaciones(not(Rel), Val, Rels)),
    obten_relacion_completa(not(Rel), Rels, not(Rel=>Vals)),
    agrega_valor_a_lista(Val, Vals, NVals),
    reemplaza(Rels, not(Rel=>Vals), not(Rel=>NVals), NRels),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, Props, NRels),
              NuevaKB), !.
agrega_relacion_a_objeto(KB, not(Rel=>Val), Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    not(tiene_relacion_nombre(not(Rel), Rels)),
    agrega_elemento_a_lista(not(Rel=>Val), Rels, NRels),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, Props, NRels),
              NuevaKB).
agrega_relacion_a_objeto(KB, Rel=>Val, Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    tiene_relacion_nombre(Rel, Rels),
    not(existe_valor_en_relaciones(Rel, Val, Rels)),
    obten_relacion_completa(Rel, Rels, Rel=>Vals),
    agrega_valor_a_lista(Val, Vals, NVals),
    reemplaza(Rels, Rel=>Vals, Rel=>NVals, NRels),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, Props, NRels),
              NuevaKB), !.
agrega_relacion_a_objeto(KB, Rel=>Val, Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    not(tiene_relacion_nombre(Rel, Rels)),
    agrega_elemento_a_lista(Rel=>Val, Rels, NRels),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, Props, NRels),
              NuevaKB).

/*
 * Agrega la propiedad dada a la clase Clase. Si el valor de la
 * propiedad ya existe, no agrega la propiedad. Si el valor no existe,
 * pero ya hay valores previos para la propiedad, entonces agrega el
 * nuevo valor a la lista de valores de la propiedad.
 * %?- kb(KB), agrega_propiedad_a_clase(KB, color=>azul, girasol, NKB), obten_clase(NKB, girasol, Clase).
 * %?- kb(KB), agrega_propiedad_a_clase(KB, volador, rosa, NKB), obten_clase(NKB, rosa, Clase).
 * %?- kb(KB), agrega_propiedad_a_clase(KB, not(volador), rosa, NKB), obten_clase(NKB, rosa, Clase).
 */
agrega_propiedad_a_clase(KB, not(Prop=>Val), Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    tiene_propiedad_nombre(not(Prop), Props),
    not(existe_valor_en_propiedades(not(Prop), Val, Props)),
    obten_propiedad_completa(not(Prop), Props, not(Prop=>Vals)),
    agrega_valor_a_lista(Val, Vals, NVals),
    reemplaza(Props, not(Prop=>Vals), not(Prop=>NVals), NProps),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, NProps, Rels, Objs),
              NuevaKB), !.
agrega_propiedad_a_clase(KB, not(Prop=>Val), Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    not(tiene_propiedad_nombre(not(Prop), Props)),
    agrega_elemento_a_lista(not(Prop=>Val), Props, NProps),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, NProps, Rels, Objs),
              NuevaKB), !.
agrega_propiedad_a_clase(KB, Prop=>Val, Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    tiene_propiedad_nombre(Prop, Props),
    not(existe_valor_en_propiedades(Prop, Val, Props)),
    obten_propiedad_completa(Prop, Props, Prop=>Vals),
    agrega_valor_a_lista(Val, Vals, NVals),
    reemplaza(Props, Prop=>Vals, Prop=>NVals, NProps),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, NProps, Rels, Objs),
              NuevaKB), !.
agrega_propiedad_a_clase(KB, Prop=>Val, Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    not(tiene_propiedad_nombre(Prop, Props)),
    agrega_elemento_a_lista(Prop=>Val, Props, NProps),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, NProps, Rels, Objs),
              NuevaKB), !.
agrega_propiedad_a_clase(KB, not(Prop), Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    not(tiene_propiedad_nombre(not(Prop), Props)),
    agrega_elemento_a_lista(not(Prop), Props, NProps),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, NProps, Rels, Objs),
              NuevaKB), !.
agrega_propiedad_a_clase(KB, Prop, Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    not(tiene_propiedad_nombre(Prop, Props)),
    agrega_elemento_a_lista(Prop, Props, NProps),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, NProps, Rels, Objs),
              NuevaKB), !.

/*
 * Agrega la propiedad dada a la clase Clase. Si el valor de la
 * propiedad ya existe, no agrega la propiedad. Si el valor no existe,
 * pero ya hay valores previos para la propiedad, entonces agrega el
 * nuevo valor a la lista de valores de la propiedad.
 * %?- kb(KB), agrega_propiedad_a_objeto(KB, alias=>'Hijo de satán', abc, NKB), obten_objeto(NKB, abc, Clase).
 * %?- kb(KB), agrega_propiedad_a_objeto(KB, not(volador), abc, NKB), obten_objeto(NKB, abc, Clase).
 * %?- kb(KB), agrega_propiedad_a_objeto(KB, color=>blanco, abc, NKB), obten_objeto(NKB, abc, Clase).
 */
agrega_propiedad_a_objeto(KB, not(Prop=>Val), Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    tiene_propiedad_nombre(not(Prop), Props),
    not(existe_valor_en_propiedades(not(Prop), Val, Props)),
    obten_propiedad_completa(not(Prop), Props, not(Prop=>Vals)),
    agrega_valor_a_lista(Val, Vals, NVals),
    reemplaza(Props, not(Prop=>Vals), not(Prop=>NVals), NProps),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, NProps, Rels),
              NuevaKB),!.
agrega_propiedad_a_objeto(KB, not(Prop=>Val), Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    not(tiene_propiedad_nombre(not(Prop), Props)),
    agrega_elemento_a_lista(not(Prop=>Val), Props, NProps),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, NProps, Rels),
              NuevaKB), !.
agrega_propiedad_a_objeto(KB, Prop=>Val, Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    tiene_propiedad_nombre(Prop, Props),
    not(existe_valor_en_propiedades(Prop, Val, Props)),
    obten_propiedad_completa(Prop, Props, Prop=>Vals),
    agrega_valor_a_lista(Val, Vals, NVals),
    reemplaza(Props, Prop=>Vals, Prop=>NVals, NProps),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, NProps, Rels),
              NuevaKB), !.
agrega_propiedad_a_objeto(KB, Prop=>Val, Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    not(tiene_propiedad_nombre(Prop, Props)),
    agrega_elemento_a_lista(Prop=>Val, Props, NProps),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, NProps, Rels),
              NuevaKB), !.
agrega_propiedad_a_objeto(KB, not(Prop), Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    not(tiene_propiedad_nombre(not(Prop), Props)),
    agrega_elemento_a_lista(not(Prop), Props, NProps),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, NProps, Rels),
              NuevaKB), !.
agrega_propiedad_a_objeto(KB, Prop, Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    not(tiene_propiedad_nombre(Prop, Props)),
    agrega_elemento_a_lista(Prop, Props, NProps),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, NProps, Rels),
              NuevaKB), !.
