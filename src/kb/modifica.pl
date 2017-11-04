:- [util, consulta].

/*
 * Predicados principales:
 * modifica_nombre_de_clase(KB, NuevaClase, Clase, NuevaKB)
 * modifica_nombre_de_objeto(KB, NuevoObjeto, Objeto, NuevaKB)
 */

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
 *
 * ?- kb(KB), modifica_nombre_de_clase_a_objetos(KB, top, [r1, r2], NKB), obten_objeto(NKB, r2, R2), obten_objeto(NKB, r1, R1), obten_clase(NKB, top, Top), obten_clase(NKB, rosa, Rosa).
 */
modifica_nombre_de_clase_a_objetos(KB, _, [], KB) :- !.
modifica_nombre_de_clase_a_objetos(KB, NuevaClase, [Objeto|Objetos], NuevaKB) :-
    modifica_nombre_de_clase_a_objeto(KB, NuevaClase, Objeto, NKB),
    modifica_nombre_de_clase_a_objetos(NKB, NuevaClase, Objetos, NuevaKB), !.

/*
 * Modifica el nombre de la superclase a la clase Clase por
 * NuevaSuperClase.
 *
 * ?- kb(KB), modifica_nombre_de_superclase_a_clase(KB, top, rosa, NKB), obten_clase(NKB, rosa, Rosa).
 */
modifica_nombre_de_superclase_a_clase(KB, NuevaSuperClase, Clase, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, NuevaSuperClase, Props, Rels, Objs),
              NuevaKB), !.


/*
 * modifica_nombre_de_superclase_a_clases(KB, NuevaSuperClase, ListaClases, NuevaKB)
 * Dada la lista de nombre de clases ListaClases, modifica la
 * superclase en la clase de cada una de ellas por NuevaSuperClase.
 */
modifica_nombre_de_superclase_a_clases(KB, _, [], KB) :- !.
modifica_nombre_de_superclase_a_clases(KB, NuevaSuperClase, [Clase|Clases], NuevaKB) :-
    modifica_nombre_de_superclase_a_clase(KB, NuevaSuperClase, Clase, NKB),
    modifica_nombre_de_superclase_a_clases(NKB, NuevaSuperClase, Clases, NuevaKB), !.

reemplaza_nombre_de_clase_a_objeto(KB, NuevaClase, Objeto, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, NuevaClase, Props, Rels),
              NuevaKB), !.

reemplaza_nombre_de_clase_a_objetos(KB, _, [], KB) :- !.
reemplaza_nombre_de_clase_a_objetos(KB, NuevaClase, [Objeto|Objetos], NuevaKB) :-
    reemplaza_nombre_de_clase_a_objeto(KB, NuevaClase, Objeto, NKB),
    reemplaza_nombre_de_clase_a_objetos(NKB, NuevaClase, Objetos, NuevaKB), !.

/*
 * reemplaza_de_lista_de_relaciones(Relaciones, Viejo, Nuevo, NuevasRelaciones)
 *
 * Reemplaza el valor de Viejo por Nuevo en la lista de Relaciones y
 * almacenando el resultado en NuevasRelaciones.
 *
 */
reemplaza_de_lista_de_relaciones([Rel|Resto], Viejo, Nuevo, [NuevaRelacion|NuevoResto]) :-
    reemplaza_de_relacion(Rel, Viejo, Nuevo, NuevaRelacion),
    reemplaza_de_lista_de_relaciones(Resto, Viejo, Nuevo, NuevoResto),
    !.
reemplaza_de_lista_de_relaciones([], _, _, []).

/*
 * Reemplaza un término por otro dentro de una lista de valores de una
 * relación.
 */
reemplaza_de_relacion(not(Atr=>Valores), Viejo, Nuevo, not(Atr=>NuevosValores)) :-
    reemplaza(Valores, Viejo, Nuevo, NuevosValores),
    !.
reemplaza_de_relacion(Atr=>Valores, Viejo, Nuevo, Atr=>NuevosValores) :-
    reemplaza(Valores, Viejo, Nuevo, NuevosValores).

/*
 * Reemplaza un término por otro dentro de la lista de relaciones de
 * un objeto.
 */
reemplaza_de_relaciones_de_objeto(KB, Objeto, Viejo, Nuevo, NuevaKB) :-
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    !,
    reemplaza_de_lista_de_relaciones(Rels, Viejo, Nuevo, NRels),
    reemplaza(KB, objeto(Objeto, Clase, Props, Rels),
              objeto(Objeto, Clase, Props, NRels),
              NuevaKB).

/*
 * Reemplaza un término por otro dentro de la lista de relaciones de
 * una clase.
 */
reemplaza_de_relaciones_de_clase(KB, Clase, Viejo, Nuevo, NuevaKB) :-
    obten_clase(KB, Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    reemplaza_de_lista_de_relaciones(Rels, Viejo, Nuevo, NRels),
    reemplaza(KB, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(Clase, SuperClase, Props, NRels, Objs),
              NuevaKB),
    !.

reemplaza_de_relaciones_de_clases(KB, [], _, _, KB) :- !.
reemplaza_de_relaciones_de_clases(KB, [Clase|Clases], Viejo, Nuevo, NuevaKB) :-
    reemplaza_de_relaciones_de_clase(KB, Clase, Viejo, Nuevo, NKB),
    reemplaza_de_relaciones_de_clases(NKB, Clases, Viejo, Nuevo, NuevaKB).

reemplaza_de_relaciones_de_objetos(KB, [], _, _, KB) :- !.
reemplaza_de_relaciones_de_objetos(KB, [Objeto|Objetos], Viejo, Nuevo, NuevaKB) :-
    reemplaza_de_relaciones_de_objeto(KB, Objeto, Viejo, Nuevo, NKB),
    reemplaza_de_relaciones_de_objetos(NKB, Objetos, Viejo, Nuevo, NuevaKB).

/*
 * Remplaza un término por otro dentro de las relaciones de todas las
 * clases y objetos de la base de conocimiento.
 */
reemplaza_de_relaciones(KB, Viejo, Nuevo, NuevaKB) :-
    extension_de_clase(KB, top, Objetos),
    reemplaza_de_relaciones_de_objetos(KB, Objetos, Viejo, Nuevo, NKB),
    subclases(NKB, top, Clases),
    reemplaza_de_relaciones_de_clases(NKB, Clases, Viejo, Nuevo, NuevaKB).


/*
 * Modifica el nombre de una clase.
 * ?- kb(KB), modifica_nombre_de_clase(KB, catdog, gato, NKB), imprime(NKB).
 */
modifica_nombre_de_clase(KB, NuevaClase, Clase, NuevaKB) :-
    obten_clase(KB,  Clase, clase(Clase, SuperClase, Props, Rels, Objs)),
    hijos(KB, Clase, Hijos),
    nombre_clases(Hijos, NHijos),
    modifica_nombre_de_superclase_a_clases(KB, NuevaClase, NHijos, NKB),
    reemplaza_nombre_de_clase_a_objetos(NKB, NuevaClase, Objs, NKB1),
    reemplaza_de_relaciones(NKB1, Clase, NuevaClase, NKB2),
    reemplaza(NKB2, clase(Clase, SuperClase, Props, Rels, Objs),
              clase(NuevaClase, SuperClase, Props, Rels, Objs),
              NuevaKB), !.

/*
 * Modifica el nombre de un objeto.
 * ?- kb(KB), modifica_nombre_de_objeto(KB, rosita2, r2, NKB), imprime(NKB).
 */
modifica_nombre_de_objeto(KB, NuevoObjeto, Objeto, NuevaKB) :-
    extension_de_clase(KB, top, Objetos),
    not(member(NuevoObjeto, Objetos)),
    obten_objeto(KB, Objeto, objeto(Objeto, Clase, Props, Rels)),
    obten_clase(KB, Clase, clase(Clase, SuperClase, CProps, CRels, Objs)),
    reemplaza(Objs, Objeto, NuevoObjeto, NObjs),
    reemplaza_de_relaciones(KB, Objeto, NuevoObjeto, NKB),
    reemplaza(NKB, clase(Clase, SuperClase, CProps, CRels, Objs),
              clase(Clase, SuperClase, CProps, CRels, NObjs),
              NKB1),
    reemplaza(NKB1, objeto(Objeto, Clase, Props, Rels),
              objeto(NuevoObjeto, Clase, Props, Rels),
              NuevaKB), !.

/*
 * modifica_propiedad_de_clase(KB,Prop,Clase,NuevaKB)
 * reemplaza la aparicion de la propiedad Prop existente por Prop
 * Prop puede tener la forma
 * not(Prop=>Valor)
 * Prop=>Valor
 * not(Prop)
 * Prop
 * En caso de no existir, no hay cambio.
 * El caso de Prop y not(Prop) es especial,
 * En este caso deberá existir el contrario
 */
modifica_propiedad_de_clase(KB,not(Prop=>Valor),Clase,NuevaKB):-
    /* Checamos si ya esta definido  y que sea de la misma manera*/
    propiedades_de_clase(KB,Clase,Props),
    member(not(Prop=>_),Props),
    /* Propiedades negativas atr=>valor el valor siempre debe de ser una lista*/
    is_list(Valor),
    obten_clase(KB, Clase, clase(Clase, Super,Propiedades, Relaciones, Objetos)),
    (
        (/*Si es miembro*/
            member(not(Prop=>_),Propiedades),
            delete(Propiedades,not(Prop=>_),NProp),
            fusiona([not(Prop=>Valor)|NProp],NuevasPropiedades)
        );
        (/*Sino*/
            fusiona([not(Prop=>Valor)|Propiedades],NuevasPropiedades)
        )
    ),
    reemplaza(KB,clase(Clase, Super,Propiedades, Relaciones, Objetos),clase(Clase, Super,NuevasPropiedades, Relaciones, Objetos),NuevaKB),
    !.

modifica_propiedad_de_clase(KB,(Prop=>Valor),Clase,NuevaKB):-
    /* Checamos si ya esta definido  y que sea de la misma manera*/
    propiedades_de_clase(KB,Clase,Props),
    member(Prop=>_,Props),
    obten_clase(KB, Clase, clase(Clase, Super,Propiedades, Relaciones, Objetos)),
    (
        (/*Si es miembro*/
            member(Prop=>_,Propiedades),
            delete(Propiedades,Prop=>_,NProp),
            fusiona([Prop=>Valor|NProp],NuevasPropiedades)
        );
        (/*Sino*/
            fusiona([Prop=>Valor|Propiedades],NuevasPropiedades)
        )
    ),
    reemplaza(KB,clase(Clase, Super,Propiedades, Relaciones, Objetos),clase(Clase, Super,NuevasPropiedades, Relaciones, Objetos),NuevaKB),
    !.

modifica_propiedad_de_clase(KB,not(Prop),Clase,NuevaKB):-
    dif(Prop,_=>_),
    propiedades_de_clase(KB,Clase,Props),
    member(Prop,Props),
    obten_clase(KB, Clase, clase(Clase, Super,Propiedades, Relaciones, Objetos)),
    fusiona([not(Prop)|Propiedades],NuevasPropiedades),
    reemplaza(KB,clase(Clase, Super,Propiedades, Relaciones, Objetos),clase(Clase, Super,NuevasPropiedades, Relaciones, Objetos),NuevaKB),
    !.

modifica_propiedad_de_clase(KB,Prop,Clase,NuevaKB):-
    dif(Prop,_=>_),
    propiedades_de_clase(KB,Clase,Props),
    member(not(Prop),Props),
    obten_clase(KB, Clase, clase(Clase, Super,Propiedades, Relaciones, Objetos)),
    fusiona([Prop|Propiedades],NuevasPropiedades),
    reemplaza(KB,clase(Clase, Super,Propiedades, Relaciones, Objetos),clase(Clase, Super,NuevasPropiedades, Relaciones, Objetos),NuevaKB),
    !.

modifica_propiedad_de_clase(KB,_,_,KB).

/*
 * modifica_propiedad_de_objeto(KB,Prop,Id,NuevaKB)
 * reemplaza la aparicion de la propiedad Prop existente por Prop
 * Prop puede tener la forma
 * not(Prop=>Valor)
 * Prop=>Valor
 * not(Prop)
 * Prop
 * En caso de no existir, no hay cambio.
 * El caso de Prop y not(Prop) es especial,
 * En este caso deberá existir el contrario
 */
modifica_propiedad_de_objeto(KB,not(Prop=>Valor),Id,NuevaKB):-
    /* Checamos si ya esta definido  y que sea de la misma manera*/
    propiedades_de_objeto(KB,Id,Props),
    member(not(Prop=>_),Props),
    /* Propiedades negativas atr=>valor el valor siempre debe de ser una lista*/
    (is_list(Valor);Valor = udf),
    obten_objeto(KB, Id, objeto(Id, Clase, Propiedades, Relaciones)),
    (
        (/*Si es miembro*/
            member(not(Prop=>_),Propiedades),
            delete(Propiedades,not(Prop=>_),NProp),
            fusiona([not(Prop=>Valor)|NProp],NuevasPropiedades)
        );
        (/*Sino*/
            fusiona([not(Prop=>Valor)|Propiedades],NuevasPropiedades)
        )
    ),
    reemplaza(KB,objeto(Id, Clase,Propiedades, Relaciones),objeto(Id, Clase,NuevasPropiedades, Relaciones),NuevaKB),
    !.

modifica_propiedad_de_objeto(KB,(Prop=>Valor),Id,NuevaKB):-
    /* Checamos si ya esta definido  y que sea de la misma manera*/
    propiedades_de_objeto(KB,Id,Props),
    member(Prop=>_,Props),
    obten_objeto(KB, Id, objeto(Id, Clase, Propiedades, Relaciones)),
    (
        (/*Si es miembro*/
            member(Prop=>_,Propiedades),
            delete(Propiedades,Prop=>_,NProp),
            fusiona([Prop=>Valor|NProp],NuevasPropiedades)
        );
        (/*Sino*/
            fusiona([Prop=>Valor|Propiedades],NuevasPropiedades)
        )
    ),
    reemplaza(KB,objeto(Id, Clase,Propiedades, Relaciones),objeto(Id, Clase,NuevasPropiedades, Relaciones),NuevaKB),
    !.

modifica_propiedad_de_objeto(KB,not(Prop),Id,NuevaKB):-
    dif(Prop,_=>_),
    propiedades_de_objeto(KB,Id,Props),
    member(Prop,Props),
    obten_objeto(KB, Id, objeto(Id, Clase, Propiedades, Relaciones)),
    fusiona([not(Prop)|Propiedades],NuevasPropiedades),
    reemplaza(KB,objeto(Id, Clase,Propiedades, Relaciones),objeto(Id, Clase,NuevasPropiedades, Relaciones),NuevaKB),
    !.

modifica_propiedad_de_objeto(KB,Prop,Id,NuevaKB):-
    dif(Prop,_=>_),
    propiedades_de_objeto(KB,Id,Props),
    member(not(Prop),Props),
    obten_objeto(KB, Id, objeto(Id, Clase, Propiedades, Relaciones)),
    fusiona([Prop|Propiedades],NuevasPropiedades),
    reemplaza(KB,objeto(Id, Clase,Propiedades, Relaciones),objeto(Id, Clase,NuevasPropiedades, Relaciones),NuevaKB),
    !.

modifica_propiedad_de_objeto(KB,_,_,KB).

/*
 * modifica_relacion_de_clase(KB,Rel,Clase,NuevaKB)
 * reemplaza la aparicion de la propiedad Prop existente por Prop
 * Prop puede tener la forma
 * not(Rel=>Valor)
 * Rel=>Valor
 * En caso de no existir, no hay cambio.
 */
modifica_relacion_de_clase(KB,not(Rel=>Valor),Clase,NuevaKB):-
    /* Checamos si ya esta definido  y que sea de la misma manera*/
    relaciones_de_clase(KB,Clase,Rels),
    member(not(Rel=>_),Rels),
    (is_list(Valor);Valor = udf),
    obten_clase(KB, Clase, clase(Clase, Super,Propiedades, Relaciones, Objetos)),
    (
        (/*Si es miembro*/
            member(not(Rel=>_),Relaciones),
            delete(Relaciones,not(Rel=>_),NRel),
            fusiona([not(Rel=>Valor)|NRel],NuevasRelaciones)
        );
        (/*Sino*/
            fusiona([not(Rel=>Valor)|Relaciones],NuevasRelaciones)
        )
    ),
    reemplaza(KB,clase(Clase, Super,Propiedades, Relaciones, Objetos),clase(Clase, Super,Propiedades, NuevasRelaciones, Objetos),NuevaKB),
    !.

modifica_relacion_de_clase(KB,Rel=>Valor,Clase,NuevaKB):-
    /* Checamos si ya esta definido  y que sea de la misma manera*/
    relaciones_de_clase(KB,Clase,Rels),
    member(Rel=>_,Rels),
    (is_list(Valor);Valor = udf),
    obten_clase(KB, Clase, clase(Clase, Super,Propiedades, Relaciones, Objetos)),
    (
        (/*Si es miembro*/
            member(Rel=>_,Relaciones),
            delete(Relaciones,Rel=>_,NRel),
            fusiona([Rel=>Valor|NRel],NuevasRelaciones)
        );
        (/*Sino*/
            fusiona([Rel=>Valor|Relaciones],NuevasRelaciones)
        )
    ),
    reemplaza(KB,clase(Clase, Super,Propiedades, Relaciones, Objetos),clase(Clase, Super,Propiedades, NuevasRelaciones, Objetos),NuevaKB),
    !.

modifica_relacion_de_clase(KB,_,_,KB).

/*
 * modifica_propiedad_de_objeto(KB,Rel,Clase,NuevaKB)
 * reemplaza la aparicion de la propiedad Prop existente por Prop
 * Prop puede tener la forma
 * not(Rel=>Valor)
 * Rel=>Valor
 * En caso de no existir, no hay cambio.
 */
modifica_relacion_de_objeto(KB,not(Rel=>Valor),Id,NuevaKB):-
    /* Checamos si ya esta definido  y que sea de la misma manera*/
    relaciones_de_objeto(KB,Id,Rels),
    member(not(Rel=>_),Rels),
    (is_list(Valor);Valor = udf),
    obten_objeto(KB, Id, objeto(Id, Clase, Propiedades, Relaciones)),
    (
        (/*Si es miembro*/
            member(not(Rel=>_),Relaciones),
            delete(Relaciones,not(Rel=>_),NRel),
            fusiona([not(Rel=>Valor)|NRel],NuevasRelaciones)
        );
        (/*Sino*/
            fusiona([not(Rel=>Valor)|Relaciones],NuevasRelaciones)
        )
    ),
    reemplaza(KB,objeto(Id, Clase,Propiedades, Relaciones),objeto(Id, Clase,Propiedades, NuevasRelaciones),NuevaKB),
    !.

modifica_relacion_de_objeto(KB,Rel=>Valor,Id,NuevaKB):-
    /* Checamos si ya esta definido  y que sea de la misma manera*/
    relaciones_de_objeto(KB,Id,Rels),
    member(Rel=>_,Rels),
    (is_list(Valor);Valor = udf),
    obten_objeto(KB, Id, objeto(Id, Clase, Propiedades, Relaciones)),
    (
        (/*Si es miembro*/
            member(Rel=>_,Relaciones),
            delete(Relaciones,Rel=>_,NRel),
            fusiona([Rel=>Valor|NRel],NuevasRelaciones)
        );
        (/*Sino*/
            fusiona([Rel=>Valor|Relaciones],NuevasRelaciones)
        )
    ),
    reemplaza(KB,objeto(Id, Clase,Propiedades, Relaciones),objeto(Id, Clase,Propiedades, NuevasRelaciones),NuevaKB),
    !.

modifica_relacion_de_objeto(KB,_,_,KB).
