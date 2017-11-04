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
