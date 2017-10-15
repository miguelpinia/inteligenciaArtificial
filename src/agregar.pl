/* Agrega una clase */
agrega_clase(KB,Clase, Superclase, Propiedades, Relaciones,NuevaKB):-
	reloadKB(KB),
	agrega_clase(Clase, Superclase, Propiedades, Relaciones),
	kb(NuevaKB).

/* Agrega un objeto sin id*/
agrega_objeto_sin_id(KB,Clase, Propiedades, Relaciones, NuevaKB):-
	reloadKB(KB),
	agrega_objeto_sin_id(Clase, Propiedades, Relaciones),
	kb(NuevaKB).


/* Agrega un objeto con id*/
agrega_objeto_con_id(KB,Ref,Clase, Propiedades, Relaciones, NuevaKB):-
	reloadKB(KB),
	agrega_objeto_con_id(Ref,Clase, Propiedades, Relaciones),
	kb(NuevaKB).

/* Agrega una relacion en una clase */
agrega_relacion_a_clase(KB,Rel, Clase,NuevaKB):-
	reloadKB(KB),
	agrega_relacion_a_clase(Rel, Clase),
	kb(NuevaKB).

/* Agrega una relacion en un objeto */
agrega_relacion_a_objeto(KB,Rel, Ref,NuevaKB):-
	reloadKB(KB),
	agrega_relacion_a_objeto(Rel, Ref),
	kb(NuevaKB).

/* Agrega una propiedad a una clase */
agrega_propiedad_a_clase(KB,Prop, Clase,NuevaKB):-
	reloadKB(KB),
	agrega_propiedad_a_clase(Prop, Clase),
	kb(NuevaKB).

/* Agrega una propiedad a un objeto */
agrega_propiedad_a_objeto(KB,Prop, Ref,NuevaKB):-
	reloadKB(KB),
	agrega_propiedad_a_objeto(Prop, Ref),
	kb(NuevaKB).
