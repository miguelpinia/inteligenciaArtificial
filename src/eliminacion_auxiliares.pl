/*
* eliminar_objeto(Ref)
* Elimina el objeto con referencia Ref de la base de datos,
* de igual forma elimina Ref de la lista de referencias a objetos de su clase.
*/
elimina_objeto(Ref) :-
	objeto(o(Ref,Clase,Props,Rels)),
	construye_objeto_term(Ref,Clase,Props,Rels,Term),
	elimina_objeto_de_clase(Ref,Clase),
	retract(Term),
	elimina_de_kb(Term),
    elimina_de_relaciones(Ref),
	!.

/*
* elimina_objeto_de_clase(Ref,Clase)
* Elimina la referencia Ref de la lista de referencias de objetos a la clase
*/
elimina_objeto_de_clase(Ref,Clase):-
	clase(c(Clase,Super,Props,Rels,Objetos)),
	delete(Objetos,Ref,ObjetosNuevos),
	construye_clase_term(Clase,Super,Props,Rels,Objetos,Term),
	construye_clase_term(Clase,Super,Props,Rels,ObjetosNuevos,TermNuevo),
	retract(Term),
	assertz(TermNuevo),
	elimina_de_kb(Term),
	agrega_a_kb(TermNuevo),
	!.

/*
* elimina_objetos(ObjetosRef)
* Elimina los objetos con referencia en la lista ObjetosRef
*/
elimina_objetos([]).
elimina_objetos([Objeto|MasObjetos]):-
	elimina_objeto(Objeto),
	elimina_objetos(MasObjetos).

/*
* elimina_clase(Clase)
* Elimina la clase Clase y a cambia la clase de sus objetos ejemplares
* y clases hijas a su clase padre.
* No podemos eliminar top
*/

elimina_clase(top):-!,fail.

elimina_clase(Clase):-
    clase(c(Clase,Super,Props,Rels,Objetos)),
    modifica_nombre_de_clase_a_objetos_no_actualiza(Super, Objetos),
    subclases_directas(Clase,Hijas),
    modifica_nombre_de_superclase_a_clases(Super,Hijas),
    agrega_lista_de_objetos_a_clase(Super,Objetos),
    elimina_de_relaciones(Clase),
    construye_clase_term(Clase,Super,Props,Rels,Objetos,Term),
    retract(Term),
    elimina_de_kb(Term),
    !.

/*
* Agrega Objetos a la lista de objetos de la clase Clase
* Solo se debería usar en la eliminación de clase ya que no hace ningún chequeo.
*/
agrega_lista_de_objetos_a_clase(Clase,ObjetosNuevos):-
    clase(c(Clase,Super,Props,Rels,Objs)),
    append(Objs,ObjetosNuevos,Objetos),
    construye_clase_term(Clase, Super, Props, Rels, Objs, ViejaClase),
    construye_clase_term(Clase, Super, Props, Rels, Objetos, NuevaClase),
    reemplaza_en_kb(ViejaClase,NuevaClase),
    retract(ViejaClase),
    assertz(NuevaClase),
    !.

/*
* subclases_directas(Clase,Hijas)
* Las subclases a un nivel de Clase con las contenidas en la lista Hijas.
*/
subclases_directas(Clase,Hijas):-
    subclases_directas_(Clase,[],Hijas).

subclases_directas_(Clase,Encontradas,Hijas):-
    clase(c(C,Clase,_,_,_)),
    not(member(C,Encontradas)),
    !,
    subclases_directas_(Clase,[C|Encontradas],Hijas).

subclases_directas_(_,Encontradas,Encontradas).
/*
* elimina_propiedad_de_objeto(Ref,Prop)
* Elimina la propiedad Prop de la lista de propiedades perteneciente al objeto con referencia Ref
*/
elimina_propiedad_de_objeto(Ref,Prop):-
	objeto(o(Ref,Clase,Props,Rels)),
    busca_en_lista(Prop,Props,Encontradas),
    subtract(Props,Encontradas,PropsNuevas),
	construye_objeto_term(Ref,Clase,Props,Rels,Term),
	construye_objeto_term(Ref,Clase,PropsNuevas,Rels,TermNuevo),
	retract(Term),
	assertz(TermNuevo),
	reemplaza_en_kb(Term,TermNuevo),
	!.


/*
* elimina_relacion_de_objeto(Ref,Prop)
* Elimina la relación Rel de la lista de relaciones perteneciente al objeto con referencia Ref
*/
elimina_relacion_de_objeto(Ref,Rel):-
	objeto(o(Ref,Clase,Props,Rels)),
    busca_en_lista(Rel,Rels,Encontradas),
    subtract(Rels,Encontradas,RelsNuevas),
	construye_objeto_term(Ref,Clase,Props,Rels,Term),
	construye_objeto_term(Ref,Clase,Props,RelsNuevas,TermNuevo),
	retract(Term),
	assert(TermNuevo),
	reemplaza_en_kb(Term,TermNuevo),
	!.

/*
* elimina_propiedad_de_clase(Clase,Prop)
* Elimina la propiedad Prop de la lista de propiedades perteneciente a la clase Clase
*/
elimina_propiedad_de_clase(Clase,Prop):-
	clase(c(Clase,Super,Props,Rels,Objetos)),
    busca_en_lista(Prop,Props,Encontradas),
    subtract(Props,Encontradas,PropsNuevas),
	construye_clase_term(Clase,Super,Props,Rels,Objetos,Term),
	construye_clase_term(Clase,Super,PropsNuevas,Rels,Objetos,TermNuevo),
	retract(Term),
	assertz(TermNuevo),
	reemplaza_en_kb(Term,TermNuevo),
	!.

/*
* elimina_propiedad_de_clase(Clase,Rel)
* Elimina la relación Rel de la lista de relaciones perteneciente a la clase Clase
*/
elimina_relacion_de_clase(Clase,Rel):-
	clase(c(Clase,Super,Props,Rels,Objetos)),
    busca_en_lista(Rel,Rels,Encontradas),
    subtract(Rels,Encontradas,RelsNuevas),
	construye_clase_term(Clase,Super,Props,Rels,Objetos,Term),
	construye_clase_term(Clase,Super,Props,RelsNuevas,Objetos,TermNuevo),
	retract(Term),
	assertz(TermNuevo),
	reemplaza_en_kb(Term,TermNuevo),
	!.

/*
* Elimina de todas las relaciones en la kb, a AEliminar
*/
elimina_de_relaciones(AEliminar):-
    extension_de_clase(top,Objetos),
    elimina_de_relaciones_de_objetos(Objetos,AEliminar),
    subclases(top,Clases),
    elimina_de_relaciones_de_clases(Clases,AEliminar).

/*
* elimina_de_relaciones_de_objetos(Objetos,AEliminar)
* Elimina AEliminar de cualquier relación de los objetos de la lista Objetos.
*/

elimina_de_relaciones_de_objetos([Objeto|Objetos],AEliminar):-
    elimina_de_relaciones_de_objeto(Objeto,AEliminar),
    elimina_de_relaciones_de_objetos(Objetos,AEliminar).

elimina_de_relaciones_de_objetos([],_).

/*
* elimina_de_relaciones_de_objetos(Clases,AEliminar)
* Elimina AEliminar de cualquier relación de las clases de la lista Clases.
*/

elimina_de_relaciones_de_clases([Clase|Clases],AEliminar):-
    elimina_de_relaciones_de_clase(Clase,AEliminar),
    elimina_de_relaciones_de_clases(Clases,AEliminar).

elimina_de_relaciones_de_clases([],_).

/*
* elimina_de_relaciones_de_clase(Clase,AEliminar)
* Elimina el valor AEliminar de las relaciones de la clase Clase.
*/

elimina_de_relaciones_de_clase(Clase,AEliminar):-
    clase(c(Clase,Super,Props,Rels,Objetos)),
    elimina_de_relaciones(Rels,AEliminar,NuevasRels),
	construye_clase_term(Clase,Super,Props,Rels,Objetos,Term),
	construye_clase_term(Clase,Super,Props,NuevasRels,Objetos,TermNuevo),
	retract(Term),
	assertz(TermNuevo),
	reemplaza_en_kb(Term,TermNuevo),
    !.

/*
* elimina_de_relaciones_de_objeto(Ref,AEliminar)
* Elimina el valor AEliminar de las relaciones de la clase Clase.
*/

elimina_de_relaciones_de_objeto(Ref,AEliminar):-
    objeto(o(Ref,Clase,Props,Rels)),
    !,
    elimina_de_relaciones(Rels,AEliminar,NuevasRels),
	construye_objeto_term(Ref,Clase,Props,Rels,Term),
	construye_objeto_term(Ref,Clase,Props,NuevasRels,TermNuevo),
	retract(Term),
	assert(TermNuevo),
	reemplaza_en_kb(Term,TermNuevo).

/*
* elimina_de_relaciones(Relaciones,AEliminar,NuevasRelaciones)
* Elimina AEliminar de las relaciones en Relaciones y crea una lista
* con las mismas relaciones sin AEliminar en NuevasRelaciones
*/

elimina_de_relaciones([Rel|Resto],AEliminar,NuevoResto):-
    elimina_de_relacion(Rel,AEliminar,not(_=>[])),
    elimina_de_relaciones(Resto,AEliminar,NuevoResto),
    !.
elimina_de_relaciones([Rel|Resto],AEliminar,NuevoResto):-
    elimina_de_relacion(Rel,AEliminar,_=>[]),
    elimina_de_relaciones(Resto,AEliminar,NuevoResto),
    !.
elimina_de_relaciones([Rel|Resto],AEliminar,[NuevaRel|NuevoResto]):-
    elimina_de_relacion(Rel,AEliminar,NuevaRel),
    elimina_de_relaciones(Resto,AEliminar,NuevoResto),
    !.
elimina_de_relaciones([],_,[]).

/*
* elimina_de_relacion(Atr=>Valores,AEliminar,Atr=>NuevosValores)
* Elimina AEliminar de la relación Atr=>Valores y construye Atr=>NuevosValores.
*/
elimina_de_relacion(not(Atr=>Valores),AEliminar,not(Atr=>NuevosValores)):-
    delete(Valores,AEliminar,NuevosValores),
    !.
elimina_de_relacion(Atr=>Valores,AEliminar,Atr=>NuevosValores):-
    delete(Valores,AEliminar,NuevosValores).
