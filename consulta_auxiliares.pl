/*
* subclases(Clase,Subclases)
* Calcula las subclases de una clase 
*/
subclases(Clase,Subclases):-
	subclases_([Clase],Subclases).


/*
* subclases_(Encontradas,Subclases)
* Calcula las subclases de una lista de clases 
*/
subclases_(Encontradas,Subclases):-
    clase(c(Subclase,Clase,_,_,_)),
    member(Clase,Encontradas),
    not(member(Subclase,Encontradas)),
    !,
    subclases_([Subclase|Encontradas],Subclases).

/* Cuando no encuentra nuevas solo regresa la misma. */
subclases_(X,X).

/*
* superclases(Clase,Superclases)
* Calcula las superclases de la clase Clase y las unifica con Superclases 
*/
superclases(top,[top]):-!.
superclases(Clase,[Clase|Superclases]):-
    clase(c(Clase,Superclase,_,_,_)),
    !,
    superclases(Superclase,Superclases).

/*
* clases_de_objeto(Ref,Clases)
* Calcula las superclases de la clase a la que pertenece el objeto con referencia Ref y o unifica con Clases.
*/
clases_de_objeto(Ref,Clases):-
    objeto(o(Ref,Clase,_,_)),
    !,
    superclases(Clase,Clases).

/*
* extension_de_clase(Clase,Objetos)
* Unifica Objetos con la lista de todas las referencias de los objetos que pertenecen a la clase Clase
*/
extension_de_clase(Clase,Objetos):-
    subclases(Clase,Clases),
    extension_clase_(Clases,Objetos).

/*
* extension_clase_(Clases,ObjetosRef)
* Predicado auxiliar que concatena las referencias a objetos pertenecientes directamente a las clases en la lista Clases
* la lista resultado de la concatenación se unifica con Objetos
*/
extension_clase_([],[]):-
    !.
extension_clase_([top|OtrasClases],ObjetosRef):-
    !,
    extension_clase_(OtrasClases,ObjetosRef).
extension_clase_([Clase|OtrasClasesRef],ObjetosRef):-
    clase(c(Clase,_,_,_,ObjetosDeClaseRef)),
    !,
    extension_clase_(OtrasClasesRef,ObjetosDeOtrasClasesRef),
    append(ObjetosDeClaseRef,ObjetosDeOtrasClasesRef,ObjetosRef).

/*
* propiedades_de_objeto(Ref,Propiedades)
* Unifica las propiedades del objeto con referencia Ref, con la lista Propiedades
* Se concatena las propiedades del objeto, con las propiedades de su clase y superclases.
* Las propiedades van de las más espeificas a las menos específicas.
*/
propiedades_de_objeto(Ref,Propiedades):-
    objeto(o(Ref,Clase,PropiedadesDeObjeto,_)),
    !,
    propiedades_de_clase(Clase,PropiedadesDeClase),
    append(PropiedadesDeObjeto,PropiedadesDeClase,Propiedades).

/*
* propiedades_de_clase(Clase,Propiedades)
* Unifica las propiedades de la clase Clase, con la lista Propiedades
* Se concatena las propiedades dela clase, con las propiedades de sus superclases.
* Las propiedades van de las más específicas a las menos específica.
*/
propiedades_de_clase(top,[]):-!.
propiedades_de_clase(Clase,Propiedades):-
    clase(c(Clase,Superclase,PropiedadesDeClase,_,_)),
    !,
    propiedades_de_clase(Superclase,PropiedadesDeSuperclase),
    append(PropiedadesDeClase,PropiedadesDeSuperclase,Propiedades).

/*
* relaciones_de_objeto(Ref,Relaciones)
* Unifica las relaciones del objeto con referencia Ref, con la lista Relaciones
* Se concatenan las relaciones del objeto, con las relaciones de su clase y superclases.
* Las relaciones van de las más espeificas a las menos específicas.
*/
relaciones_de_objeto(Ref,Relaciones):-
    objeto(o(Ref,Clase,_,RelacionesDeObjeto)),
    !,
    relaciones_de_clase(Clase,RelacionesDeClase),
    append(RelacionesDeObjeto,RelacionesDeClase,Relaciones).


/*
* relaciones_de_clase(Clase,Relaciones)
* Unifica las relaciones de la clase Clase, con la lista Relaciones
* Las relaciones van de las más espeificas a las menos específicas.
*/
relaciones_de_clase(top,[]):-!.
relaciones_de_clase(Clase,Relaciones):-
    clase(c(Clase,Superclase,_,RelacionesDeClase,_)),
    !,
    relaciones_de_clase(Superclase,RelacionesDeSuperclase),
    append(RelacionesDeClase,RelacionesDeSuperclase,Relaciones).

/*
* extension_de_relacion(Rel,ObjetosRefVal)
* ObjetosRefVal es la lista de referencias a objetos que tienen la propiedad Prop con su respectivo(s) valor(es)
*/
extension_de_relacion(Rel,ObjetosRef):-
    extension_relacion_(Rel,[],ObjetosRef).

/*
* Predicado auxiliar para calcular la extension de una relacion
* ObjetosRef es la lista de referencias a objetos que tienen la relación Rel
* Encontrados es la lista de objetos encontrados que cumplen con tener la relación
*/
extension_relacion_(Rel,Encontrados,ObjetosRefVal):-
    objeto(o(Ref,_,_,_)),
    not(member(Ref=>_,Encontrados)),
    objeto_tiene_relacion(Ref,Rel,Respuesta,Valor),
    Respuesta = 'Si',
    !,
    extension_relacion_(Rel,[Ref=>Valor|Encontrados],ObjetosRefVal).

extension_relacion_(_,X,X).

/*
* extension_propiedad(Prop,ObjetosRef)
* ObjetosRefVal es la lista de referencias a objetos que tienen la propiedad Prop con su respectivo(s) valor(es)
*/

extension_de_propiedad(Prop,ObjetosRefVal):-
    extension_propiedad_(Prop,[],ObjetosRefVal).

/*
* extension_propiedad_(Prop,Encontrados,ObjetosRef)
* ObjetosRef es la lista de referencias a objetos que tienen la propiedad Prop
* Encontrados es una lista que va guardando los objetos encontrados.
*/
extension_propiedad_(Prop,Encontrados,ObjetosRefVal):-
    objeto(o(Ref,_,_,_)),
    not(member(Ref=>_,Encontrados)),
    objeto_tiene_propiedad(Ref,Prop,Respuesta,Valor),
    Respuesta = 'Si',
    !,
    extension_propiedad_(Prop,[Ref=>Valor|Encontrados],ObjetosRefVal).

extension_propiedad_(_,X,X).
