/*Las clases y objetos se guardan en estructuras
* c(Clase,Super,Props,Rels,Objetos)
* o(Ref,Clase,Props,Rels)
*
* Objetos La lista de objetos de la clase
*
* Ref ID interno
*
* Clase La clase del objeto
*
* Props lista de propiedades
*
* Rels lista de relaciones
*
* Propiedades
*
* propiedad not(propiedad)
* propiedad=>Valor
* propiedad=>Valores
* not(propiedad=>Valores)
*
* Relaciones
*
* OBJ puede ser un objeto o clase
*
* accion=>Objetos not(accion=>Objetos)
*
*/

/*
* clases_de_objeto(KB,Ref,Clases)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Calcula las superclases de la clase a la que pertenece el objeto con referencia Ref y o unifica con Clases.
*/
clases_de_objeto(KB,Ref,Clases):-
    reloadKB(KB),
    clases_de_objeto(Ref,Clases).


/*
* extension_de_clase(KB,Clase,Objetos)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Unifica Objetos con la lista de todas las referencias de los objetos que pertenecen a la clase Clase
*/
extension_de_clase(KB,Clase,Objetos):-
    reloadKB(KB),
    extension_de_clase(Clase,Objetos).

/*
* propiedades_de_objeto(KB,Ref,Propiedades)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Unifica las propiedades del objeto con referencia Ref, con la lista Propiedades
* Se concatena las propiedades del objeto, con las propiedades de su clase y superclases.
* Las propiedades van de las más espeificas a las menos específicas.
*/
propiedades_de_objeto(KB,Ref,Propiedades):-
    reloadKB(KB),
    propiedades_de_objeto(Ref,Propiedades).

/*
* propiedades_de_clase(KB,Clase,Propiedades)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Calcula las propiedades de la clase Clase y as unifica con Propiedades
*/
propiedades_de_clase(KB,Clase,Propiedades):-
    reloadKB(KB),
    propiedades_de_clase(Clase,Propiedades).

/*
* relaciones_de_objeto(KB,Ref,Relaciones)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Unifica las relaciones del objeto con referencia Ref, con la lista Relaciones
* Las relaciones van de las más específicas a las menos específicas.
*/
relaciones_de_objeto(KB,Ref,Relaciones):-    
    reloadKB(KB),
    relaciones_de_objeto(Ref,Relaciones).

/*
* relaciones_de_clase(KB,Clase,Relaciones)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Unifica las relaciones de la clase Clase, con la lista Relaciones
* Las relaciones van de las más específicas a las menos específicas.
*/
relaciones_de_clase(KB,Clase,Relaciones):-
    reloadKB(KB),
    relaciones_de_clase(Clase,Relaciones).


/*
* extension_de_relacion(KB,Rel,ObjetosRefVal)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* ObjetosRefVal es la lista de referencias a objetos que tienen la relacion Rel con su respectivo(s) valor(es)
*/
extension_de_relacion(KB,Rel,ObjetosRefVal):-
    reloadKB(KB),
    extension_de_relacion(Rel,ObjetosRefVal).

/*
* extension_de_propiedad(Prop,ObjetosRef)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* ObjetosRefVal es la lista de referencias a objetos que tienen la propiedad Prop con su respectivo(s) valor(es)
*/

extension_de_propiedad(KB,Prop,ObjetosRefVal):-    
    reloadKB(KB),
    extension_de_propiedad(Prop,ObjetosRefVal).



