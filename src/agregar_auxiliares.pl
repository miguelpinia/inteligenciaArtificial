/*
* agrega_clase(Clase, Superclase, Propiedades, Relaciones)
* Agrega una nueva clase Clase con superclase Superclase propieades Propiedades y relaciones Relaciones
* Falla si no existe la superclase o si ya existe la clase
*/
agrega_clase(Clase, Superclase, Propiedades, Relaciones):-
    clase(c(Superclase,_,_,_,_)),
    not(clase(c(Clase,_,_,_,_))),
    !,
	agrega_a_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, []))),
    assertz(clase(c(Clase, Superclase, Propiedades, Relaciones, []))).

/*
* agrega_objeto_sin_id(Clase, Props, Rels)
* Agrega un objeto nuevo en la clase Clase con propiedades Props y relaciones Rels
* Crea un id numérico 
* Falla en caso de no existir la clase Clase.
*/
agrega_objeto_sin_id(Clase, Props, Rels):-    
	clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos)),
	refs_a_lista(Refs),
    ids_numericos(Refs,RefsNumericos),
	maximoid([-1|RefsNumericos],Y),
	Z is Y+1,
	agrega_a_kb(objeto(o(Z, Clase, Props, Rels))),
    assertz(objeto(o(Z, Clase, Props, Rels))),
	elimina_de_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
    retract(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
	agrega_a_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, [Z|Objetos]))),
    assertz(clase(c(Clase, Superclase, Propiedades, Relaciones, [Z|Objetos]))), 
    !.

/*
* 
*/

agrega_objeto_con_id(Ref,Clase, Props, Rels):-    
	clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos)),
	agrega_a_kb(objeto(o(Ref, Clase, Props, Rels))),
    assertz(objeto(o(Ref, Clase, Props, Rels))),
	elimina_de_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
    retract(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
	agrega_a_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, [Ref|Objetos]))),
    assertz(clase(c(Clase, Superclase, Propiedades, Relaciones, [Ref|Objetos]))), 
    !.

/*Debe estar comentada la linea de cut ! del reloadkb
Guarda TODOS los IDs en una lista */	
refs_a_lista(L):-
	findall(X,(objeto(o(X, _, _, _))),L).

/* Busca el id más grande de la lista */
maximoid([X|Xs],M):-
	list_max2(M, X, Xs).
list_max2(M, M, []):- !.
list_max2(X, Y, [Z|Zs]):-
	Z >= Y,
    !,
    list_max2(X, Z, Zs).
list_max2(X, Y, [Z|Zs]):-
    Z =< Y,
    list_max2(X, Y, Zs).

/*
* Elimina las inconsistencias
* Eliminar AEliminar de Original da como resutado Resultado
* AEliminar,Original pueden ser listas o no.
*/
elimina_inconsistencia(AEliminar,Original,Resultado):-
    is_list(AEliminar),
    is_list(Original),
    subtract(Original,AEliminar,Resultado),
    Resultado \= [],
    !.

elimina_inconsistencia(AEliminar,Original,Resultado):-
    is_list(AEliminar),
    is_list(Original),
    subtract(Original,AEliminar,Resultado),
    Resultado = [],
    !,
    fail.

elimina_inconsistencia(AEliminar,Original,Original):-
    not(is_list(AEliminar)),
    not(is_list(Original)),
    AEliminar \= Original,
    !.

elimina_inconsistencia(AEliminar,Original,Original):-
    is_list(AEliminar),
    not(member(Original,AEliminar)),
    !.

elimina_inconsistencia(AEliminar,Original,Resultado):-
    delete(Original,AEliminar,Resultado),
    Resultado \= [].

/*
* agrega_relacion_a_clase(Relacion, Clase)
* Agrega la relación Relacion a la clase Clase solo en caso de no existir en la KB.
* Falla si ya está definida, en este caso se debe usar modifica.
*/
agrega_relacion_a_clase(not(Nombre => Valores), Clase):-
	clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos)), 
    clasifica(Relaciones,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,Nombre,Respuesta2,ValoresAEliminar),
    Respuesta2 = 'Si',
    !,
    elimina_inconsistencia(ValoresAEliminar,Valores,Consistentes),
	elimina_de_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
    retract(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
	agrega_a_kb(clase(c(Clase, Superclase, Propiedades, [not(Nombre => Consistentes)|Relaciones], Objetos))),
    assertz(clase(c(Clase, Superclase, Propiedades, [not(Nombre => Consistentes)|Relaciones], Objetos))),
    !.


agrega_relacion_a_clase(not(Nombre => _), Clase):-
	clase(c(Clase, _, _, Relaciones, _)), 
    clasifica(Relaciones,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,Nombre,Respuesta2,_),
    Respuesta2 = 'Si',
    !,
    fail.

agrega_relacion_a_clase(not(Nombre => Valores), Clase):-
	clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos)), 
    clasifica(Relaciones,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    !,
	elimina_de_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
    retract(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
	agrega_a_kb(clase(c(Clase, Superclase, Propiedades, [not(Nombre => Valores)|Relaciones], Objetos))),
    assertz(clase(c(Clase, Superclase, Propiedades, [not(Nombre => Valores)|Relaciones], Objetos))),
    !.

agrega_relacion_a_clase(Nombre => Valores, Clase):-
	clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos)), 
    clasifica(Relaciones,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,Nombre,Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,not(Nombre),Respuesta2,ValoresAEliminar),
    Respuesta2 = 'Si',    
    elimina_inconsistencia(ValoresAEliminar,Valores,Consistentes),
    !,
	elimina_de_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
    retract(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
	agrega_a_kb(clase(c(Clase, Superclase, Propiedades, [Nombre => Consistentes|Relaciones], Objetos))),
    assertz(clase(c(Clase, Superclase, Propiedades, [Nombre => Consistentes|Relaciones], Objetos))),
    !.


agrega_relacion_a_clase(Nombre => _, Clase):-
	clase(c(Clase, _, _, Relaciones, _)), 
    clasifica(Relaciones,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,Nombre,Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,not(Nombre),Respuesta2,_),
    Respuesta2 = 'Si',
    !,
    fail.

agrega_relacion_a_clase(Nombre => Valores, Clase):-
	clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos)), 
    clasifica(Relaciones,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,Nombre,Respuesta,_),
    Respuesta = 'No se',
    !,
	elimina_de_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
    retract(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
	agrega_a_kb(clase(c(Clase, Superclase, Propiedades, [Nombre => Valores|Relaciones], Objetos))),
    assertz(clase(c(Clase, Superclase, Propiedades, [Nombre => Valores|Relaciones], Objetos))),
    !.

/*
* agrega_relacion_a_objeto(Relacion, Ref)
* Agrega la relación Relación al objeto con referencia Ref.
*/	

agrega_relacion_a_objeto(not(Nombre => Valores), Ref):-
	objeto(o(Ref, Clase, Propiedades, Relaciones)),
    clasifica(Relaciones,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,Nombre,Respuesta2,ValoresAEliminar),
    Respuesta2 = 'Si',
    elimina_inconsistencia(ValoresAEliminar,Valores,Consistentes),
    !,
	elimina_de_kb(objeto(o(Ref, Clase, Propiedades, Relaciones))),
    retract(objeto(o(Ref, Clase, Propiedades, Relaciones))),
	agrega_a_kb(objeto(o(Ref, Clase, Propiedades, [not(Nombre => Consistentes)|Relaciones]))),
    assertz(objeto(o(Ref, Clase, Propiedades, [not(Nombre => Consistentes)|Relaciones]))),
    !.

agrega_relacion_a_objeto(not(Nombre => _), Ref):-
	objeto(o(Ref, _, _, Relaciones)),
    clasifica(Relaciones,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,Nombre,Respuesta2,_),
    Respuesta2 = 'Si',
    !,
    fail.

agrega_relacion_a_objeto(not(Nombre => Valores), Ref):-
	objeto(o(Ref, Clase, Propiedades, Relaciones)),
    clasifica(Relaciones,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    !,
	elimina_de_kb(objeto(o(Ref, Clase, Propiedades, Relaciones))),
    retract(objeto(o(Ref, Clase, Propiedades, Relaciones))),
	agrega_a_kb(objeto(o(Ref, Clase, Propiedades, [not(Nombre => Valores)|Relaciones]))),
    assertz(objeto(o(Ref, Clase, Propiedades, [not(Nombre => Valores)|Relaciones]))),
    !.

agrega_relacion_a_objeto(Nombre => Valores, Ref):-
	objeto(o(Ref, Clase, Propiedades, Relaciones)),
    clasifica(Relaciones,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,Nombre,Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,not(Nombre),Respuesta2,ValoresAEliminar),
    Respuesta2 = 'Si',
    elimina_inconsistencia(ValoresAEliminar,Valores,Consistentes),
    !,
	elimina_de_kb(objeto(o(Ref, Clase, Propiedades, Relaciones))),
    retract(objeto(o(Ref, Clase, Propiedades, Relaciones))),
	agrega_a_kb(objeto(o(Ref, Clase, Propiedades, [Nombre => Consistentes|Relaciones]))),
    assertz(objeto(o(Ref, Clase, Propiedades, [Nombre => Consistentes|Relaciones]))),
    !.

agrega_relacion_a_objeto(Nombre => _, Ref):-
	objeto(o(Ref, _, _, Relaciones)),
    clasifica(Relaciones,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,Nombre,Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,not(Nombre),Respuesta2,_),
    Respuesta2 = 'Si',
    !,
    fail.

agrega_relacion_a_objeto(Nombre => Valores, Ref):-
	objeto(o(Ref, Clase, Propiedades, Relaciones)),
    clasifica(Relaciones,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,Nombre,Respuesta,_),
    Respuesta = 'No se',
    !,
	elimina_de_kb(objeto(o(Ref, Clase, Propiedades, Relaciones))),
    retract(objeto(o(Ref, Clase, Propiedades, Relaciones))),
	agrega_a_kb(objeto(o(Ref, Clase, Propiedades, [Nombre => Valores|Relaciones]))),
    assertz(objeto(o(Ref, Clase, Propiedades, [Nombre => Valores|Relaciones]))),
    !.
	
/*
* agrega_propiedad_a_clase(Propiedad, Clase)
* Agrega la propiedad Propiedad a a clase clase.
*/
agrega_propiedad_a_clase(not(Nombre => Valores), Clase):-
	clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,Nombre,Respuesta2,ValoresAEliminar),
    Respuesta2 = 'Si',
    elimina_inconsistencia(ValoresAEliminar,Valores,Consistentes),
    !,
	elimina_de_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
    retract(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
	agrega_a_kb(clase(c(Clase, Superclase,[not(Nombre => Consistentes)|Propiedades] , Relaciones, Objetos))),
    assertz(clase(c(Clase, Superclase,[not(Nombre => Consistentes)|Propiedades] , Relaciones, Objetos))),
    !.

agrega_propiedad_a_clase(not(Nombre => _), Clase):-
	clase(c(Clase, _, Propiedades, _, _)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,Nombre,Respuesta2,_),
    Respuesta2 = 'Si',
    !,
    fail.


agrega_propiedad_a_clase(not(Nombre => Valores), Clase):-
	clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    !,
	elimina_de_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
    retract(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
	agrega_a_kb(clase(c(Clase, Superclase,[not(Nombre => Valores)|Propiedades] , Relaciones, Objetos))),
    assertz(clase(c(Clase, Superclase,[not(Nombre => Valores)|Propiedades] , Relaciones, Objetos))),
    !.


agrega_propiedad_a_clase(Nombre => Valores, Clase):-
	clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,Nombre,Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,not(Nombre),Respuesta2,ValoresAEliminar),
    Respuesta2 = 'Si',
    elimina_inconsistencia(ValoresAEliminar,Valores,Consistentes),
    !,
	elimina_de_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
    retract(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
	agrega_a_kb(clase(c(Clase, Superclase,[Nombre => Consistentes|Propiedades] , Relaciones, Objetos))),
    assertz(clase(c(Clase, Superclase,[Nombre => Consistentes|Propiedades] , Relaciones, Objetos))),
    !.

agrega_propiedad_a_clase(Nombre => _, Clase):-
	clase(c(Clase, _, Propiedades, _, _)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,Nombre,Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,not(Nombre),Respuesta2,_),
    Respuesta2 = 'Si',
    !,
    fail.


agrega_propiedad_a_clase(Nombre => Valores, Clase):-
	clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    !,
	elimina_de_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
    retract(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
	agrega_a_kb(clase(c(Clase, Superclase,[Nombre => Valores|Propiedades] , Relaciones, Objetos))),
    assertz(clase(c(Clase, Superclase,[Nombre => Valores|Propiedades] , Relaciones, Objetos))),
    !.

/* Agrega PROPIEDAD de un solo un atributo */
agrega_propiedad_a_clase(not(Nombre), Clase):-
	clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    !,
	elimina_de_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
    retract(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
	agrega_a_kb(clase(c(Clase, Superclase, [not(Nombre)|Propiedades], Relaciones, Objetos))),
    assertz(clase(c(Clase, Superclase, [not(Nombre)|Propiedades], Relaciones, Objetos))),
    !.


agrega_propiedad_a_clase(not(Nombre), Clase):-
	clase(c(Clase, _, Propiedades, _, _)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta \= 'No se',
    !,
    fail.

agrega_propiedad_a_clase(Nombre, Clase):-
	clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,Nombre,Respuesta,_),
    Respuesta = 'No se',
    !,
	elimina_de_kb(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
    retract(clase(c(Clase, Superclase, Propiedades, Relaciones, Objetos))),
	agrega_a_kb(clase(c(Clase, Superclase, [Nombre|Propiedades], Relaciones, Objetos))),
    assertz(clase(c(Clase, Superclase, [Nombre|Propiedades], Relaciones, Objetos))),
    !.

/*
* agrega_propiedad_a_objeto(Propiedad, Ref)
* Agrega la propiedad Propiedad en el objeto con referencia Ref
*/

agrega_propiedad_a_objeto(not(Nombre => Valores), Ref):-
	objeto(o(Ref, Clase, Propiedades, Relaciones)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,Nombre,Respuesta2,ValoresAEliminar),
    Respuesta2 = 'Si',
    elimina_inconsistencia(ValoresAEliminar,Valores,Consistentes),
    !,
	elimina_de_kb(objeto(o(Ref,Clase, Propiedades, Relaciones))),
    retract(objeto(o(Ref,Clase, Propiedades, Relaciones))),
	agrega_a_kb(objeto(o(Ref,Clase, [not(Nombre => Consistentes)|Propiedades] , Relaciones))),
    assertz(objeto(o(Ref,Clase,[not(Nombre => Consistentes)|Propiedades] , Relaciones))),
    !.

agrega_propiedad_a_objeto(not(Nombre => _), Ref):-
	objeto(o(Ref, _, Propiedades, _)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,Nombre,Respuesta2,_),
    Respuesta2 = 'Si',
    !,
    fail.

agrega_propiedad_a_objeto(not(Nombre => Valores), Ref):-
	objeto(o(Ref, Clase, Propiedades, Relaciones)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
	elimina_de_kb(objeto(o(Ref, Clase, Propiedades, Relaciones))),
    retract(objeto(o(Ref, Clase, Propiedades, Relaciones))),
	agrega_a_kb(objeto(o(Ref, Clase,[not(Nombre => Valores)|Propiedades] , Relaciones))),
    assertz(objeto(o(Ref, Clase,[not(Nombre => Valores)|Propiedades] , Relaciones))),
    !.


agrega_propiedad_a_objeto(Nombre => Valores, Ref):-
	objeto(o(Ref, Clase, Propiedades, Relaciones)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,Nombre,Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,not(Nombre),Respuesta2,ValoresAEliminar),
    Respuesta2 = 'Si',
    elimina_inconsistencia(ValoresAEliminar,Valores,Consistentes),
    !,
	elimina_de_kb(objeto(o(Ref,Clase, Propiedades, Relaciones))),
    retract(objeto(o(Ref,Clase, Propiedades, Relaciones))),
	agrega_a_kb(objeto(o(Ref,Clase, [Nombre => Consistentes|Propiedades] , Relaciones))),
    assertz(objeto(o(Ref,Clase, [Nombre => Consistentes|Propiedades] , Relaciones))),
    !.

agrega_propiedad_a_objeto(Nombre => _,  Ref):-
	objeto(o(Ref, _, Propiedades, _)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,Nombre,Respuesta,_),
    Respuesta = 'No se',
    tiene(Positivas,Negativas,not(Nombre),Respuesta2,_),
    Respuesta2 = 'Si',
    !,
    fail.


agrega_propiedad_a_objeto(Nombre => Valores,  Ref):-
	objeto(o(Ref, Clase, Propiedades, Relaciones)),  
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    !,
	elimina_de_kb(objeto(o(Ref, Clase, Propiedades, Relaciones))),
    retract(objeto(o(Ref, Clase,Propiedades, Relaciones))),
	agrega_a_kb(objeto(o(Ref, Clase,[Nombre => Valores|Propiedades] , Relaciones))),
    assertz(objeto(o(Ref, Clase,[Nombre => Valores|Propiedades] , Relaciones))),
    !.

/* Agrega PROPIEDAD de un solo un atributo */
agrega_propiedad_a_objeto(not(Nombre),  Ref):-
	objeto(o(Ref, Clase, Propiedades, Relaciones)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta = 'No se',
    !,
	elimina_de_kb(objeto(o(Ref, Clase, Propiedades, Relaciones))),
    retract(objeto(o(Ref, Clase, Propiedades, Relaciones))),
	agrega_a_kb(objeto(o(Ref, Clase, [not(Nombre)|Propiedades], Relaciones))),
    assertz(objeto(o(Ref, Clase, [not(Nombre)|Propiedades], Relaciones))),
    !.


agrega_propiedad_a_objeto(not(Nombre), Ref):-
	objeto(o(Ref, _, Propiedades, _)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,not(Nombre),Respuesta,_),
    Respuesta \= 'No se',
    !,
    fail.

agrega_propiedad_a_objeto(Nombre,  Ref):-
	objeto(o(Ref, Clase, Propiedades, Relaciones)), 
    clasifica(Propiedades,[],[],Positivas,Negativas),
    tiene(Positivas,Negativas,Nombre,Respuesta,_),
    Respuesta = 'No se',
    !,
	elimina_de_kb(objeto(o(Ref, Clase, Propiedades, Relaciones))),
    retract(objeto(o(Ref, Clase, Propiedades, Relaciones))),
	agrega_a_kb(objeto(o(Ref, Clase, [Nombre|Propiedades], Relaciones))),
    assertz(objeto(o(Ref, Clase, [Nombre|Propiedades], Relaciones))),
    !.
