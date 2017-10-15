/*
* Módulo que provee predicados para modificar:
*
* a) El nombre de una clase u objeto.
* b) El valor de una propiedad específica de una clase u objeto
* c) Con quien mantiene una relación específica con una clase u objeto.
*/

/*
* modifica_nombre_clase_de_objeto_ref(NuevaClase, Referencia):
*
* Dada la referencia Referencia de un objeto, cambia el valor de la
* clase asociada por el de la clase NuevaClase.
*
* Modo de uso:
*
* ?- modifica_nombre_de_clase_a_objeto(top, 7).
* true.
*
* ?- objeto(X).
* ...;
* X = o(7, top, [color=>blanco], []).
*/
modifica_nombre_de_clase_a_objeto(NuevaClase, Referencia) :-
    ref_to_obj(Referencia, o(Referencia, ViejaClase, Propiedades, Relaciones)),
    construye_objeto_term(Referencia, NuevaClase, Propiedades, Relaciones, NuevoTermino),
    construye_objeto_term(Referencia, ViejaClase, Propiedades, Relaciones, ViejoTermino),
    mueve_objeto_de_clase(ViejaClase, Referencia, NuevaClase),
    retract(ViejoTermino),
    assertz(NuevoTermino),
    reemplaza_en_kb(ViejoTermino, NuevoTermino),
    !.

mueve_objeto_de_clase(ViejaClase, Referencia, NuevaClase) :-
    clase(c(ViejaClase, ViejaSuper, ViejaProps, ViejaRels, ViejaObjs)),
    clase(c(NuevaClase, NuevaSuper, NuevaProps, NuevaRels, NuevaObjs)),
    delete(ViejaObjs, Referencia, ModViejaObjs),
    append(NuevaObjs, [Referencia], ModNuevaObjs),
    construye_clase_term(ViejaClase, ViejaSuper, ViejaProps, ViejaRels, ViejaObjs, ViejoTerm),
    construye_clase_term(ViejaClase, ViejaSuper, ViejaProps, ViejaRels, ModViejaObjs, ModViejoTerm),
    construye_clase_term(NuevaClase, NuevaSuper, NuevaProps, NuevaRels, NuevaObjs, NuevoTerm),
    construye_clase_term(NuevaClase, NuevaSuper, NuevaProps, NuevaRels, ModNuevaObjs, ModNuevoTerm),
    retract(ViejoTerm),
    assertz(ModViejoTerm),
    retract(NuevoTerm),
    assertz(ModNuevoTerm),
    reemplaza_en_kb(ViejoTerm, ModViejoTerm),
    reemplaza_en_kb(NuevoTerm, ModNuevoTerm),
    !.

/*
* modifica_nombre_de_clase_a_objetos(NuevaClase, ListaReferencias):
*
* Modifica el nombre de la clase a los objetos asociados a cada una de
* las referencias de ListaReferencias.
*
* Modo de uso:
*
* ?- modifica_nombre_de_clase_a_objetos(girasol, [5,6,7]).
* true.
*
* ?- objeto(X).
* ...;
* X = o(5, girasol, [color=>rojo], []) ;
* X = o(6, girasol, [], []) ;
* X = o(7, girasol, [color=>blanco], []).
*/
modifica_nombre_de_clase_a_objetos(_, []) :- !.

modifica_nombre_de_clase_a_objetos(NuevaClase, [Referencia|ListaReferencias]) :-
    modifica_nombre_de_clase_a_objeto(NuevaClase, Referencia),
    modifica_nombre_de_clase_a_objetos(NuevaClase, ListaReferencias).

/*
* modifica_nombre_de_superclase_a_clase(NuevaSuperClase, Clase):
*
* Dada la clase Clase, modifica el valor de la superclase de la clase
* por NuevaSuperClase.
*
* Modo de uso:
*
* ?- modifica_nombre_de_superclase_a_clase(top, rosa).
* true.
*
* ?- clase(X).
* X = c(animal, top, [movil], [], []) ;
* X = c(perro, animal, [canino], [odia=>[gato]], [1, 2]) ;
* X = c(gato, animal, [felino], [huye=>[perro]], []) ;
* X = c(vegetal, top, [not(movil), not(color=>negro)], [], []) ;
* X = c(girasol, vegetal, [movil, color=>amarillo], [], [3, 4]) ;
* X = c(rosa, top, [bonita, color=>udf], [], [5, 6, 7]).
*/
modifica_nombre_de_superclase_a_clase(NuevaSuperClase, Clase) :-
    clase(c(Clase, SuperClase, Propiedades, Relaciones, Objetos)),
    construye_clase_term(Clase, NuevaSuperClase, Propiedades, Relaciones, Objetos, NuevaClase),
    construye_clase_term(Clase, SuperClase, Propiedades, Relaciones, Objetos, ViejaClase),
    retract(ViejaClase),
    assertz(NuevaClase),
    agrega_a_kb(NuevaClase),
    elimina_de_kb(ViejaClase),
    !.

/*
* modifica_nombre_de_superclase_a_clases(NuevaSuperClase, ListaClase):
*
* Dada la lista de nombre de clases ListaClases, modifica la
* superclase en la clase de cada una de ellas por NuevaSuperClase.
*
* Modo de uso:
*
* ?- modifica_nombre_de_superclase_a_clases(animal, [vegetal, girasol, rosa]).
* true.
*
* ?- clase(X).
* X = c(animal, top, [movil], [], []) ;
* X = c(perro, animal, [canino], [odia=>[gato]], [1, 2]) ;
* X = c(gato, animal, [felino], [huye=>[perro]], []) ;
* X = c(vegetal, animal, [not(movil), not(color=>negro)], [], []) ;
* X = c(girasol, animal, [movil, color=>amarillo], [], [3, 4]) ;
* X = c(rosa, animal, [bonita, color=>udf], [], [5, 6, 7]).
*/
modifica_nombre_de_superclase_a_clases(_, []) :- !.

modifica_nombre_de_superclase_a_clases(NuevaSuperClase, [Clase|ListaClases]) :-
    modifica_nombre_de_superclase_a_clase(NuevaSuperClase, Clase),
    modifica_nombre_de_superclase_a_clases(NuevaSuperClase, ListaClases).

/*
* hijos(Clase, Hijos):
  Calcula los hijos directos de la clase Clase.
*
* Modo de uso:
*
* ?- hijos(animal, X).
* X = [gato, perro].
*/
hijos(Clase, Hijos) :- hijos_(Clase, [], Hijos).

hijos_(Clase, Encontrados, Hijos) :-
    clase(c(Subclase, Clase, _, _, _)),
    not(member(Subclase, Encontrados)),
    !,
    hijos_(Clase, [Subclase|Encontrados], Hijos).

hijos_(_, X, X) :- !.

modifica_nombre_de_clase_a_objeto_no_actualiza(NuevaClase, Referencia) :-
    ref_to_obj(Referencia, o(Referencia, ViejaClase, Propiedades, Relaciones)),
    construye_objeto_term(Referencia, NuevaClase, Propiedades, Relaciones, NuevoTermino),
    construye_objeto_term(Referencia, ViejaClase, Propiedades, Relaciones, ViejoTermino),
    retract(ViejoTermino),
    assertz(NuevoTermino),
    reemplaza_en_kb(ViejoTermino, NuevoTermino),
    !.
modifica_nombre_de_clase_a_objetos_no_actualiza(_, []) :- !.
modifica_nombre_de_clase_a_objetos_no_actualiza(NuevaClase, [Referencia|ListaReferencias]) :-
    modifica_nombre_de_clase_a_objeto_no_actualiza(NuevaClase, Referencia),
    modifica_nombre_de_clase_a_objetos_no_actualiza(NuevaClase, ListaReferencias).


/*
* modifica_nombre_de_clase(NuevaClase, Clase):
*
* Modifica el nombre de la clase Clase por NuevaClase, afectando a
* todos los descendientes directos de la clase, así como a todos los
* objetos declarados directamente de la clase Clase.
*
* Modo de uso:
*
* % Revisa el estado de las relaciones de clase, en particular para gato
* % en clases y objetos.
* ?- listing(clase).
* :- dynamic clase/1.
*
* ... .
* clase(c(perro, animal, [canino], [odia=>[gato]], [1, 2])).
* clase(c(gato, animal, [felino], [huye=>[perro]], [])).
* ... .
*
* true.
*
* ?- listing(objeto).
* :- dynamic objeto/1.
*
* ... .
* objeto(o(2, perro, [not(agresivo), alias=>[fido, bueno]], [not(odia=>[gato]), odia=>[ardilla]])).
* ... .
*
* true.
* % Cambia el nombre de la case gato por catdog.
* ?- modifica_nombre_de_clase(catdog, gato).
* true.
*
* ?- listing(objeto).
* :- dynamic objeto/1.
*
* ... .
* objeto(o(2, perro, [not(agresivo), alias=>[fido, bueno]], [odia=>[catdog], odia=>[ardilla]])).
*
* true.
*
* ?- listing(clase).
* :- dynamic clase/1.
*
* ... .
* clase(c(perro, animal, [canino], [odia=>[catdog]], [1, 2])).
* clase(c(catdog, animal, [felino], [huye=>[perro]], [])).
* ... .
*
* true.
*/
modifica_nombre_de_clase(NuevaClase, Clase) :-
    clase(c(Clase, SuperClase, Propiedades, Relaciones, Objetos)),
    reemplaza_de_relaciones(Clase, NuevaClase),
    construye_clase_term(NuevaClase, SuperClase, Propiedades, Relaciones, Objetos, NuevoTermino),
    construye_clase_term(Clase, SuperClase, Propiedades, Relaciones, Objetos, ViejoTermino),
    modifica_nombre_de_clase_a_objetos_no_actualiza(NuevaClase, Objetos),
    hijos(Clase, Hijos),
    modifica_nombre_de_superclase_a_clases(NuevaClase, Hijos),
    retract(ViejoTermino),
    assertz(NuevoTermino),
    agrega_a_kb(NuevoTermino),
    elimina_de_kb(ViejoTermino),
    !.

/*
* modifica_id_de_objeto(Referencia, NuevaReferencia)
*
* Sustituye el id Referencia del objeto por NuevaReferencia. Esto sólo
* lo realiza si no se encuentra declarado previamente la
* NuevaReferencia como extensión de la clase top.
*
* Modo de uso:
* ?- listing(clase).
* :- dynamic clase/1.
*
* ... .
* clase(c(perro, animal, [canino], [odia=>[gato]], [1, 2])).
* ... .
*
* true.
*
* ?- listing(objeto).
* :- dynamic objeto/1.
*
* ... .
* objeto(o(2, perro, [not(agresivo), alias=>[fido, bueno]], [not(odia=>[gato]), odia=>[ardilla]])).
* ... .
*
* true.
*
* ?- modifica_id_de_objeto(2, 10).
* true.
*
* ?- listing(clase).
* :- dynamic clase/1.
*
* ....
* clase(c(perro, animal, [canino], [odia=>[gato]], [1, 10])).
*
* true.
*
* ?- listing(objeto).
* :- dynamic objeto/1.
*
* ... .
* objeto(o(1, perro, [agresivo, alias=>[diablo]], [muerde=>[10]])).
* objeto(o(10, perro, [not(agresivo), alias=>[fido, bueno]], [not(odia=>[gato]), odia=>[ardilla]])).
*
* true.
*
*/
modifica_id_de_objeto(Referencia, NuevaReferencia) :-
    extension_de_clase(top, Objetos),
    not(member(NuevaReferencia, Objetos)),
    ref_to_obj(Referencia, o(Referencia, Clase, Propiedades, Relaciones)),
    construye_objeto_term(NuevaReferencia, Clase, Propiedades, Relaciones, NuevoTermino),
    construye_objeto_term(Referencia, Clase, Propiedades, Relaciones, ViejoTermino),
    reemplaza_de_relaciones(Referencia, NuevaReferencia),
    reemplaza_objeto_de_clase(Referencia, NuevaReferencia),
    retract(ViejoTermino),
    assertz(NuevoTermino),
    agrega_a_kb(NuevoTermino),
    elimina_de_kb(ViejoTermino),
    !.

/*
* modifica_propiedad_de_clase(Clase, Propiedad, Valores):
*
* Modifica la propiedad Propiedad modificando la lista o el valor
* asociado, reemplazándolo por Valores. En caso de que se quiera
* modificar una propiedad sin valores por su negada, basta con agregar
* en la misma posición que Valores, el valor por el que se quiere
* reemplazar.
*
* Modo de uso:
*
* ?- clase(c(gato, SuperClase, Propiedades, Relaciones, Objetos)).
* ...,
* Propiedades = [felino, not(patas=>[1, 2])],
* ... .
*
* ?- modifica_propiedad_de_clase(gato, patas, [1, 2, 3]).
* true.
*
* ?- clase(c(gato, SuperClase, Propiedades, Relaciones, Objetos)).
* ...,
* Propiedades = [felino, not(patas=>[1, 2, 3])],
* ... .
*
* ?- clase(c(gato, SuperClase, Propiedades, Relaciones, Objetos)).
* ...,
* Propiedades = [not(felino), not(patas=>[1, 2, 3])],
* ... .
*
* ?- modifica_propiedad_de_clase(gato, not(felino), felino).
* true.
*
* ?- clase(c(gato, SuperClase, Propiedades, Relaciones, Objetos)).
* ...,
* Propiedades = [felino, not(patas=>[1, 2, 3])],
* ... .
*/

modifica_propiedad_de_clase(Clase, not(Nombre), Valores) :-
    clase(c(Clase, _, Propiedades, _, _)),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre=>_)]),
    busca_en_lista(Nombre,Propiedades,[Nombre=>AEliminar]),
    not(elimina_inconsistencia(AEliminar,Valores,_)),
    !,
    fail.

modifica_propiedad_de_clase(Clase, not(Nombre), Valores) :-
    clase(c(Clase, SuperClase, Propiedades, Relaciones, Objetos)),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre=>_)]),
    busca_en_lista(Nombre,Propiedades,[Nombre=>AEliminar]),
    elimina_inconsistencia(AEliminar,Valores,Consistentes),
    !,
    reemplaza(Propiedades, not(Nombre=>_), not(Nombre=>Consistentes), NuevasPropiedades),
    construye_clase_term(Clase, SuperClase, NuevasPropiedades, Relaciones, Objetos, NuevaClase),
    construye_clase_term(Clase, SuperClase, Propiedades, Relaciones, Objetos, ViejaClase),
    retract(ViejaClase),
    assertz(NuevaClase),
    agrega_a_kb(NuevaClase),
    elimina_de_kb(ViejaClase),
    !.

modifica_propiedad_de_clase(Clase, not(Nombre), Valores) :-
    clase(c(Clase, SuperClase, Propiedades, Relaciones, Objetos)),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre=>_)]),
    !,
    busca_en_lista(Nombre,Propiedades,[]),
    reemplaza(Propiedades, not(Nombre=>_), not(Nombre=>Valores), NuevasPropiedades),
    construye_clase_term(Clase, SuperClase, NuevasPropiedades, Relaciones, Objetos, NuevaClase),
    construye_clase_term(Clase, SuperClase, Propiedades, Relaciones, Objetos, ViejaClase),
    retract(ViejaClase),
    assertz(NuevaClase),
    agrega_a_kb(NuevaClase),
    elimina_de_kb(ViejaClase),
    !.

modifica_propiedad_de_clase(Clase, Nombre, Valores) :-
    clase(c(Clase, _, Propiedades, _, _)),
    busca_en_lista(Nombre,Propiedades,[Nombre=>_]),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre=>AEliminar)]),
    not(elimina_inconsistencia(AEliminar,Valores,_)),
    !,
    fail.


modifica_propiedad_de_clase(Clase, Nombre, Valores) :-
    clase(c(Clase, SuperClase, Propiedades, Relaciones, Objetos)),
    busca_en_lista(Nombre,Propiedades,[Nombre=>_]),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre=>AEliminar)]),
    elimina_inconsistencia(AEliminar,Valores,Consistentes),
    !,
    reemplaza(Propiedades, Nombre=>_, Nombre=>Consistentes, NuevasPropiedades),
    construye_clase_term(Clase, SuperClase, NuevasPropiedades, Relaciones, Objetos, NuevaClase),
    construye_clase_term(Clase, SuperClase, Propiedades, Relaciones, Objetos, ViejaClase),
    retract(ViejaClase),
    assertz(NuevaClase),
    agrega_a_kb(NuevaClase),
    elimina_de_kb(ViejaClase),
    !.

modifica_propiedad_de_clase(Clase, Nombre, Valores) :-
    clase(c(Clase, SuperClase, Propiedades, Relaciones, Objetos)),
    busca_en_lista(Nombre,Propiedades,[Nombre=>_]),
    !,
    busca_en_lista(not(Nombre),Propiedades,[]),
    reemplaza(Propiedades, Nombre=>_, Nombre=>Valores, NuevasPropiedades),
    construye_clase_term(Clase, SuperClase, NuevasPropiedades, Relaciones, Objetos, NuevaClase),
    construye_clase_term(Clase, SuperClase, Propiedades, Relaciones, Objetos, ViejaClase),
    retract(ViejaClase),
    assertz(NuevaClase),
    agrega_a_kb(NuevaClase),
    elimina_de_kb(ViejaClase),
    !.
/*
* Caso si es tipo Propiedad not(Propiedad)
* En este caso solo modificamos
*/

modifica_propiedad_de_clase(Clase, not(Nombre), 'No') :-
    clase(c(Clase, _, Propiedades, _, _)),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre)]),
    !.

modifica_propiedad_de_clase(Clase, not(Nombre), 'Si') :-
    clase(c(Clase, SuperClase, Propiedades, Relaciones, Objetos)),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre)]),
    reemplaza(Propiedades, not(Nombre), Nombre, NuevasPropiedades),
    construye_clase_term(Clase, SuperClase, NuevasPropiedades, Relaciones, Objetos, NuevaClase),
    construye_clase_term(Clase, SuperClase, Propiedades, Relaciones, Objetos, ViejaClase),
    retract(ViejaClase),
    assertz(NuevaClase),
    agrega_a_kb(NuevaClase),
    elimina_de_kb(ViejaClase),
    !.

modifica_propiedad_de_clase(Clase, Nombre, 'Si') :-
    clase(c(Clase, _, Propiedades, _, _)),
    busca_en_lista(Nombre,Propiedades,[Nombre]),
    !.

modifica_propiedad_de_clase(Clase, Nombre, 'No') :-
    clase(c(Clase, SuperClase, Propiedades, Relaciones, Objetos)),
    busca_en_lista(Nombre,Propiedades,[Nombre]),
    !,
    reemplaza(Propiedades, Nombre, not(Nombre), NuevasPropiedades),
    construye_clase_term(Clase, SuperClase, NuevasPropiedades, Relaciones, Objetos, NuevaClase),
    construye_clase_term(Clase, SuperClase, Propiedades, Relaciones, Objetos, ViejaClase),
    retract(ViejaClase),
    assertz(NuevaClase),
    agrega_a_kb(NuevaClase),
    elimina_de_kb(ViejaClase),
    !.

modifica_propiedad_de_clase(_,_,_).

/*
* modifica_propiedad_de_objeto(Referencia, Propiedad, Valores).
*
* Modifica la propiedad Propiedad modificando la lista o el valor
* asociado, reemplazándolo por Valores. En caso de que se quiera
* modificar una propiedad sin valores por su negada, basta con agregar
* en la misma posiciń que Valores, el valor por el que se quiere
* reemplazar.
*
* Modo de uso:
*
* ?- ref_to_obj(1, X).
* X = o(1, perro, [agresivo, alias=>[diablo]], []).
*
* ?- modifica_propiedad_de_objeto(1, agresivo, not(agresivo)).
* true.
*
* ?- ref_to_obj(1, X).
* X = o(1, perro, [not(agresivo), alias=>[diablo]], []).
*
*
*
* ?- ref_to_obj(3, X).
* X = o(3, girasol, [color=>rojo, alias=>[rojito, chulote]], []).
*
* ?- modifica_propiedad_de_objeto(3, alias, [amarillo, solecito]).
* true.
*
* ?- ref_to_obj(3, X).
* X = o(3, girasol, [color=>rojo, alias=>[amarillo, solecito]], []).
*/


modifica_propiedad_de_objeto(Ref, not(Nombre), Valores) :-
    objeto(o(Ref, _, Propiedades, _)),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre=>_)]),
    busca_en_lista(Nombre,Propiedades,[Nombre=>AEliminar]),
    not(elimina_inconsistencia(AEliminar,Valores,_)),
    !,
    fail.

modifica_propiedad_de_objeto(Ref, not(Nombre), Valores) :-
    objeto(o(Ref, Clase, Propiedades, Relaciones)),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre=>_)]),
    busca_en_lista(Nombre,Propiedades,[Nombre=>AEliminar]),
    elimina_inconsistencia(AEliminar,Valores,Consistentes),
    !,
    reemplaza(Propiedades, not(Nombre=>_), not(Nombre=>Consistentes), NuevasPropiedades),
    construye_objeto_term(Ref,Clase, NuevasPropiedades, Relaciones, Nuevo),
    construye_objeto_term(Ref,Clase, Propiedades, Relaciones, Viejo),
    retract(Viejo),
    assertz(Nuevo),
    agrega_a_kb(Nuevo),
    elimina_de_kb(Viejo),
    !.


modifica_propiedad_de_objeto(Ref, not(Nombre), Valores) :-
    objeto(o(Ref, Clase, Propiedades, Relaciones)),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre=>_)]),
    !,
    busca_en_lista(Nombre,Propiedades,[]),
    reemplaza(Propiedades, not(Nombre=>_), not(Nombre=>Valores), NuevasPropiedades),
    construye_objeto_term(Ref,Clase, NuevasPropiedades, Relaciones, Nuevo),
    construye_objeto_term(Ref,Clase, Propiedades, Relaciones, Viejo),
    retract(Viejo),
    assertz(Nuevo),
    agrega_a_kb(Nuevo),
    elimina_de_kb(Viejo),
    !.

modifica_propiedad_de_objeto(Ref, Nombre, Valores) :-
    objeto(o(Ref, _, Propiedades, _)),
    busca_en_lista(Nombre,Propiedades,[Nombre=>_]),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre=>AEliminar)]),
    not(elimina_inconsistencia(AEliminar,Valores,_)),
    !,
    fail.

modifica_propiedad_de_objeto(Ref, Nombre, Valores) :-
    objeto(o(Ref, Clase, Propiedades, Relaciones)),
    busca_en_lista(Nombre,Propiedades,[Nombre=>_]),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre=>AEliminar)]),
    elimina_inconsistencia(AEliminar,Valores,Consistentes),
    !,
    reemplaza(Propiedades, Nombre=>_, Nombre=>Consistentes, NuevasPropiedades),
    construye_objeto_term(Ref,Clase, NuevasPropiedades, Relaciones, Nuevo),
    construye_objeto_term(Ref,Clase, Propiedades, Relaciones, Viejo),
    retract(Viejo),
    assertz(Nuevo),
    agrega_a_kb(Nuevo),
    elimina_de_kb(Viejo),
    !.

modifica_propiedad_de_objeto(Ref, Nombre, Valores) :-
    objeto(o(Ref, Clase, Propiedades, Relaciones)),
    busca_en_lista(Nombre,Propiedades,[Nombre=>_]),
    !,
    busca_en_lista(not(Nombre),Propiedades,[]),
    reemplaza(Propiedades, Nombre=>_, Nombre=>Valores, NuevasPropiedades),
    construye_objeto_term(Ref,Clase, NuevasPropiedades, Relaciones, Nuevo),
    construye_objeto_term(Ref,Clase, Propiedades, Relaciones, Viejo),
    retract(Viejo),
    assertz(Nuevo),
    agrega_a_kb(Nuevo),
    elimina_de_kb(Viejo),
    !.


/*
* Caso si es tipo Propiedad not(Propiedad)
* En este caso solo modificamos
*/

modifica_propiedad_de_objeto(Ref, not(Nombre), 'No') :-
    objeto(o(Ref, _, Propiedades, _)),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre)]),
    !.

modifica_propiedad_de_objeto(Ref, not(Nombre), 'Si') :-
    objeto(o(Ref, Clase, Propiedades, Relaciones)),
    busca_en_lista(not(Nombre),Propiedades,[not(Nombre)]),
    reemplaza(Propiedades, not(Nombre), Nombre, NuevasPropiedades),
    construye_objeto_term(Ref,Clase, NuevasPropiedades, Relaciones, Nuevo),
    construye_objeto_term(Ref,Clase, Propiedades, Relaciones, Viejo),
    retract(Viejo),
    assertz(Nuevo),
    agrega_a_kb(Nuevo),
    elimina_de_kb(Viejo),
    !.

modifica_propiedad_de_objeto(Ref, Nombre, 'Si') :-
    objeto(o(Ref, _, Propiedades, _)),
    busca_en_lista(Nombre,Propiedades,[Nombre]),
    !.

modifica_propiedad_de_objeto(Ref, Nombre, 'No') :-
    objeto(o(Ref, Clase, Propiedades, Relaciones)),
    busca_en_lista(Nombre,Propiedades,[Nombre]),
    !,
    reemplaza(Propiedades, Nombre, not(Nombre), NuevasPropiedades),
    construye_objeto_term(Ref,Clase, NuevasPropiedades, Relaciones, Nuevo),
    construye_objeto_term(Ref,Clase, Propiedades, Relaciones, Viejo),
    retract(Viejo),
    assertz(Nuevo),
    agrega_a_kb(Nuevo),
    elimina_de_kb(Viejo),
    !.

modifica_propiedad_de_objeto(_,_,_).


/*
* modifica_relacion_de_clase(Clase, Relacion, Valores):
*
* Modifica la relacion Relacion modificando la lista o el valor
* asociado, reemplazándolo por Valores. En caso de que se quiera
* modificar una relacion sin valores por su negada, basta con agregar
* en la misma posición que Valores, el valor por el que se quiere
* reemplazar.
*

*/



modifica_relacion_de_clase(Clase, not(Nombre), Valores) :-
    clase(c(Clase, _, _, Relaciones, _)),
    busca_en_lista(not(Nombre),Relaciones,[not(Nombre=>_)]),
    busca_en_lista(Nombre,Relaciones,[Nombre=>AEliminar]),
    not(elimina_inconsistencia(AEliminar,Valores,_)),
    !,
    fail.

modifica_relacion_de_clase(Clase, not(Nombre), Valores) :-
    clase(c(Clase, SuperClase, Propiedades, Relaciones, Objetos)),
    busca_en_lista(not(Nombre),Relaciones,[not(Nombre=>_)]),
    busca_en_lista(Nombre,Relaciones,[Nombre=>AEliminar]),
    elimina_inconsistencia(AEliminar,Valores,Consistentes),
    !,
    reemplaza(Relaciones, not(Nombre=>_), not(Nombre=>Consistentes), NuevasRelaciones),
    construye_clase_term(Clase, SuperClase, Propiedades, NuevasRelaciones, Objetos, NuevaClase),
    construye_clase_term(Clase, SuperClase, Propiedades, Relaciones, Objetos, ViejaClase),
    retract(ViejaClase),
    assertz(NuevaClase),
    agrega_a_kb(NuevaClase),
    elimina_de_kb(ViejaClase),
    !.

modifica_relacion_de_clase(Clase, not(Nombre), Valores) :-
    clase(c(Clase, SuperClase, Propiedades, Relaciones, Objetos)),
    busca_en_lista(not(Nombre),Relaciones,[not(Nombre=>_)]),
    !,
    busca_en_lista(Nombre,Relaciones,[]),
    reemplaza(Relaciones, not(Nombre=>_), not(Nombre=>Valores), NuevasRelaciones),
    construye_clase_term(Clase, SuperClase, Propiedades, NuevasRelaciones, Objetos, NuevaClase),
    construye_clase_term(Clase, SuperClase, Propiedades, Relaciones, Objetos, ViejaClase),
    retract(ViejaClase),
    assertz(NuevaClase),
    agrega_a_kb(NuevaClase),
    elimina_de_kb(ViejaClase),
    !.

modifica_relacion_de_clase(Clase, Nombre, Valores) :-
    clase(c(Clase, _, _, Relaciones, _)),
    busca_en_lista(Nombre,Relaciones,[Nombre=>_]),
    busca_en_lista(not(Nombre),Relaciones,[not(Nombre=>AEliminar)]),
    not(elimina_inconsistencia(AEliminar,Valores,_)),
    !,
    fail.

modifica_relacion_de_clase(Clase, Nombre, Valores) :-
    clase(c(Clase, SuperClase, Propiedades, Relaciones, Objetos)),
    busca_en_lista(Nombre,Relaciones,[Nombre=>_]),
    busca_en_lista(not(Nombre),Relaciones,[not(Nombre=>AEliminar)]),
    elimina_inconsistencia(AEliminar,Valores,Consistentes),
    !,
    reemplaza(Relaciones, Nombre=>_, Nombre=>Consistentes, NuevasRelaciones),
    construye_clase_term(Clase, SuperClase, Propiedades, NuevasRelaciones, Objetos, NuevaClase),
    construye_clase_term(Clase, SuperClase, Propiedades, Relaciones, Objetos, ViejaClase),
    retract(ViejaClase),
    assertz(NuevaClase),
    agrega_a_kb(NuevaClase),
    elimina_de_kb(ViejaClase),
    !.

modifica_relacion_de_clase(Clase, Nombre, Valores) :-
    clase(c(Clase, SuperClase, Propiedades, Relaciones, Objetos)),
    busca_en_lista(Nombre,Relaciones,[Nombre=>_]),
    !,
    busca_en_lista(not(Nombre),Relaciones,[]),
    reemplaza(Relaciones, Nombre=>_, Nombre=>Valores, NuevasRelaciones),
    construye_clase_term(Clase, SuperClase, Propiedades, NuevasRelaciones, Objetos, NuevaClase),
    construye_clase_term(Clase, SuperClase, Propiedades, Relaciones, Objetos, ViejaClase),
    retract(ViejaClase),
    assertz(NuevaClase),
    agrega_a_kb(NuevaClase),
    elimina_de_kb(ViejaClase),
    !.

modifica_relacion_de_clase(_,_, _).

/*
* modifica_relacion_de_objeto(Referencia, Relacion, Valores).
*
* Modifica la relacion Relacion modificando la lista o el valor
* asociado, reemplazándolo por Valores. En caso de que se quiera
* modificar una relacion sin valores por su negada, basta con agregar
* en la misma posiciń que Valores, el valor por el que se quiere
* reemplazar.

*/

modifica_relacion_de_objeto(Ref, not(Nombre), Valores) :-
    objeto(o(Ref, _, _, Relaciones)),
    busca_en_lista(not(Nombre),Relaciones,[not(Nombre=>_)]),
    busca_en_lista(Nombre,Relaciones,[Nombre=>AEliminar]),
    not(elimina_inconsistencia(AEliminar,Valores,_)),
    !,
    fail.

modifica_relacion_de_objeto(Ref, not(Nombre), Valores) :-
    objeto(o(Ref, Clase, Propiedades, Relaciones)),
    busca_en_lista(not(Nombre),Relaciones,[not(Nombre=>_)]),
    busca_en_lista(Nombre,Relaciones,[Nombre=>AEliminar]),
    elimina_inconsistencia(AEliminar,Valores,Consistentes),
    !,
    reemplaza(Relaciones, not(Nombre=>_), not(Nombre=>Consistentes), NuevasRelaciones),
    construye_objeto_term(Ref,Clase, Propiedades, NuevasRelaciones, Nuevo),
    construye_objeto_term(Ref,Clase, Propiedades, Relaciones, Viejo),
    retract(Viejo),
    assertz(Nuevo),
    agrega_a_kb(Nuevo),
    elimina_de_kb(Viejo),
    !.

modifica_relacion_de_objeto(Ref, not(Nombre), Valores) :-
    objeto(o(Ref, Clase, Propiedades, Relaciones)),
    busca_en_lista(not(Nombre),Relaciones,[not(Nombre=>_)]),
    !,
    busca_en_lista(Nombre,Relaciones,[]),
    reemplaza(Relaciones, not(Nombre=>_), not(Nombre=>Valores), NuevasRelaciones),
    construye_objeto_term(Ref,Clase, Propiedades, NuevasRelaciones, Nuevo),
    construye_objeto_term(Ref,Clase, Propiedades, Relaciones, Viejo),
    retract(Viejo),
    assertz(Nuevo),
    agrega_a_kb(Nuevo),
    elimina_de_kb(Viejo),
    !.

modifica_relacion_de_objeto(Ref, Nombre, Valores) :-
    objeto(o(Ref, _, _, Relaciones)),
    busca_en_lista(Nombre,Relaciones,[Nombre=>_]),
    busca_en_lista(not(Nombre),Relaciones,[not(Nombre=>AEliminar)]),
    not(elimina_inconsistencia(AEliminar,Valores,_)),
    !,
    fail.

modifica_relacion_de_objeto(Ref, Nombre, Valores) :-
    objeto(o(Ref, Clase, Propiedades, Relaciones)),
    busca_en_lista(Nombre,Relaciones,[Nombre=>_]),
    busca_en_lista(not(Nombre),Relaciones,[not(Nombre=>AEliminar)]),
    elimina_inconsistencia(AEliminar,Valores,Consistentes),
    !,
    reemplaza(Relaciones, Nombre=>_, Nombre=>Consistentes, NuevasRelaciones),
    construye_objeto_term(Ref,Clase, Propiedades, NuevasRelaciones, Nuevo),
    construye_objeto_term(Ref,Clase, Propiedades, Relaciones, Viejo),
    retract(Viejo),
    assertz(Nuevo),
    agrega_a_kb(Nuevo),
    elimina_de_kb(Viejo),
    !.

modifica_relacion_de_objeto(Ref, Nombre, Valores) :-
    objeto(o(Ref, Clase, Propiedades, Relaciones)),
    busca_en_lista(Nombre,Relaciones,[Nombre=>_]),
    !,
    busca_en_lista(not(Nombre),Relaciones,[]),
    reemplaza(Relaciones, Nombre=>_, Nombre=>Valores, NuevasRelaciones),
    construye_objeto_term(Ref,Clase, Propiedades, NuevasRelaciones, Nuevo),
    construye_objeto_term(Ref,Clase, Propiedades, Relaciones, Viejo),
    retract(Viejo),
    assertz(Nuevo),
    agrega_a_kb(Nuevo),
    elimina_de_kb(Viejo),
    !.

modifica_relacion_de_objeto(_, _, _).


/*
*  reemplaza_objeto_de_clase(Viejo, Nuevo).
*
*  Reemplaza el identificador Viejo en la clase del objeto definido por
*  Viejo por el nuevo identificador Nuevo.
*
*/
reemplaza_objeto_de_clase(Viejo, Nuevo) :-
    objeto(o(Viejo, Clase, _, _)),
    clase(c(Clase, Super, Propiedades, Relaciones, Objetos)),
    reemplaza(Objetos, Viejo, Nuevo, NuevosObjetos),
    construye_clase_term(Clase, Super, Propiedades, Relaciones, Objetos, ViejoTermino),
    construye_clase_term(Clase, Super, Propiedades, Relaciones, NuevosObjetos, NuevoTermino),
    retract(ViejoTermino),
    assertz(NuevoTermino),
    agrega_a_kb(NuevoTermino),
    elimina_de_kb(ViejoTermino),
    !.

/*
*  reemplaza_de_relaciones(Viejo, Nuevo)
*
*  Reemplaza de todas las relaciones de objetos y clases, el valor de
*  Viejo por Nuevo.
*
*/
reemplaza_de_relaciones(Viejo, Nuevo) :-
    extension_de_clase(top, Objetos),
    reemplaza_de_relaciones_de_objetos(Objetos, Viejo, Nuevo),
    subclases(top, Clases),
    reemplaza_de_relaciones_de_clases(Clases, Viejo, Nuevo).

/*
* reemplaza_de_relaciones_de_objetos(Objetos, Viejo, Nuevo)
*
* De la lista de objetos Objetos, reemplaza el valor de Viejo por Nuevo
* si existe en la lista de relaciones.
*
*/
reemplaza_de_relaciones_de_objetos([Objeto|Objetos], Viejo, Nuevo) :-
    reemplaza_de_relaciones_de_objeto(Objeto, Viejo, Nuevo),
    reemplaza_de_relaciones_de_objetos(Objetos, Viejo, Nuevo).

reemplaza_de_relaciones_de_objetos([], _, _).

/*
* reemplaza_de_relaciones_de_clases(Clases, Viejo, Nuevo)
*
* De la lista de clases Clases, reemplaza el valor de Viejo por Nuevo
* si existe en la lista de relaciones.
*
*/
reemplaza_de_relaciones_de_clases([Clase|Clases], Viejo, Nuevo) :-
    reemplaza_de_relaciones_de_clase(Clase, Viejo, Nuevo),
    reemplaza_de_relaciones_de_clases(Clases, Viejo, Nuevo).

reemplaza_de_relaciones_de_clases([], _, _).

reemplaza_de_relaciones_de_clase(Clase, Viejo, Nuevo) :-
    clase(c(Clase, Super, Propiedades, Relaciones, Objetos)),
    reemplaza_de_relaciones(Relaciones, Viejo, Nuevo, NuevasRelaciones),
    construye_clase_term(Clase, Super, Propiedades, Relaciones, Objetos, Termino),
    construye_clase_term(Clase, Super, Propiedades, NuevasRelaciones, Objetos, NuevoTermino),
    retract(Termino),
    assertz(NuevoTermino),
    reemplaza_en_kb(Termino, NuevoTermino),
    !.


reemplaza_de_relaciones_de_objeto(Referencia, Viejo, Nuevo) :-
    objeto(o(Referencia, Clase, Propiedades, Relaciones)),
    !,
    reemplaza_de_relaciones(Relaciones, Viejo, Nuevo, NuevasRelaciones),
    construye_objeto_term(Referencia, Clase, Propiedades, Relaciones, Termino),
    construye_objeto_term(Referencia, Clase, Propiedades, NuevasRelaciones, NuevoTermino),
    retract(Termino),
    assertz(NuevoTermino),
    reemplaza_en_kb(Termino, NuevoTermino).

/*
*  reemplaza_de_relaciones(Relaciones, Viejo, Nuevo, NuevasRelaciones)
*
*  reemplaza el valor de Viejo por Nuevo en la lista de Relaciones y
*  almacenando el resultado en NuevasRelaciones.
*
*/
reemplaza_de_relaciones([Rel|Resto], Viejo, Nuevo, [NuevaRelacion|NuevoResto]) :-
    reemplaza_de_relacion(Rel, Viejo, Nuevo, NuevaRelacion),
    reemplaza_de_relaciones(Resto, Viejo, Nuevo, NuevoResto),
    !.
reemplaza_de_relaciones([], _, _, []).

reemplaza_de_relacion(not(Atr=>Valores), Viejo, Nuevo, not(Atr=>NuevosValores)) :-
    reemplaza(Valores, Viejo, Nuevo, NuevosValores),
    !.

reemplaza_de_relacion(Atr=>Valores, Viejo, Nuevo, Atr=>NuevosValores) :-
    reemplaza(Valores, Viejo, Nuevo, NuevosValores).
