:-[consulta].

/*
 * extension_de_propiedad_(KB,Prop,Objetos)
 * La lsta de los objetos en KB que tienen la propiedad Prop es Objetos.
 */
extension_de_propiedad_(KB,Prop,Objetos):-
    findall(Id,member(objeto(Id,_,_,_),KB),Todos),
    filtra_objetos_por_propiedad(KB,Todos,Prop,Objetos).

/*
 * filtra_objetos_por_propiedad(KB,Objetos,Prop,Filtrados)
 * Filtrados es la lista de objetos en Objetos que tienen la propiedad Prop
 * Solo toma en cuenta el nombre de la propiedad, no valores específicos
 */

filtra_objetos_por_propiedad(_,[],_,[]):-!.    

filtra_objetos_por_propiedad(KB,[Objeto|Objetos],Prop,[Objeto=>Valor|ObjetosFiltrados]):-
    objeto_tiene_propiedad(KB,Objeto,Prop,Valor),
    filtra_objetos_por_propiedad(KB,Objetos,Prop,ObjetosFiltrados),
    !.

filtra_objetos_por_propiedad(KB,[_|Objetos],Prop,ObjetosFiltrados):-
    filtra_objetos_por_propiedad(KB,Objetos,Prop,ObjetosFiltrados).


/*
 * objeto_tiene_propiedad(KB,Objeto,Prop,Valor)
 * El objeto con id Objeto en la kb KB, tiene la propiedad Prop con valor Valor
 * Prop es de la forma Prop o not(Prop)
 * Solo toma en cuenta el nombre de la propiedad, no valores específicos
 */
objeto_tiene_propiedad(KB,Objeto,not(Prop),Valor):-
    propiedades_de_objeto(KB,Objeto,Props),
    extiendePropiedades(Props,PropsExt),
    member(not(Prop=>Valor),PropsExt),
    !.

objeto_tiene_propiedad(KB,Objeto,Prop,Valor):-
    propiedades_de_objeto(KB,Objeto,Props),
    extiendePropiedades(Props,PropsExt),
    member(Prop=>Valor,PropsExt),
    !.

/*
 * extension_de_propiedad_(KB,Prop,Objetos)
 * La lsta de los objetos en KB que tienen la propiedad Prop es Objetos.
 */
extension_de_relacion_(KB,Rel,Objetos):-
    findall(Id,member(objeto(Id,_,_,_),KB),Todos),
    filtra_objetos_por_relacion(KB,Todos,Rel,Objetos).

/*
 * filtra_objetos_por_propiedad(KB,Objetos,Prop,Filtrados)
 * Filtrados es la lista de objetos en Objetos que tienen la relacion Rel
 * Solo toma en cuenta el nombre de la propiedad, no valores específicos
 */

filtra_objetos_por_relacion(_,[],_,[]):-!.

filtra_objetos_por_relacion(KB,[Objeto|Objetos],Rel,[Objeto=>Valor|ObjetosFiltrados]):-
    objeto_tiene_relacion(KB,Objeto,Rel,Valor),
    filtra_objetos_por_relacion(KB,Objetos,Rel,ObjetosFiltrados),
    !.

filtra_objetos_por_relacion(KB,[_|Objetos],Rel,ObjetosFiltrados):-
    filtra_objetos_por_relacion(KB,Objetos,Rel,ObjetosFiltrados).


/*
 * objeto_tiene_relacion(KB,Objeto,Prop,Valor)
 * El objeto con id Objeto en la kb KB, tiene la relacion Prop con valor Valor
 * Rel es de la forma Rel o not(Rel)
 * Solo toma en cuenta el nombre de la relacion, no valores específicos
 */

objeto_tiene_relacion(KB,Objeto,not(Rel),Valor):-
    relaciones_de_objeto(KB,Objeto,Rels),
    member(not(Rel=>Valor),Rels),
    !.

objeto_tiene_relacion(KB,Objeto,Rel,Valor):-
    relaciones_de_objeto(KB,Objeto,Rels),
    member(Rel=>Valor,Rels),
    !.

/*
 * filtra_por_valores(ListaIdValor,Valor,ListaIdValorFiltrado)
 * ListaIdValorFiltrado esta formada por los elementos de ListaIdValor que tienen el valor especificado
 */

filtra_por_valor([],_,[]):-!.

filtra_por_valor([Atr=>Vals|Resto],Valor,[Atr=>Vals|RestoFiltrado]):-
    member(Valor,Vals), 
    filtra_por_valor(Resto,Valor,RestoFiltrado),
    !.
    
filtra_por_valor([_|Resto],Valor,RestoFiltrado):-
    filtra_por_valor(Resto,Valor,RestoFiltrado).
