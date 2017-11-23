/*
 * Agrega operador =>.
 */
:- op(800,xfx,'=>').


/*
 * extiendePropiedades(Originales,Extendidas)
 * true si Extendidas es la lista que convierte las propiedades simples de la sigueinte forma:
 * Reemplaza las propiedades simples por su forma extendida.
 * Atr por Atr=>si
 * not(Atr) por Atr=>no
 * as demás quedan igual.
 */



extiendePropiedades([],[]):-!.

extiendePropiedades([not(Atr=>Val)|Props],[not(Atr=>Val)|ExtProps]):-
    extiendePropiedades(Props,ExtProps),
    !.

extiendePropiedades([Atr=>Val|Props],[Atr=>Val|ExtProps]):-
    extiendePropiedades(Props,ExtProps),
    !.

extiendePropiedades([not(Prop)|Props],[Prop=>no|[not(Prop=>si)|ExtProps]]):-
    extiendePropiedades(Props,ExtProps),
    !.

extiendePropiedades([Prop|Props],[Prop=>si|[not(Prop=>no)|ExtProps]]):-
    extiendePropiedades(Props,ExtProps),
    !.

/*
 * primera(X,Lista,Y)
 *
 * Si la primera aparición de X en Lista es X entonces Y = X
 * Si la primera aparición de X en Lista es not(X) entonces Y = not(X)
 */
primera(X, [X|_], X) :- !.
primera(X, [not(X)|_], not(X)) :- !.
primera(not(X), [X|_], X) :- !.
primera(not(X), [not(X)|_], not(X)) :- !.
primera(X, [Y|Tail], Z):-dif(X, Y),dif(not(X), Y),dif(X, not(Y)),primera(X, Tail, Z).

/*
* reemplaza(Lista,Actual,Nuevo,NuevaLista)
* Reemplazar todas las apariciones de Actual por Nuevo en Lista da como resultado NuevaLista
*/
reemplaza([],_,_,[]):-!.
reemplaza([X|Resto],X,Y,[Y|RestoReemplazado]):-
	!,
	reemplaza(Resto,X,Y,RestoReemplazado).
reemplaza([W|Resto],X,Y,[W|RestoReemplazado]):-
	!,
	reemplaza(Resto,X,Y,RestoReemplazado).

/* imprime(Lista)
 * Imprime Lista un elemento por linea
 */
imprime([]).
imprime([H|T]) :- write(H), nl, imprime(T).


/*
* ids_numericos(Ids,IdsNumericos)
* los id's numricos de la lista Ids es IdsNumericos
*/

ids_numericos([X|Resto],[X|NumericosEnResto]):-
    number(X),
    ids_numericos(Resto,NumericosEnResto),
    !.
ids_numericos([_|Resto],NumericosEnResto):-
    ids_numericos(Resto,NumericosEnResto),
    !.
ids_numericos([],[]).

/*
 * Niega la lista las propiedades o relaciones de una lista.
 * true si la ListaNegada contiene las mismas propiedades o relaciones que Lista pero en su forma negada.
 * niega_lista(Lista,ListaNegada)
 */

/*
 * Primero eliminamos doble negación para que todo funcione mejor
 */
niega_lista([not(not(X))|Resto],Res):-
    niega_lista([X|Resto],Res),!.
/*
 * Caso not(X) se convierte en X
 */
niega_lista([not(X)|Resto],[X|RestoNegado]):-
    niega_lista(Resto,RestoNegado).
/*
 * Caso X se convierte en not(X)
 */
niega_lista([X|Resto],[not(X)|RestoNegado]):-
    niega_lista(Resto,RestoNegado).
/*
 * Caso base.
 * Si ya no hay nada ya acabamos.
 */
niega_lista([],[]).

/*
 *
 * Fusiona los atibutos contenidos en una lista respetando el orden de especificidad
 * fusiona([bonita,color=>blanco,color=>val([azul,negro]),not(color=>rojo),not(bonita)],Res).
 * Res = [color=>blanco, bonita, not(color=>[rojo])].
 */

fusiona(Lista,ListaFusionada):-
    clasifica(Lista,Positivas_,Negativas_),
    delete(Positivas_,_=>[],Positivas),
    delete(Negativas_,not(_=>[]),Negativas),
    append(Positivas,Negativas,ListaFusionada).

/*
* clasifica(Lista,Positivas,Negativas)
* Llamada principal
*/
clasifica(Lista,Positivas,Negativas):-
    clasifica(Lista,[],[],Positivas,NegativasPositivas),
    niega_lista(NegativasPositivas,Negativas).

/*
 * clasifica(Lista,PositivasEncontradas,NegativasEncontradas,PositivasEncontradas,NegativasEncontradas)
 *
 * Categoriza las propiedades o relaciones de Lista en Negativas y
 * positivas repetando la especificidad Lista una lista con las
 * propiedades o relaciones posibles a agregar (solo en caso de no
 * estar ya agregadas anteriormente) PositivasEncontradas Las
 * propiedades o relaciones positivas que ya se encontraron
 * NegativasEncontradas Las propiedades o relaciones negativas que ya
 * se encontraron
 */
clasifica([],PositivasEncontradas,NegativasEncontradas,PositivasEncontradas,NegativasEncontradas):-!.
/* Caso atributo=>ListaDeValores */
clasifica([not(not(X))|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    !,
    clasifica([X|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas).

clasifica([not(Atr=>ValoresNuevos)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    is_list(ValoresNuevos),
    primera(Atr=>ValoresPositivos,PositivasEncontradas,Atr=>ValoresPositivos),
    is_list(ValoresPositivos),
    primera(Atr=>ValoresNegativos,NegativasEncontradas,Atr=>ValoresNegativos),
    is_list(ValoresNegativos),
    !,
    subtract(ValoresNuevos,ValoresPositivos,NuevosMenosPositivos),
    subtract(NuevosMenosPositivos,ValoresNegativos,NuevosMenosPositivosMenosNegativos),
    append(ValoresNegativos,NuevosMenosPositivosMenosNegativos,NuevosNegativos),
    reemplaza(NegativasEncontradas,Atr=>ValoresNegativos,Atr=>NuevosNegativos,NuevasNegativasEncontradas),
    clasifica(Restantes,PositivasEncontradas,NuevasNegativasEncontradas,Positivas,Negativas).

clasifica([not(Atr=>ValoresNuevos)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    is_list(ValoresNuevos),
    primera(Atr=>ValoresPositivos,PositivasEncontradas,Atr=>ValoresPositivos),
    is_list(ValoresPositivos),
    !,
    subtract(ValoresNuevos,ValoresPositivos,NuevosMenosPositivos),
    clasifica(
        Restantes,
        PositivasEncontradas,
        [Atr=>NuevosMenosPositivos|NegativasEncontradas],
        Positivas,
        Negativas).

clasifica([not(Atr=>ValoresNuevos)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    is_list(ValoresNuevos),
    primera(Atr=>ValoresNegativos,NegativasEncontradas,Atr=>ValoresNegativos),
    is_list(ValoresNegativos),
    !,
    subtract(ValoresNuevos,ValoresNegativos,NuevosNegativos),
    reemplaza(NegativasEncontradas,Atr=>ValoresNegativos,Atr=>NuevosNegativos,NuevasNegativasEncontradas),
    clasifica(Restantes,PositivasEncontradas,NuevasNegativasEncontradas,Positivas,Negativas).

clasifica([not(Atr=>ValoresNuevos)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    is_list(ValoresNuevos),
    !,
    clasifica(
        Restantes,
        PositivasEncontradas,
        [Atr=>ValoresNuevos|NegativasEncontradas],
        Positivas,
        Negativas).


clasifica([Atr=>ValoresNuevos|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    is_list(ValoresNuevos),
    primera(Atr=>ValoresPositivos,PositivasEncontradas,Atr=>ValoresPositivos),
    is_list(ValoresPositivos),
    primera(Atr=>ValoresNegativos,NegativasEncontradas,Atr=>ValoresNegativos),
    is_list(ValoresNegativos),
    !,
    subtract(ValoresNuevos,ValoresPositivos,NuevosMenosPositivos),
    subtract(NuevosMenosPositivos,ValoresNegativos,NuevosMenosPositivosMenosNegativos),
    append(ValoresPositivos,NuevosMenosPositivosMenosNegativos,NuevosPositivos),
    reemplaza(PositivasEncontradas,Atr=>ValoresPositivos,Atr=>NuevosPositivos,NuevasPositivasEncontradas),
    clasifica(Restantes,NuevasPositivasEncontradas,NegativasEncontradas,Positivas,Negativas).

clasifica([Atr=>ValoresNuevos|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    is_list(ValoresNuevos),
    primera(Atr=>ValoresPositivos,PositivasEncontradas,Atr=>ValoresPositivos),
    is_list(ValoresPositivos),
    !,
    subtract(ValoresNuevos,ValoresPositivos,NuevosMenosPositivos),
    append(ValoresPositivos,NuevosMenosPositivos,NuevosPositivos),
    reemplaza(PositivasEncontradas,Atr=>ValoresPositivos,Atr=>NuevosPositivos,NuevasPositivasEncontradas),
    clasifica(Restantes,NuevasPositivasEncontradas,NegativasEncontradas,Positivas,Negativas).

clasifica([Atr=>ValoresNuevos|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    is_list(ValoresNuevos),
    primera(Atr=>ValoresNegativos,NegativasEncontradas,Atr=>ValoresNegativos),
    is_list(ValoresNegativos),
    !,
    subtract(ValoresNuevos,ValoresNegativos,NuevosMenosNegativos),
    clasifica(Restantes,[Atr=>NuevosMenosNegativos|PositivasEncontradas],NegativasEncontradas,Positivas,Negativas).

clasifica([Atr=>ValoresNuevos|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    is_list(ValoresNuevos),
    !,
    clasifica(
        Restantes,
        [Atr=>ValoresNuevos|PositivasEncontradas],
        NegativasEncontradas,
        Positivas,
        Negativas).

/*
* Caso menos complicado valor=>atributo
* Puede haber muchos negados pero solo un positivo
*/

clasifica([not(Atr=>Valor)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    not(is_list(Valor)),
    not(member(Atr=>Valor,PositivasEncontradas)),
    not(member(Atr=>_,NegativasEncontradas)),
    !,
    clasifica(Restantes,PositivasEncontradas,[Atr=>[Valor]|NegativasEncontradas],Positivas,Negativas).

clasifica([not(Atr=>Valor)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    not(is_list(Valor)),
    not(member(Atr=>Valor,PositivasEncontradas)),
    primera(Atr=>ValoresNegativos,NegativasEncontradas,Atr=>ValoresNegativos),
    not(member(Valor,ValoresNegativos)),
    reemplaza(NegativasEncontradas,Atr=>ValoresNegativos,Atr=>[Valor|ValoresNegativos],NuevasNegativasEncontradas),
    !,
    clasifica(Restantes,PositivasEncontradas,NuevasNegativasEncontradas,Positivas,Negativas).

clasifica([not(_=>_)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    !,
    clasifica(Restantes,PositivasEncontradas,NegativasEncontradas,Positivas,Negativas).

clasifica([Atr=>Valor|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    not(is_list(Valor)),
    not(member(Atr=>_,PositivasEncontradas)),
    primera(Atr=>ValoresNegativos,NegativasEncontradas,Atr=>ValoresNegativos),
    not(member(Valor,ValoresNegativos)),
    !,
    clasifica(Restantes,[Atr=>Valor|PositivasEncontradas],NegativasEncontradas,Positivas,Negativas).

clasifica([Atr=>Valor|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    not(is_list(Valor)),
    not(member(Atr=>_,PositivasEncontradas)),
    not(primera(Atr=>_,NegativasEncontradas,Atr=>_)),
    !,
    clasifica(Restantes,[Atr=>Valor|PositivasEncontradas],NegativasEncontradas,Positivas,Negativas).

clasifica([_=>_|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    !,
    clasifica(Restantes,PositivasEncontradas,NegativasEncontradas,Positivas,Negativas).


/*Caso propiedades simples propiedad o not(propiedad)*/
clasifica([not(Atr)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    not(member(Atr,PositivasEncontradas)),
    not(member(Atr,NegativasEncontradas)),
    clasifica(Restantes,PositivasEncontradas,[Atr|NegativasEncontradas],Positivas,Negativas),
    !.
clasifica([not(_)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    clasifica(Restantes,PositivasEncontradas,NegativasEncontradas,Positivas,Negativas),
    !.
clasifica([Atr|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    not(member(Atr,PositivasEncontradas)),
    not(member(Atr,NegativasEncontradas)),
    clasifica(Restantes,[Atr|PositivasEncontradas],NegativasEncontradas,Positivas,Negativas),
    !.
clasifica([_|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
    clasifica(Restantes,PositivasEncontradas,NegativasEncontradas,Positivas,Negativas).

/*
* Estructura para cola de prioridad en una lista
* Agregar en PQ a key=>Value da como resultado NPQ
* agrega_pq(PQ,key=>Value,NPQ)
* Eliminar el valor Val de PQ da NPQ
* elimina_pq(PQ,Val,NPQ)
* Modficar la llave del valor Val por Key en PQ da NPQ,
* Si no existe se agrega.
* cambia_key_pq(PQ,Key=>Val,NPQ)
*/

agrega_pq([],Key=>Val,[Key=>Val]):-
    !.

agrega_pq([Lower=>Value|RPQ],Key=>Val,[Key=>Val|[Lower=>Value|RPQ]]):-
    Key<Lower,
    !.

agrega_pq([Lower=>Value|RPQ],Key=>Val,[Lower=>Value|NRPQ]):-
    agrega_pq(RPQ,Key=>Val,NRPQ),
    !.
/*Agrega una lista de llave=>valor a PQ resultando NPQ*/
agrega_pq_muchos(Pq,[Primero|Resto],NPQ):-
    agrega_pq(Pq,Primero,Pq2),
    agrega_pq_muchos(Pq2,Resto,NPQ),!.

agrega_pq_muchos(Pq,[],Pq).

/* Elimina de PQ la aparición de Val*/
elimina_pq(PQ,Val,NPQ):-
    delete(PQ,_=>Val,NPQ).

cambia_key_pq(PQ,Key=>Val,NPQ):-
    elimina_pq(PQ,Val,NPQ1),
    agrega_pq(NPQ1,Key=>Val,NPQ).

/*
* Convierte una lista de Atr=>Valor a una lista de solamente los atributos
* Ejemplo lista_de_atributos([a1=>v1,a2=>v2,...,an=>vn],[a1,a2,...,an])
*/
lista_de_atributos([Atr=>_|Resto],[Atr|AtrResto]):-
    lista_de_atributos(Resto,AtrResto).

lista_de_atributos([],[]).

/*
* Convierte una lista de Atr=>Valor a una lista de solamente los atributos
* Ejemplo lista_de_atributos([a1=>v1,a2=>v2,...,an=>vn],[v1,v2,...,vn])
*/
lista_de_valores(Lista,Valores):-
    voltea_av(Lista,Volteada),
    lista_de_atributos(Volteada,Valores),
    !.

/*
* voltea_av(Lista,ListaVolteada)
* Voltea atr=>val por val=>atr
*/
voltea_av([Atr=>Val|Resto],[Val=>Atr|VolteaResto]):-
    voltea_av(Resto,VolteaResto),!.
voltea_av([],[]).

/*
 * filtra_por_valores(ListaIdValor,Valor,ListaIdValorFiltrado)
 * ListaIdValorFiltrado esta formada por los elementos de ListaIdValor que tienen el valor especificado
 */

filtra_por_valor([],_,[]):-!.

filtra_por_valor([Atr=>Vals|Resto],Valor,[Atr=>Vals|RestoFiltrado]):-
    ((is_list(Vals),member(Valor,Vals));(Vals = Valor)),
    filtra_por_valor(Resto,Valor,RestoFiltrado),
    !.

filtra_por_valor([_|Resto],Valor,RestoFiltrado):-
    filtra_por_valor(Resto,Valor,RestoFiltrado).

/*
 * filtra_por_valores(ListaIdValor,Valor,ListaIdValorFiltrado)
 * ListaIdValorFiltrado esta formada por los elementos de ListaIdValor que tienen el valor especificado
 */

filtra_por_atributo([],_,[]):-!.

filtra_por_atributo([Atr=>Vals|Resto],Atr,[Atr=>Vals|RestoFiltrado]):-
    filtra_por_atributo(Resto,Atr,RestoFiltrado),
    !.

filtra_por_atributo([Atr2=>_|Resto],Atr,RestoFiltrado):-
    dif(Atr2,Atr),
    filtra_por_atributo(Resto,Atr,RestoFiltrado).

/*
* Filtra por valores
* filtra_por_valores(Lista,Valores,Filtrada)
*/
filtra_por_valores(Lista,[Valor|Resto],Filtrada):-
    filtra_por_valor(Lista,Valor,FiltradaPorValor),
    filtra_por_valores(Lista,Resto,FiltradaPorResto),
    append(FiltradaPorValor,FiltradaPorResto,Filtrada).

filtra_por_valores(_,[],[]).

/*
* Filtra por valores
* filtra_por_valores(Lista,Valores,Filtrada)
*/
filtra_por_atributos(Lista,[Atr|Resto],Filtrada):-
    filtra_por_atributo(Lista,Atr,FiltradaPorAtr),
    filtra_por_atributos(Lista,Resto,FiltradaPorResto),
    append(FiltradaPorAtr,FiltradaPorResto,Filtrada).

filtra_por_atributos(_,[],[]).

/*
* Obtiene los estantes pobservados
*/
obten_estantes_observados(KB,EstantesObservados):-
    extension_de_clase(KB,estante,Estantes),
    extension_de_propiedad_(KB,observado,Ext),
    filtra_por_valor(Ext,si,Observados_),
    lista_de_atributos(Observados_,Observados),
    intersection(Estantes,Observados,EstantesObservados).


/*
* Obtiene los estantes que no se han escaneado()
* TODO Modificar para tomar en cuanta la propiedad observado o not(observado)
*/
obten_estantes_no_observados(KB,EstantesNoObservados):-
    /* Obtenemos todos los estantes*/
    extension_de_clase(KB,estante,Todos),
    /*Obtenemos todos los estantes observados*/
    obten_estantes_observados(KB,EstantesObservados),
    /* Verificamos que todos ya se hayan visitado*/
    subtract(Todos,EstantesObservados,EstantesNoObservados).

/*
* Obtiene las creencias del robot que hay en la KB
*/
obten_creencias(KB,Creencias):-
    extension_de_propiedad_(KB,cree,Ext),
    lista_de_valores(Ext,[val(Creencias)]).

/*
* Obtiene las observaciones del robot que hay en la KB
*/
obten_observaciones(KB,Observaciones):-
    extension_de_propiedad_(KB,obs,Ext),
    lista_de_valores(Ext,[val(Observaciones)]).


/*
* Obtiene los objetos movidos por el robot.
*/
obten_movidos(KB,Movidos):-
    extension_de_propiedad_(KB,movidos,Ext),
    lista_de_valores(Ext,[val(Movidos)]).

/*Obtiene el costo de una accion dada*/

/* Caso especial ba es la fusion de buscar y agarrar*/
obten_costo(KB,ba(Oi),C):-
    obten_costo(KB,buscar(Oi),C1),
    obten_costo(KB,agarrar(Oi),C2),
    C is C1 + C2,
    !.

obten_costo(KB,Accion,C):-
    propiedades_de_objeto(KB,c,[_=>val(Costos)]),
    primera(Accion=>_,Costos,Accion=>C).

/*Obtiene la probabilidad de una accion dada*/

/* Caso especial ba es la fusion de buscar y agarrar*/
obten_probabilidad(KB,ba(Oi),P):-
    obten_probabilidad(KB,buscar(Oi),P1),
    obten_probabilidad(KB,agarrar(Oi),P2),
    P is P1 * P2,
    !.

obten_probabilidad(KB,Accion,P):-
    propiedades_de_objeto(KB,p,[_=>val(Probs)]),
    primera(Accion=>_,Probs,Accion=>P).

/*Obtine la recompensa de una acción dada*/

/* Caso especial ba es la fusion de buscar y agarrar*/
obten_recompensa(KB,ba(Oi),R):-
    obten_recompensa(KB,buscar(Oi),R1),
    obten_recompensa(KB,agarrar(Oi),R2),
    R is R1 + R2,
    !.

obten_recompensa(KB,Accion,R):-
    propiedades_de_objeto(KB,r,[_=>val(Recs)]),
    primera(Accion=>_,Recs,Accion=>R).

/* Obtiene el lugar correcto de un producto */
lugar_correcto_de_producto(KB,Producto,Lugar):-
    obten_objeto(KB,Producto,objeto(Producto, Clase, _, _)),
    relaciones_de_clase(KB,Clase,Props),
    filtra_por_atributo(Props,loc,[loc=>Lugar]).

/* Obtiene la posición del robot */
obten_posicion(KB,Pos):-
    propiedades_de_objeto(KB,golem,Props),
    filtra_por_atributo(Props,pos,[pos=>Pos]).

/* Obtiene las acciones pendientes del robot */
obten_acciones_pendientes(KB,Pendientes):-
    extension_de_propiedad_(KB,pendientes,[_=>val(Pendientes)]).

/*Cargando izquierdo*/
cargando_izq(KB,Carga):-
    ((relaciones_de_objeto(KB,izq,[tiene=>Carga]),!);
    Carga=[]).

/*Cargando derecho*/
cargando_der(KB,Carga):-
    ((relaciones_de_objeto(KB,der,[tiene=>Carga]),!);
    Carga=[]).

/*
 * Verifica si una lista es vacía.
 */
es_vacia(Lista) :- Lista = [].

/*
* Obtiene los objetos que golem sabe son inalcanzables
*/
obten_inalcanzables(KB,Inalcanzables):-
    extension_de_propiedad_(KB,inalcanzable,[_=>val(Inalcanzables)]).

/*
* Obtiene la primera observacion de los objetos marcados
*/
obten_primera_observacion(KB,PrimeraObservacion):-
    propiedades_de_objeto(KB,golem,Props),
    filtra_por_atributo(Props,primera_observacion,[primera_observacion=>val(PrimeraObservacion)]).

/*
* Obtiene los objetos incluidos en el reporte
*/
obtiene_objetos_en_reporte(KB,Objetos):-
    propiedades_de_objeto(KB,golem,Props),
    filtra_por_atributo(Props,objetos_en_reporte,[_=>val(Objetos)]).

/*
* Obtiene los productos marcados
*/
obten_productos_marcados(KB,Marcados):-
    extension_de_propiedad_(KB,marcado,Ext),
    filtra_por_valor(Ext,si,Filtrados),
    lista_de_atributos(Filtrados,Marcados).

/* Calcula costo de plan */
calcula_costo_de_plan(KB,[Accion|Resto],CostoTotal):-
    obten_costo(KB,Accion,C),
    calcula_costo_de_plan(KB,Resto,CostoResto),
    CostoTotal is CostoResto + C,
    !.

calcula_costo_de_plan(_,[],0).

/* Calcula probabilidad */
calcula_probabilidad_de_plan(KB,[Accion|Resto],ProbabilidadTolal):-
    obten_probabilidad(KB,Accion,P),
    calcula_probabilidad_de_plan(KB,Resto,ProbabilidadResto),
    ProbabilidadTolal is ProbabilidadResto * P,
    !.

calcula_probabilidad_de_plan(_,[],1.0).

/* Calcula los objetos inalcanzables en la realidad */
calcula_inalcanzables_en_realidad(KB,Inalcanzables):-
    extension_de_propiedad_(KB,alcanzable,Ext),
    filtra_por_valor(Ext,no,AtrValNoAlcanzables),
    lista_de_atributos(AtrValNoAlcanzables,Inalcanzables).
