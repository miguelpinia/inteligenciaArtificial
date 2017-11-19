/* Si ya no hay acciones pendientes ya terminamos*/
simulador(KB):-
    obten_acciones_pendientes(KB,[]),
    write('No hay más pendientes...'),nl,
    !.

simulador(KB):-
    diagnostico(KB,Diagnostico,KB2),
    write('Mi diagnostico a cerca de las acciones del asistente son:'),nl,
    imprime(Diagnostico),nl,
    decision(KB2,Decisiones),
    write('Mi decision es:'),nl,
    imprime(Decisiones),nl,
    planeacion(KB2,Decisiones,Plan),
    write('Mi plan es:'),nl,
    imprime(Plan),nl,
    /* Ya no nos importa el resultado, se ciclará hasta que termine con las acciones pendientes */
    simula_plan(KB2,Plan,_,NuevaKB),
    simulador(NuevaKB).

/*
* simula_plan(KB,Acciones,Ok,NuevaKB)
* Simula la lista de acciones Acciones, teniendo como punto de partida la KB
* puede terminar la ejecución o parar si el resultado de la ejecución de una acción falla.
*/

/* Si al ejecutar la primera acción todo sale bien seguimos con la ejecución*/
simula_plan(KB,[Accion|Resto],Ok,NuevaKB):-
    write('Accion: '),write(Accion),write(' Exito'),nl,
    simula_accion(KB,Accion,1,KB2),
    simula_plan(KB2,Resto,Ok,NuevaKB),
    !.

/* Si al ejecutar la primera accion fallamos, debemos terminar la ejecución del plan*/
simula_plan(KB,[Accion|_],0,NuevaKB):-
    simula_accion(KB,Accion,0,NuevaKB),
    write('Accion: '),write(Accion),write(' Falla'),nl,
    !.

/* Caso cuando termina con todas las acciones ya terminamos satisfactoriamente*/
simula_plan(KB,[],1,KB).

/*
* simula_accion(KB,Accion,Ok,NuevaKB)
* Después de simular Accion partiendo de KB se genera NuevaKB
* puede fallar en cuyo caso Ok=0 o no en cuyo caso Ok=1
*/

/*
* Cada acción tiene una probabilidad de éxito,
* Si un número aleatorio del [0,1) es mayor o igual a la probabilidad fallamos.
*/
simula_accion(KB,Accion,0,_):-
    obten_probabilidad(KB,Accion,P),
    random(0,1,R),
    P =< R,
    !.

/*
* Simula la acción de mover de Li a Lj
* Si la posición del robot no es Li fallará
* Si la posición Lj no es mostrador o algún estante fallará.
* Actualizala propiedad pos de golem
*/
simula_accion(KB,mover(Li,Lj),1,NuevaKB):-
    obten_posicion(KB,Li),
    extension_de_clase(KB,estante,Estantes),
    member(Lj,[mostrador|Estantes]),
    modifica_propiedad_de_objeto(KB,pos=>Lj,golem,NuevaKB),
    !.
/*
* Simula la acción de buscar(Oi),
* Actualiza la KB con lo observado
* Ok=1 Si el objeto se encontró
* Ok = 0 Si el objeto no se encontró
*/
simula_accion(KB,buscar(Oi),Ok,NuevaKB):-
    obten_posicion(KB,Pos),
    /* Aqui tenemos que actualizar las observaciones */
    obten_observaciones(KB,Observaciones),
    /* Eliminamos cualquier objeto que fué observado previamente*/
    delete(Observaciones,_=>Pos,Obs),
    /* Obtenemos el contenido del estante*/
    ((relaciones_de_objeto(KB,Pos,Rels),filtra_por_atributo(Rels,tiene,[tiene=>Contenido]),!);Contenido=[]),
    /* Borramos cualquier observación anterior de los objetos contenidos */
    filtra_por_atributos(Obs,Contenido,ABorrar),
    subtract(Obs,ABorrar,Obs2),
    /* Construimos observaciones */
    construye_observaciones(Contenido,Pos,NObs),
    /* Buscar los objetos que no son alcanzables e imprime un mensaje. */
    extension_de_propiedad_(KB, alcanzable, Objs),
    filtra_por_valor(Objs, no, NoAlcanzables),
    lista_de_atributos(NoAlcanzables, ObjsNoAlcanzables),
    nl, write('Los siguientes objetos no se pueden alcanzar: '), nl,
    imprime(ObjsNoAlcanzables), nl,
    /* Agregamos las observaciones*/
    append(NObs,Obs2,NuevasObservaciones),
    modifica_propiedad_de_objeto(KB,obs=>val(NuevasObservaciones),golem,KB2),
    /* Necesiamos saber si hay objetos que se deben reacomodar*/
    lista_de_objetos_a_reacomodar(KB2,Contenido,Pos,AReac),
    /* Silo en caso de que el objeto que buscamos está en lo que vimos y que no hay nada fuera de lugar estamos bien*/
    ((member(Oi,Contenido),Ok=1);Ok=0),
    /*Obtenemos la lista de tareas pendientes */
    propiedades_de_objeto(KB2,golem,Props),
    filtra_por_atributo(Props,pendientes,[_=>val(Pends)]),
    /* Elimina los objetos inalcanzables de las listas de acciones de pendientes. */
    union(Pends,AReac,NPendientes),
    findall(Pend,
            ((member(entregar(P), NPendientes), not(member(P, ObjsNoAlcanzables)), Pend = entregar(P));
             (member(reacomodar(P), NPendientes), not(member(P, ObjsNoAlcanzables)), Pend = reacomodar(P))),
            NuevosPendientes),
    /* Actualizamos la lista de pendientes*/
    modifica_propiedad_de_objeto(KB2,pendientes=>val(NuevosPendientes),golem,NuevaKB_),
    /* Lo marcamos como observado*/
    (agrega_propiedad_a_objeto(NuevaKB_,observado,Pos,NuevaKB);modifica_propiedad_de_objeto(NuevaKB_,observado,Pos,NuevaKB)),
    !.

simula_accion(KB,agarrar(Oi),1,NuevaKB):-
    obten_posicion(KB,Pos),
    obten_observaciones(KB,Observaciones),
    /*Verificamos que el objeto esta donde estamos parados*/
    filtra_por_atributo(Observaciones,Oi,[Oi=>Pos]),
    (/* Solo en caso de tener un brazo desocupado podemos agarrar*/
        (/* Brazo izquierdo libre*/
            extension_de_relacion_(KB,tiene,Ext),
            filtra_por_atributo(Ext,izq,[]),
            agrega_relacion_a_objeto(KB,tiene=>Oi,izq,KB2),
            elimina_relacion_de_objeto(KB2,tiene=>Oi,Pos,NuevaKB),
            !
        );
        (/* Brazo derecho libre*/
            extension_de_relacion_(KB,tiene,Ext),
            filtra_por_atributo(Ext,der,[]),
            agrega_relacion_a_objeto(KB,tiene=>Oi,der,KB2),
            elimina_relacion_de_objeto(KB2,tiene=>Oi,Pos,NuevaKB)
        )
    ),
    !.

simula_accion(KB,colocar(Oi),1,NuevaKB):-
    obten_posicion(KB,Pos),
    /* Buscamos dónde está el objeto Oi*/
    extension_de_relacion_(KB,tiene,Ext),
    /* Un brazo solo puede cargar un objeto a la vez */
    filtra_por_valor(Ext,Oi,[Brazo=>[Oi]]),
    /* Solo en caso de tener en un brazo podemos colocar*/
    (Brazo = izq ; Brazo = der),
    (
        (/* Si estamos en el mostrador lo estamos entregando */
            Pos=mostrador,
            /* Lo eliminamos de la KB*/
            elimina_objeto(KB,Oi,KB2_),
            /* Obtenemos las actividades pendientes*/
            propiedades_de_objeto(KB2_,golem,Props),
            filtra_por_atributo(Props,pendientes,[_=>val(Pends)]),
            /* Eliminamos la actividad pendiente entregar(Oi) */
            delete(Pends,entregar(Oi),NuevosPendientes),
            /* Actuaizamos los pendientes en la KB */
            modifica_propiedad_de_objeto(KB2_,pendientes=>val(NuevosPendientes),golem,KB2),
            !
        );
        (/* Si estamos colocando en un estante podríamos haber terminando de reacomodar(Oi)*/
            dif(mostrador,Pos),
            /* Agrega la relacion tiene=>Oi al estante donde estamos */
            agrega_relacion_a_objeto(KB,tiene=>Oi,Pos,KB2_),
            /* Obtenemos las actividades pendientes*/
            propiedades_de_objeto(KB2_,golem,Props),
            filtra_por_atributo(Props,pendientes,[_=>val(Pends)]),
            /* Eliminamos la actividad pendiente entregar(Oi) */
            delete(Pends,reacomodar(Oi),NuevosPendientes),
            /* Actuaizamos los pendientes en la KB */
            modifica_propiedad_de_objeto(KB2_,pendientes=>val(NuevosPendientes),golem,KB2__),
            /* Agregamos el objeto a los movidos */
            obten_movidos(KB2__,MovidosPorGolem),
            fusiona([Oi|MovidosPorGolem],NuevosMovidos),
            modifica_propiedad_de_objeto(KB2__,movidos=>val(NuevosMovidos),golem,KB2)
        )
    ),
    /* Finalmente eliminamos la relacion tiene=>Oi del brazo izquierdo */
    elimina_relacion_de_objeto(KB2,tiene=>Oi,Brazo,NuevaKB),
    !.

/*
* En cualquier otro caso regresamos la base inicial y Ok=0.
*/
simula_accion(KB,_,0,KB).

/*
* lista_de_objetos_a_reacomodar(KB,Objetos,Pos,ListaReacomodar)
* Los Objetos que estan en Pos necesitan reacomodarse, y las acciones pendientes
* generadas a partir de ellos estan en ListaReacomodar
*/
lista_de_objetos_a_reacomodar(KB,[Objeto|Resto],Pos,[reacomodar(Objeto)|RestoAReacomodar]):-
    lugar_correcto_de_producto(KB,Objeto,Lugar),
    dif(Pos,Lugar),
    lista_de_objetos_a_reacomodar(KB,Resto,Pos,RestoAReacomodar),
    !.

lista_de_objetos_a_reacomodar(KB,[Objeto|Resto],Pos,RestoAReacomodar):-
    lugar_correcto_de_producto(KB,Objeto,Lugar),
    Pos = Lugar,
    lista_de_objetos_a_reacomodar(KB,Resto,Pos,RestoAReacomodar),
    !.

lista_de_objetos_a_reacomodar(_,[],_,[]).

/*
* construye_observaciones(Contenido,Pos,Observaciones)
* Dada una lista de productos [p1,p2,...,pn]
* Construye una lista [p1=>Pos,p2=>Pos,...,pn=>Pos]
*/
construye_observaciones([Obj|Resto],Pos,[Obj=>Pos|RestoObs]):-
    construye_observaciones(Resto,Pos,RestoObs),
    !.

construye_observaciones([],_,[]).
