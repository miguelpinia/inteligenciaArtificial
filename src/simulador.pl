/* Si ya no hay acciones pendientes ya terminamos*/
simulador(KB):-
    obten_acciones_pendientes(KB,[]),
    %imprime(KB),nl,
    nl,write('No hay más pendientes...'),nl,nl,
    !.

simulador(KB):-
    diagnostico(KB,Diagnostico,KB2),
    decision(KB2,Decisiones),
    planeacion(KB2,Decisiones,Plan),
    ((not(es_vacia(Diagnostico)),
      write('Mi diagnóstico acerca de las acciones del asistente son:'),nl,
      imprime(Diagnostico),nl);
     (write('No tengo información suficiente para realizar un diagnóstico.'),nl)),
    ((not(es_vacia(Decisiones)),
      write('Mi decisión es:'),nl,
      imprime(Decisiones),nl,
      write('Mi plan es:'),nl,
      imprime(Plan),nl);
     (write('No tengo ínformación suficiente para tomar decisiones y diseñar un plan.'),nl)),
        /* Ya no nos importa el resultado, se ciclará hasta que termine con las acciones pendientes */
    simula_plan(KB2,Plan,_,NuevaKB),
    simulador(NuevaKB), !.

/*
* simula_plan(KB,Acciones,Ok,NuevaKB)
* Simula la lista de acciones Acciones, teniendo como punto de partida la KB
* puede terminar la ejecución o parar si el resultado de la ejecución de una acción falla.
*/

/* Si al ejecutar la primera acción todo sale bien seguimos con la ejecución*/
simula_plan(KB,[Accion|Resto],Ok,NuevaKB):-
    write('Accion: '),write(Accion),nl,
    simula_accion(KB,Accion,AOk,KB2,1),
    %imprime(KB2),nl,
    (
        (
            AOk=1,
            nl,write('Exito'),nl,
            simula_plan(KB2,Resto,Ok,NuevaKB),
            !
        );
        (
            Ok=0,
            nl,write('Falla'),nl,nl,
            NuevaKB = KB2
        )
    ),
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
simula_accion(KB,Accion,Ok,KB,1):-
    obten_probabilidad(KB,Accion,P),
    random(0.0,1.0,R),
    P =< R,
    write('Falla por probabilidad'),nl,
    Ok=0,
    !.

/*
* Simula la acción de mover de Li a Lj
* Si la posición del robot no es Li fallará
* Si la posición Lj no es mostrador o algún estante fallará.
* Actualizala propiedad pos de golem
*/
simula_accion(KB,mover(Li,Lj),1,NuevaKB,_):-
    obten_posicion(KB,Li),
    extension_de_clase(KB,estante,Estantes),
    member(Lj,[mostrador|Estantes]),
    modifica_propiedad_de_objeto(KB,pos=>Lj,golem,NuevaKB),
    !.

/*
* Simula la acción de buscar(Oi),
* Actualiza la KB con lo observado
* Ok=1 Si el objeto se encontró
* Ok = 0 Si el objeto no se encontró o se moificaron las metas por objetos inalcanzables
*/
simula_accion(KB,buscar(Oi),Ok,NuevaKB,_):-
    /* Obtenemos la posición actual del robot */
    obten_posicion(KB,Pos),
    /* Aqui tenemos que actualizar las observaciones */
    obten_observaciones(KB,Observaciones),
    /* Obtenemos el contenido del estante*/
    ((relaciones_de_objeto(KB,Pos,Rels),filtra_por_atributo(Rels,tiene,[tiene=>Contenido]),!);Contenido=[]),
    /* Para avisar si hay go que no sabía veo lo que me pasaron en el reporte*/
    obtiene_objetos_en_reporte(KB,ObjetosEnReporte),
    /* Obtengo los objetos que NO estan en el reporte*/
    subtract(Contenido,ObjetosEnReporte,NoConocidos),
    /* De esos, solo los que no he marcado los tengo que reportar */
    obten_productos_marcados(KB,Marcados),
    /* borro los que ya marqué */
    subtract(NoConocidos,Marcados,NuevosDesconocidos),
    (
        (/* Hay algun nuevo objeto que no conocía realmente?*/
            dif(NuevosDesconocidos,[]),
            /* Reporto */
            nl,write('Me dí cuenta que también hay:'),nl,
            imprime(NuevosDesconocidos),nl,
            !
        );
        (/* Si no no hago nada */
            true
        )
    ),
    /* Productos que no estan en su lugar */
    /* De los productos en Contenido que no han sido marcados son con los que trabajaremos */
    subtract(Contenido,Marcados,NuevosVistos),
    /* Calculamos los no alcanzables y vemos cuales de los nuevos no alcanzamos */
    calcula_inalcanzables_en_realidad(KB,Inalcanzables),
    intersection(NuevosVistos,Inalcanzables,NuevosInalcanzables),
    (
        (/* Si hay nuevos inalcanzables los debo reportar, quitarlos de las metas y agragarlos a lo inalcanzable */
            dif(NuevosInalcanzables,[]),
            nl,write('Me dí cuenta que no puedo alcanzar esto:'),nl,
            imprime(NuevosInalcanzables),nl,
            /* Agrego en la propiedad inalcanzable de golem */
            obten_inalcanzables(KB,InalcanzablesConocidos),
            append(InalcanzablesConocidos,NuevosInalcanzables,NuevosInalcanzablesConocidos),
            modifica_propiedad_de_objeto(KB,inalcanzable=>val(NuevosInalcanzablesConocidos),golem,KB2),
            /* Obtengo los pendientes */
            obten_acciones_pendientes(KB2,Pendientes),
            /* Obtengo los objetos que estan actualmente en metas*/
            objetos_validos(Pendientes,ObjetosEnPendientes),
            /* Obengo los nuevos inalcanzables que hay en los pendientes*/
            intersection(NuevosInalcanzables,ObjetosEnPendientes,NuevosInalcanzablesEnPendientes),
            (
                (/* Si hay inalcanzables en las metas debo eliminarlas y además debo fallar */
                    dif(NuevosInalcanzablesEnPendientes,[]),
                    elimina_de_pendientes(Pendientes,NuevosInalcanzablesEnPendientes,NuevosPendientes),
                    /* Calculo los que borré*/
                    subtract(Pendientes,NuevosPendientes,PendientesAEliminar),
                    nl,write('Debo eliminar de mis pendientes:'),nl,
                    imprime(PendientesAEliminar),nl,
                    modifica_propiedad_de_objeto(KB2,pendientes=>val(NuevosPendientes),golem,KB3),
                    CambioEnMetas=1,
                    !
                );
                (/* En otro caso solo agregué lso nuevos inalcanzables a la propiedad de golem y no cambie metas */
                    KB3 = KB2,
                    CambioEnMetas=0
                )
            ),
            !
        );
        (/*Sino no hay cambio en la base en este punto y no cambie metas */
            KB3 = KB,
            CambioEnMetas=0
        )
    ),
    /* Registramos los objetos vistos por primera vez y su posisión original */
    marca_productos(KB3,Contenido,Pos,KB4),
    /* La lista de reacomodos debe contener solamente a los nuevos que podemos alcanzar */
    subtract(NuevosVistos,NuevosInalcanzables,ObjetosAReacomodar),
    /* Consruimos la lista de reacomodos*/
    lista_de_objetos_a_reacomodar(KB4,ObjetosAReacomodar,Pos,ListaDeReacomodos),
    (
        (/* Si la lista de reacomodos es no vacía debo reportar los pendientes a agregar y agregar en los pendientes de golem */
            dif(ListaDeReacomodos,[]),
            nl,write('Debo agregar a mis pendientes:'),nl,
            imprime(ListaDeReacomodos),nl,
            /* Los nuevos pendientes serán los pendientes en la base anterior mas la lista de reacomodos*/
            obten_acciones_pendientes(KB4,PendientesActualizados),
            append(PendientesActualizados,ListaDeReacomodos,NuevosPendientesDefinitivos),
            modifica_propiedad_de_objeto(KB4,pendientes=>val(NuevosPendientesDefinitivos),golem,KB5),
            !
        );
        (
            KB5=KB4
        )
    ),
    construye_observaciones(Contenido,Pos,NuevasObservaciones),
    append(NuevasObservaciones,Observaciones,ObservacionesSinFusionar),
    fusiona(ObservacionesSinFusionar,ObservacionesDefinitivas),
    modifica_propiedad_de_objeto(KB5,obs=>val(ObservacionesDefinitivas),golem,KB6),
    (
        (
            (
                (
                    not(member(Oi,Contenido)),
                    nl,write('No vi:'),nl,write(Oi),nl,nl,
                    !
                );
                CambioEnMetas=1
            ),
            Ok=0,
            !
        );
        (
            Ok = 1
        )
    ),
    (/* Por último marcamos el estante observado*/
        (/* Si aún no lo esta */
            agrega_propiedad_a_objeto(KB6,observado,Pos,NuevaKB)
        );
        (/* En caso contrario ya terminamos */
            NuevaKB = KB6
        )
    ),
    !.

simula_accion(KB,agarrar(Oi),1,NuevaKB,_):-
    obten_posicion(KB,Pos),
    obten_observaciones(KB,Observaciones),
    /*Verificamos que el objeto esta donde estamos parados*/
    filtra_por_atributo(Observaciones,Oi,[Oi=>Pos]),
    (/* Solo en caso de tener un brazo desocupado podemos agarrar*/
        (/* Brazo izquierdo libre*/
            extension_de_relacion_(KB,tiene,Ext),
            filtra_por_atributo(Ext,izq,[]),
            agrega_relacion_a_objeto(KB,tiene=>Oi,izq,KB2),
            elimina_relacion_de_objeto(KB2,tiene=>Oi,Pos,KB3),
            /* Modifico la creencia*/
            obten_creencias(KB,Creencias),
            fusiona([Oi=>izq|Creencias],NuevasCreencias),
            modifica_propiedad_de_objeto(KB3,cree=>val(NuevasCreencias),golem,KB4),
            /* Modifico la observacion*/
            obten_observaciones(KB,Observaciones),
            fusiona([Oi=>izq|Observaciones],NuevasObservaciones),
            modifica_propiedad_de_objeto(KB4,obs=>val(NuevasObservaciones),golem,KB5),
            /* Modifico los movidos ya que una vez tomado no importa lo que el asistente hizo YA SE QUE YO LO TENGO*/
            obten_movidos(KB,Movidos),
            union([Oi],Movidos,NuevosMovidos),
            modifica_propiedad_de_objeto(KB5,movidos=>val(NuevosMovidos),golem,NuevaKB),
            !
        );
        (/* Brazo derecho libre*/
            extension_de_relacion_(KB,tiene,Ext),
            filtra_por_atributo(Ext,der,[]),
            agrega_relacion_a_objeto(KB,tiene=>Oi,der,KB2),
            elimina_relacion_de_objeto(KB2,tiene=>Oi,Pos,KB3),
            /* Modifico la creencia*/
            obten_creencias(KB,Creencias),
            fusiona([Oi=>der|Creencias],NuevasCreencias),
            modifica_propiedad_de_objeto(KB3,cree=>val(NuevasCreencias),golem,KB4),
            /* Modifico la observacion*/
            obten_observaciones(KB,Observaciones),
            fusiona([Oi=>der|Observaciones],NuevasObservaciones),
            modifica_propiedad_de_objeto(KB4,obs=>val(NuevasObservaciones),golem,KB5),
            /* Modifico los movidos ya que una vez tomado no importa lo que el asistente hizo YA SE QUE YO LO TENGO*/
            obten_movidos(KB,Movidos),
            union([Oi],Movidos,NuevosMovidos),
            modifica_propiedad_de_objeto(KB5,movidos=>val(NuevosMovidos),golem,NuevaKB)
        )
    ),
    !.

simula_accion(KB,colocar(Oi),1,NuevaKB,_):-
    obten_posicion(KB,Pos),
    /* Buscamos dónde está el objeto Oi*/
    extension_de_relacion_(KB,tiene,Ext),
    /* Un brazo solo puede cargar un objeto a la vez */
    filtra_por_valor(Ext,Oi,[Brazo=>[Oi]]),
    /* Solo en caso de tener en un brazo podemos colocar*/
    ((Brazo = izq ,!); Brazo = der),
    (
        (/* Si estamos en el mostrador lo estamos entregando */
            Pos=mostrador,
            /* Lo eliminamos de la KB*/
            elimina_objeto(KB,Oi,KB2_),
            /* Obtenemos las actividades pendientes*/
            propiedades_de_objeto(KB2_,golem,Props),
            filtra_por_atributo(Props,pendientes,[_=>val(Pends)]),
            /* Eliminamos la actividad pendiente entregar(Oi) */
            delete(Pends,entregar(Oi),NPendientes),
            delete(NPendientes,reacomodar(Oi),NuevosPendientes),
            /* Actuaizamos los pendientes en la KB */
            modifica_propiedad_de_objeto(KB2_,pendientes=>val(NuevosPendientes),golem,NuevaKB),
            !
        );
        (/* Si estamos colocando en un estante podríamos haber terminando de reacomodar(Oi)*/
            dif(mostrador,Pos),
            /* Agrega la relacion tiene=>Oi al estante donde estamos */
            agrega_relacion_a_objeto(KB,tiene=>Oi,Pos,KB2),
            /* Quitamos la relación tiene=>Oi del brazo en el que esté */
            elimina_relacion_de_objeto(KB2,tiene=>Oi,Brazo,KB3),
            /* Obtenemos las actividades pendientes*/
            propiedades_de_objeto(KB3,golem,Props),
            filtra_por_atributo(Props,pendientes,[_=>val(Pends)]),
            /* Eliminamos la actividad pendiente entregar(Oi) */
            delete(Pends,reacomodar(Oi),NuevosPendientes),
            /* Actuaizamos los pendientes en la KB */
            modifica_propiedad_de_objeto(KB3,pendientes=>val(NuevosPendientes),golem,KB4),
            /* Agregamos el objeto a los movidos */
            obten_movidos(KB4,MovidosPorGolem),
            fusiona([Oi|MovidosPorGolem],NuevosMovidos),
            modifica_propiedad_de_objeto(KB4,movidos=>val(NuevosMovidos),golem,NuevaKB)
        )
    ),
    !.

/*
* En cualquier otro caso regresamos la base inicial y Ok=0.
*/
simula_accion(KB,_,0,KB,_).

/*
* lista_de_objetos_a_reacomodar(KB,Objetos,Pos,ListaReacomodar)
* Los Objetos que estan en Pos necesitan reacomodarse, y las acciones pendientes
* generadas a partir de ellos estan en ListaReacomodar
*/
lista_de_objetos_a_reacomodar(KB,[Objeto|Resto],Pos,ListaDeReacomodos):-
    lugar_correcto_de_producto(KB,Objeto,Lugar),
    (
        (
            dif(Pos,Lugar),
            lista_de_objetos_a_reacomodar(KB,Resto,Pos,RestoAReacomodar),
            ListaDeReacomodos = [reacomodar(Objeto)|RestoAReacomodar],
            !
        );
        (
            Pos = Lugar,
            lista_de_objetos_a_reacomodar(KB,Resto,Pos,RestoAReacomodar),
            ListaDeReacomodos = RestoAReacomodar
        )
    ),
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


/*
* elimina_pendientes_con_objetos(Pendientes,Objetos,NuevosPendentes)
* Elimina las acciones pendientes que involucaran los objetos en la lista Objetos
*/

elimina_pendientes_con_objetos(Pendientes,[Objeto|Resto],NuevosPendentes):-
    delete(Pendientes,entregar(Objeto),Pendientes2),
    delete(Pendientes2,reacomodar(Objeto),Pendientes3),
    elimina_pendientes_con_objetos(Pendientes3,Resto,NuevosPendentes),
    !.

elimina_pendientes_con_objetos(Pendientes,[],Pendientes).

marca_productos(KB,[Producto|Resto],Pos,NuevaKB):-
    propiedades_de_objeto(KB,Producto,Props),
    (
        (/* En caso de no estar marcado */
            not(member(marcado,Props)),
            /* Lo marco */
            agrega_propiedad_a_objeto(KB,marcado,Producto,NKB1),
            /* Agrego a la propiedad primera_observacion de golem */
            obten_primera_observacion(NKB1,PrimeraObservacion),
            modifica_propiedad_de_objeto(NKB1,primera_observacion=>val([Producto=>Pos|PrimeraObservacion]),golem,NKB2),
            /* Marco los demás */
            marca_productos(NKB2,Resto,Pos,NuevaKB),
            !
        );
        (/* En caso de haber sido marcado nos seguimos */
            marca_productos(KB,Resto,Pos,NuevaKB)
        )
    ),
    !.

marca_productos(KB,[],_,KB).

/*
 * Establece los productos que debe estar en el estante que se está
 * observando, además de actualizar la posición de golem a dicho
 * estante.
 */
establece_estante_observado(KB, Estante, Productos, NuevaKB) :-
    modifica_propiedad_de_objeto(KB, pos=>Estante, golem, KB1),
    modifica_propiedad_de_objeto(KB1, tiene=>Productos, Estante, KB2),
    obten_observaciones(KB1, Obs),
    delete(Obs, _=>Estante, Obs1),
    filtra_por_atributos(Obs1, Productos, ABorrar),
    subtract(Obs1, ABorrar, Obs2),
    construye_observaciones(Productos, Estante, NObs),
    append(NObs, Obs2, Observaciones),
    modifica_propiedad_de_objeto(KB2, obs=>val(Observaciones), golem, NuevaKB),
    !.

/*
 * Prepara la base de conocimiento para establecer el estado de un
 * estante, observarlo y actualizar el estado interno de golem.
 */
prepara_kb_obs(File, Estante, Productos, NuevaKB) :-
    open_kb(File, KB),
    establece_estante_observado(KB, Estante, Productos, KB1),
    lista_de_objetos_a_reacomodar(KB1,Productos,Estante,AReac_),
    extension_de_propiedad_(KB1, alcanzable, Objs),
    filtra_por_valor(Objs, no, NoAlcanzables),
    lista_de_atributos(NoAlcanzables, ObjsNoAlcanzables),
    intersection(ObjsNoAlcanzables,Productos,NoAlcanzablesAqui),
    ((dif(NoAlcanzablesAqui,[]),
      obten_inalcanzables(KB,Inalcanzables),
      subtract(NoAlcanzablesAqui,Inalcanzables,InalcanzablesNuevos),
      dif(InalcanzablesNuevos,[]),
      append(Inalcanzables,InalcanzablesNuevos,NuevosInalcanzables_),
      list_to_set(NuevosInalcanzables_,NuevosInalcanzables),
      modifica_propiedad_de_objeto(KB1,inalcanzables=>val(NuevosInalcanzables),golem,KB2),
      !);
     (KB2=KB1)),
    ((dif(NoAlcanzablesAqui,[]),
      obten_acciones_pendientes(KB2,Pendientes),
      objetos_validos(Pendientes,ObjetosEnPendientes),
      intersection(NoAlcanzablesAqui,ObjetosEnPendientes,NoAlcanzablesAquiEnPendientes),
      dif(NoAlcanzablesAquiEnPendientes,[]),
      elimina_de_pendientes(Pendientes,ObjetosEnPendientes,NuevosPendentes),
      modifica_propiedad_de_objeto(KB2,pendientes=>val(NuevosPendentes),golem,KB3),
      !);
     (KB3=KB2)),
    propiedades_de_objeto(KB3,golem,Props),
    filtra_por_atributo(Props,pendientes,[_=>val(Pends)]),
    subtract(AReac_,NoAlcanzablesAqui,AReac),
    union(Pends,AReac,Pendientes),
    modifica_propiedad_de_objeto(KB3,pendientes=>val(Pendientes),golem,NuevaKB_),
    (agrega_propiedad_a_objeto(NuevaKB_,observado,Estante,NuevaKB);modifica_propiedad_de_objeto(NuevaKB_,observado,Estante,NuevaKB)),
    !.

/*
 * Realiza la simulación de un paso, utilizando una base de
 * conocimiento template que está en FILE, actualizando los PRODUCTOS
 * en ESTANTE y a partir de ahí, realizar un diagnóstico, toma de
 * decisión y un plan de acción.
 * ?- prueba_simulacion_un_paso('ejercicios/ejercicio1.txt',drink_shelf,[coca,heineken],Diag,Decs,Plan).
 */
prueba_simulacion_un_paso(File, Estante, Productos, Diag, Decs, Plan) :-
    prepara_kb_obs(File, Estante, Productos, KB),
    write('Mi diagnóstico es: '), nl,nl,
    diagnostico(KB, Diag, KB1),
    imprime(Diag),nl,nl,
    decision(KB1, Decs),
    write('Mi decisión es: '), nl,nl,
    imprime(Decs),nl,nl,
    planeacion(KB1, Decs, Plan),
    write('Mi plan es: '),nl,nl,
    imprime(Plan), nl, nl.
