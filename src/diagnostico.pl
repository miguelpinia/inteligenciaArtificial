/*
* diagnostico(KB,Diagnostico,NuevaKB)
* Dada una KB actual genera el diagnóstico Diagnostico de las actividades hechas por el asistente humano,
* NuevaKB ya tiene el nuevo estado de creencia.
*/
diagnostico(KB,Diagnostico,NuevaKB):-
    /* Obtenemos las creencias del robot*/
    obten_creencias(KB,Creencias),
    /* Obtenemos las observaciones del robot*/
    obten_observaciones(KB,Observaciones),
    /* Fusionamos creencias y observaciones */
    append(Observaciones,Creencias,ObsCre),
    fusiona(ObsCre,ObsCreFus),
    /*
    * Eliminamos objetos no observados en caso de que ya se hayan observado todos los estantes,
    * Más adelante podremos reagregar un objeto simulando el caso en el que nos mueven las cosas de lugar
    */
    elimina_objetos_no_observados(KB,Observaciones,ObsCreFus,NuevasCreencias,KB2),
    /*
    * Obtenemos los estantes observados para decidir que objetos vamos a diagnosticar
    */
    obten_estantes_observados(KB2,EstantesObservados),
    /*
    * Obtenemos los objetos a diagnosticar
    * OJO: Si ya se han observado todos los estantes, en este punto deja de haber busquedas ya que en tal caso,
    * ya se habrán eliminado los objetos no observados.
    * Si es así ObjetosADiagnosticar sempre será [] que es un estado meta para la busqueda.
    */
    obten_objetos_a_diagnosticar(NuevasCreencias,Observaciones,EstantesObservados,ObjetosADiagnosticar),
    /* Hacemos el diagnóstico de los objetos requeridos */
    diag(KB2,[node(0,null,ObjetosADiagnosticar)],[],Meta),
    append(Meta,NuevasCreencias,DiagnosticoYNuevasCreencias),
    fusiona(DiagnosticoYNuevasCreencias,SiguientesCreencias),
    /*
    * Generamos el diagnostico en base a las SiguientesCreencias y lo que ha reacomodado el robot
    * Los objetos reacomodados por el robot NO APARECEN EN EL DIAGNOSTICO
    */
    %extension_de_clase(KB2,producto,Productos),
    %obten_movidos(KB2,MovidosPorGolem),
    %write('XXXXXXXXXXXXXX MOVIDOS XXXXXxxx'),write(MovidosPorGolem),nl,
    %subtract(Productos,MovidosPorGolem,NoMovidosPorGolem),
    %write('XXXXXXXXXXXXXX NO MOVIDOS XXXXXxxx'),write(NoMovidosPorGolem),nl,
    %genera_diagnostico(SiguientesCreencias,NoMovidosPorGolem,Diagnostico),
    /* Genera_diagnostico del asistente*/
    /* Primero obtenemos las primeras observaciones de los productos */
    propiedades_de_objeto(KB2,golem,Props),
    filtra_por_atributo(Props,primera_observacion,[primera_observacion=>val(Primeras)]),
    append(Primeras,SiguientesCreencias,CreenciasAReportar),
    fusiona(CreenciasAReportar,CreenciasAReportarFusionadas),
    /*Solo debemos reportar en el diagnostico lo que nos dijeron en el reporte*/
    obtiene_objetos_en_reporte(KB,ObjetosEnReporte),
    filtra_por_atributos(CreenciasAReportarFusionadas,ObjetosEnReporte,CreenciasAReportarFusionadasYFiltradas),
    genera_diagnostico(KB2,CreenciasAReportarFusionadasYFiltradas,Diagnostico),
    /* Actualiza la KB para tener las SiguientesCreencias */
    modifica_propiedad_de_objeto(KB2,cree=>val(SiguientesCreencias),golem,NuevaKB).

/*
* diag(KB,Abiertos,Cerrados,Meta)
* dada la KB los nodos Abiertos y los nodos Cerrados, encuentra la meta Meta
* Hace el diagnostico por medio de un DFS.
* estados se representan como listas objeto=>lugar
* [coca=>bebidas,mundet=>_]
* Los estados meta son listas donde todos sus objetos tienen un lugar concreto, es decir no puede haber
* elementos del tipo objeto=>_
* para testear se hace la pregunta del tipo not(member(_=>'#',Estado)),
* Se supone que no hay lugar con identificador '#'
* Se utiliza el funtor node(Id,ParentId,Estado) para representar el estado Estado que tiene como padre al nodo con id ParentId
*/

/*
* Caso de que ya encontramos la meta
*/
diag(_,[node(_,_,Estado)|_],_,Estado):-
    not(member(_=>'#',Estado)),
    !.

/*
* Caso en que el siguiente nodo abierto no es meta
*/
diag(KB,[node(Id,IdPadre,Estado)|RestoNodos],Cerrados,Meta):-
    findall(Id,member(node(Id,_,_),[node(Id,IdPadre,Estado)|RestoNodos]),IdsAbiertos),
    findall(Id,member(node(Id,_,_),Cerrados),IdsCerrados),
    append(IdsAbiertos,IdsCerrados,Ids),
    max_list(Ids,MaxId),
    suc_diag(KB,MaxId,node(Id,IdPadre,Estado),Sucesores),
    append(Sucesores,RestoNodos,NuevosAbiertos),
    diag(KB,NuevosAbiertos,[node(Id,IdPadre,Estado),Cerrados],Meta),
    !.

/*
* El diagnostico para cuando ya no hay más nodos que abrir es que no hay solución
* Por la definición de los estados esto nunca debería pasar.
*/
diag(_,[],_,udf).

/*
* Genera los sucesores de Estado en orden de cercanía al lugar donde debería estar dentro de los estantes NoObservados
* suc_diag(LastId,NoObservados,Id,Estado,Sucesores)
*/

suc_diag(KB,LastId,node(Id,_,Estado),Sucesores):-
    /* Obtenemos los estantes no observados */
    obten_estantes_no_observados(KB,NoObservados),
    /* Agregamos los sucesores por prioridad de distancia al lugar correcto*/
    agrega_sucesores(KB,[],Id,Estado,LastId,NoObservados,NPQ),
    /*Obtenemos la lista de sucesores ya ordenada*/
    lista_de_valores(NPQ,Sucesores).

/*
* agrega_sucesores(KB,PQ,IdPadre,Estado,UltimoIDUsado,NoObservados,NPQ)
*
*/
agrega_sucesores(KB,PQ,IdPadre,Estado,UltimoIDUsado,[Lugar|Resto],NPQ):-
    primera(_=>'#',Estado,Producto=>_),
    lugar_correcto_de_producto(KB,Producto,LugarCorrecto),
    obten_costo(KB,mover(LugarCorrecto,Lugar),Key),
    SiguenteID is UltimoIDUsado +1,
    reemplaza(Estado,Producto=>_,Producto=>Lugar,SiguenteEstado),
    agrega_pq(PQ,Key=>node(SiguenteID,IdPadre,SiguenteEstado),PQ2),
    agrega_sucesores(KB,PQ2,IdPadre,Estado,SiguenteID,Resto,NPQ).

/* Si ya no hay nada que agregar ya acabamos*/
agrega_sucesores(_,PQ,_,_,_,[],PQ).

/*
* Genera el diagnosico en base a las creencias que se quireren reportar
*
*/
genera_diagnostico(KB,CreenciasAReportar,Diagnostico):-
    lista_de_valores(CreenciasAReportar,Estantes_),
    list_to_set(Estantes_,Estantes),
    estantes_ordenados(KB,mostrador,Estantes,EstantesOrdenados),
    genera_acciones(EstantesOrdenados,mostrador,CreenciasAReportar,Diagnostico),
    !.

/*
* Ordena los estantes para tener el mejor recorrido del humano
* El inicio es el punto de partida,
* el siguiente eleento será el mas cercano
*/
estantes_ordenados(_,_,[],[]):-!.

estantes_ordenados(KB,Inicio,Estantes,[EstanteMasCercano|RestoOrdenado]):-
    dif(Estantes,[]),
    genera_llaves(KB,Inicio,Estantes,ListaPQ),
    agrega_pq_muchos([],ListaPQ,[_=>EstanteMasCercano|_]),
    delete(Estantes,EstanteMasCercano,Resto),
    estantes_ordenados(KB,EstanteMasCercano,Resto,RestoOrdenado).

/*
* Genera la lista de llaves basadas en el costo de moverse del inicio al cada lugar
* genera_llaves(Inicio,Estantes,ListaPQ)
*/
genera_llaves(KB,Inicio,[Estante|Resto],[C=>Estante|RestoConLlave]):-
    obten_costo(KB,mover(Inicio,Estante),C),
    genera_llaves(KB,Inicio,Resto,RestoConLlave),
    !.

genera_llaves(_,_,[],[]).

/* Genera las acciones del diagnostico */

genera_acciones([Estante|Resto],Anterior,Creencias,Acciones):-
    filtra_por_valor(Creencias,Estante,EnEstante),
    lista_de_atributos(EnEstante,Objetos),
    genera_colocar(Objetos,ListaDeColocar),
    genera_acciones(Resto,Estante,Creencias,AccionesEnResto),
    append([mover(Anterior,Estante)|ListaDeColocar],AccionesEnResto,Acciones),
    !.

genera_acciones([],_,_,[]).

/*
* Genera la lista de colocar(Oi) de los objetos Oi en Objetos
*/
genera_colocar([Objeto|Resto],[colocar(Objeto)|ColocarResto]):-
    genera_colocar(Resto,ColocarResto),!.

genera_colocar([],[]).


/*Elimina los objetos no observados siempre y cuando ya se hayan observado los 3 estantes*/
elimina_objetos_no_observados(KB,Observaciones,Creencias,NuevasCreencias,NuevaKB):-
    obten_estantes_no_observados(KB,[]),
    /*Obtenemos los objetos observados*/
    lista_de_atributos(Observaciones,ObjetosObservados),
    /*Las nuevas creencias serán solamente los objetos que se han observado
    * Pueden habernos movido de lugar un objeto o que realmente no esté en los estantes
    * Si está en otro estante eventualmente lo agragará cuando vuelva a observar el estante
    */
    filtra_por_atributos(Creencias,ObjetosObservados,NuevasCreencias),
    /*Ahora eliminamos cualquier pendiente que tenga que ver con los que borramos*/
    /* Obtenemos las creecias eliminadas*/
    subtract(Creencias,NuevasCreencias,CreenciasEliminadas),
    /* Obtenemos los objetos relacionadas a la creencia */
    lista_de_atributos(CreenciasEliminadas,ObjetosEliminados),
    /* Obtenemos los pendientes de golem */
    obten_acciones_pendientes(KB,Pendientes),
    /* Eliminamos los pendientes de los objetos que eliminamos*/
    elimina_de_pendientes(Pendientes,ObjetosEliminados,NuevosPendientes),
    (
        ( /* Caso en el que no quitamos nada */
            length(Pendientes,L),
            length(NuevosPendientes,L),
            NuevaKB = KB,
            !
        );
        (/* Caso en el que quitamos metas, hay que avisar...*/
            write('No encontré los objetos en las siguientes metas, así que las eliminaré de los pendientes:'),nl,
            subtract(Pendientes,NuevosPendientes,Eliminadas),
            imprime(Eliminadas),nl,
            (
                (
                    NuevosPendientes=[],
                    nl,write('Se han eliminado todos los pendientes...'),nl,nl,
                    !
                );
                (
                    true
                )
            ),
            /* modificamos la base con los nuevos pendientes */
            modifica_propiedad_de_objeto(KB,pendientes=>val(NuevosPendientes),golem,NuevaKB)
        )
    ),
    !.

/*Si no se han observado todos los estantes, no se pueden liminar objetos de las creencias*/
elimina_objetos_no_observados(KB,_,Creencias,Creencias,KB).

/*
* elimina_de_pendientes(Pendientes,Objetos,NuevosPendientes)
*/
elimina_de_pendientes([Pendiente|Resto],Objetos,NuevosPendientes):-
    elimina_de_pendientes(Resto,Objetos,RestoPendientes),
    ((Pendiente=entregar(Oi),!);Pendiente=reacomodar(Oi)),
    (
        (
            member(Oi,Objetos),
            NuevosPendientes = RestoPendientes,
            !
        );
        (
            not(member(Oi,Objetos)),
            NuevosPendientes = [Pendiente|RestoPendientes]
        )
    ),
    !.

elimina_de_pendientes([],_,[]).

/*
* obten_objetos_a_diagnosticar(Creencias,Observaciones,EstantesObservados,ObjetosADiagnosticar)
* Obtiene los objetos que tenemos que diagnosticar,
* Para tener que dignosticar un objeto se tiene que cumplir
* que en la creencia se encuentre objeto=lugar
* que lugar ya haya sido observado
* que objeto=lugar no esté en las observaciones
*/

obten_objetos_a_diagnosticar([Obj=>Lugar|Resto],Observaciones,EstantesObservados,[Obj=>'#'|RestoADiagnosticar]):-
    member(Lugar,EstantesObservados),
    not(member(Obj=>Lugar,Observaciones)),
    obten_objetos_a_diagnosticar(Resto,Observaciones,EstantesObservados,RestoADiagnosticar),
    !.

obten_objetos_a_diagnosticar([_|Resto],Observaciones,EstantesObservados,RestoADiagnosticar):-
    obten_objetos_a_diagnosticar(Resto,Observaciones,EstantesObservados,RestoADiagnosticar),
    !.

obten_objetos_a_diagnosticar([],_,_,[]).
