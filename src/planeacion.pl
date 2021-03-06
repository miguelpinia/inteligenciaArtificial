/* plan_suc(KB,node(Id,Padre,G,Pos,Izq,Der,Pend))
Id - nodo actual
Padre - Id del nodo padre
G - Costo del camino hasta el nodo actual
Pend - Lista de las acciones pendientes
Pos - Posicion del robot
Izq - Lo que trae en el brazo izquierdo
Der - Lo que trae en el brazo derecho

Calcula los sucesores del nodo Id
*/

planeacion(KB,Desiciones,Plan):-
	obten_posicion(KB,Pos),
	cargando_izq(KB,Izq),
    %write('XXXXXXXX'),write(Izq),write('XXXXXXXX'),nl,
	cargando_der(KB,Der),
    %write('XXXXXXXXXXXXXXXXX'),nl,
    %write(node(0,nil,0,edo(Pos,Izq,Der,Desiciones,[]))),nl,
	plan(KB,[0=>node(0,nil,0,edo(Pos,Izq,Der,Desiciones,[]))],[],PlanBA),
    transforma_plan(PlanBA,Plan).

/* Cambia las apariciones de ba(Oi) en PlanAB por buscar(Oi),agarrar(Oi)*/
transforma_plan([ba(Oi)|RestoPlanBA],[buscar(Oi)|[agarrar(Oi)|RestoPlan]]):-
    transforma_plan(RestoPlanBA,RestoPlan),
    !.


transforma_plan([Accion|RestoPlanBA],[Accion|RestoPlan]):-
    dif(Accion,ba(_)),
    transforma_plan(RestoPlanBA,RestoPlan),
    !.


/* Ya terminamos*/
transforma_plan([],[]).

/* Caso base si ya no hay pendientes ya terminamos y el plan esta dentro del estado */
plan(_,[_=>node(_,_,_,edo(_,_,_,[],Plan))|_],_,Plan):-
    !.

/*plan(_,[Key=>node(Id,Padre,G,edo(Pos,Izq,Der,[],Plan))|Resto],Cerrados,Plan):-
    todos_planeacion([Key=>node(Id,Padre,G,edo(Pos,Izq,Der,[],Plan))|Resto],Cerrados,Todos),
    save_kb('planeacion.txt',Todos),
    !.*/

/* Caso en que no es meta */

plan(KB,[_=>node(Id,Padre,G,edo(Pos,Izq,Der,Pend,PlanActual))|Resto],Cerrados,Plan):-
    %nl,write(node(Id,Padre,G,edo(Pos,Izq,Der,Pend,PlanActual))),nl,
	lista_de_valores(Resto,Abiertos),
	append(Abiertos,Cerrados,Nodos),
	findall(I,member(node(I,_,_,_),Nodos),Ids),
	max_list([Id|Ids],LastId),
	plan_suc(KB,node(Id,Padre,G,edo(Pos,Izq,Der,Pend,PlanActual)),LastId,Sucesores),
	agrega_pq_muchos(Resto,Sucesores,Nuevos_abiertos),
	plan(KB,Nuevos_abiertos,[node(Id,Padre,G,edo(Pos,Izq,Der,Pend,PlanActual))|Cerrados],Plan).


/*
* lugares_validos(KB,Pendientes,Lugares)
* regresa solo los objetos que nos interesan según la meta
* podemos solo tratar de buscar y agarrar los objetos que tenemos
* en los pendientes para reducir el branch factor
*/
%lugares_validos(KB,_,[mostrador|Ext]):-
    %extension_de_clase(KB,estante,Ext).

lugares_validos(KB,Pendientes,Lugares):-
    lugares_validos_(KB,Pendientes,Lugares).

/*
 * lugares_validos_(KB,Pendientes,Lugares)
 * lo mismo que lugares_validos_(KB,Pendientes,Lugares) pero puede contener izq o der
 */
lugares_validos_(KB,[entregar(Oi)|Resto],Lugares):-
    obten_creencias(KB,Creencias),
    filtra_por_atributo(Creencias,Oi,[Oi=>LugarOi|_]),
    lugares_validos_(KB,Resto,RestoLugares),
    lugar_correcto_de_producto(KB,Oi,LugarCorrecto),
    append([LugarCorrecto,mostrador,LugarOi],RestoLugares,Lugares_),
    list_to_set(Lugares_,Lugares),
    !.

lugares_validos_(KB,[reacomodar(Oi)|Resto],Lugares):-
    obten_creencias(KB,Creencias),
    filtra_por_atributo(Creencias,Oi,[Oi=>LugarOi|_]),
    lugares_validos_(KB,Resto,RestoLugares),
    lugar_correcto_de_producto(KB,Oi,LugarCorrecto),
    append([LugarCorrecto,LugarOi],RestoLugares,Lugares_),
    list_to_set(Lugares_,Lugares),
    !.

lugares_validos_(_,[],[]).

/*
 * objetos_validos(Pendientes,Objetos)
 * regresa solo los objetos que nos interesan segun los pendientes
 * solo buscaremso o agarraremos objetos que nos inteesan
 */
objetos_validos([entregar(Oi)|Resto],Objetos):-
    objetos_validos(Resto,RestoObjetos),
    union([Oi],RestoObjetos,Objetos),
    !.

objetos_validos([reacomodar(Oi)|Resto],Objetos):-
    objetos_validos(Resto,RestoObjetos),
    union([Oi],RestoObjetos,Objetos),
    !.

objetos_validos([],[]).


plan_suc(KB,node(Id,Padre,G,edo(mostrador,Izq,Der,Pend,Plan)),LastId,Sucesores):-
	/*Movernos solo a los lugares validos excepto por donde estamos*/
    (   last(Plan, Ultimo);
        Ultimo = ba(_)),
    (   (   Ultimo = mover(_, _),
            Movimientos = [], !);
        (   lugares_validos(KB,Pend,Lugares),
            delete(Lugares,mostrador,DestinosValidos_),
            /*Ademas no queremos regresar a donde ya estuvimos*/
            visitados(Plan,Visitados),
            subtract(DestinosValidos_,Visitados,DestinosValidos),
            calcula_mover(mostrador,DestinosValidos,Movimientos))),
	(
		(/*Si el brazo izquierdo esta vacio no podemos entregar */
			Izq=[],
			(
				(
					Der=[],
					Acciones=Movimientos,!
				);
				(
					Der=[Oi],
					Acciones=[colocar(Oi)|Movimientos]
				)
			),
			!

		);
		(
			Izq=[Oi],
			(
				(
					Der=[],
					Acciones=[colocar(Oi)|Movimientos],!
				);
				(
					Der=[Oj],
					Acciones=[colocar(Oi)|[colocar(Oj)|Movimientos]]
				)
			)
		)
	),
    /*Finalmente no tendría sentido repetir algo que ya pensabamos hacer*/
    subtract(Acciones,Plan,AccionesNuevas),
	calcula_sucesores(KB,AccionesNuevas,node(Id,Padre,G,edo(mostrador,Izq,Der,Pend,Plan)),LastId,Sucesores),
    !.

plan_suc(KB,node(Id,Padre,G,edo(Estante,Izq,Der,Pend,Plan)),LastId,Sucesores):-
	dif(Estante,mostrador),
    (   last(Plan, Ultimo);
        Ultimo = ba(_)),
    (   (   Ultimo = mover(_, _),
            Movimientos = [], !);
        /*Movernos solo a los lugares validos excepto por donde estamos*/
        (   lugares_validos(KB,Pend,Lugares),
            delete(Lugares,Estante,DestinosValidos_),
            /*Ademas no queremos regresar a donde ya estuvimos*/
            visitados(Plan,Visitados),
            subtract(DestinosValidos_,Visitados,DestinosValidos),
            calcula_mover(Estante,DestinosValidos,Movimientos))),
    obten_creencias(KB,Creencias),
    filtra_por_valor(Creencias,Estante,Filtrados),
    lista_de_atributos(Filtrados,Objetos),
    objetos_validos(Pend,Validos),
    intersection(Objetos,Validos,ObjetosValidos),
    /* Calculamos los objetos que podemos buscar y agarrar*/
    calcula_ba(ObjetosValidos,Ba),
    (
		(
			Izq=[],
			(
				(/* Si los dos brazos estan libres no podemos colocar */
					Der=[],
                    /* Podemos buscar y agarrar o movernos*/
                	append(Ba,Movimientos,Acciones),
					!
				);
				(/* Si sólo el izquierdo esta libre */
					Der=[Oi],
                    /* Podemos buscar y agarrar o movernos*/
                	append(Ba,Movimientos,Acciones_),
                    /* Además podemos colocar lo que tenemos en el brazo derecho */
					Acciones=[colocar(Oi)|Acciones_]
				)
			),
			!

		);
		(
			Izq=[Oi],
			(
				(/* Si sólo el derecho esta libre */
					Der=[],
                    /* Podemos buscar y agarrar o movernos */
                	append(Ba,Movimientos,Acciones_),
                    /* Además podemos colocar lo que tenemos en el brazo izquierdo */
					Acciones=[colocar(Oi)|Acciones_],
					!
				);
				(/* Si no hay brazo libre no puedo agarrar*/
					Der=[Oj],
                    /* Podemos colocar lo que tenemos en ambos brazos o movernos */
					Acciones=[colocar(Oi)|[colocar(Oj)|Movimientos]]
				)
			)
		)
	),
    /*Finalmente no tendría sentido repetir algo que ya pensabamos hacer*/
    subtract(Acciones,Plan,AccionesNuevas),
	calcula_sucesores(KB,AccionesNuevas,node(Id,Padre,G,edo(Estante,Izq,Der,Pend,Plan)),LastId,Sucesores),
    !.

/* Calcula la secuencia de movientos posibles dado un origen y una lista de destinos */
calcula_mover(Origen,[Destino|Resto],[mover(Origen,Destino)|Movimientos]):-
	calcula_mover(Origen,Resto,Movimientos),!.
calcula_mover(_,[],[]).

/*ba- buscar y agarrar*/
calcula_ba([Objeto|Resto],[ba(Objeto)|Ba_resto]):-
	calcula_ba(Resto,Ba_resto),!.
calcula_ba([],[]).

/*
 * visitados(Plan,Lugares)
 * Calcula los lugares visitados basado en el plan
 */
visitados([mover(_,Lj)|Resto],Lugares):-
    visitados(Resto,RestoLugares),
    union([Lj],RestoLugares,Lugares),
    !.

visitados([Accion|Resto],Lugares):-
    dif(Accion,mover(_,_)),
    visitados(Resto,Lugares),
    !.

visitados([],[]).


calcula_sucesores(KB,[Accion|Acciones],node(Id,IdPadre,H,edo(Pos,Izq,Der,Pend,Plan)),LastId,
                  [Key=>node(Nuevo_Id,Id,Key,edo(Nueva_pos,Nuevo_izq,Nuevo_der,Nuevos_pend,Nuevo_plan))|Sucesores]):-
	(
		(
			Accion=mover(Pos,Lj),
			Nueva_pos=Lj,
			Nuevo_izq =  Izq,
			Nuevo_der = Der,
			Nuevos_pend = Pend,
			!
		);
		(
			Accion=colocar(Oi),
			(
				Nueva_pos = Pos,
				(
					(
						Izq=[Oi],
						Nuevo_izq=[],
						Nuevo_der=Der,
						!
					);
					(
						Der=[Oi],
						Nuevo_izq=Izq,
						Nuevo_der=[]
					)
				),
				(
					(
						Pos = mostrador,
						/* Vamos a entregar */
						delete(Pend,entregar(Oi),Nuevos_pend),
						!
					);
					(
						lugar_correcto_de_producto(KB,Oi,Pos),
						delete(Pend,reacomodar(Oi),Nuevos_pend)
					);
					(
						Nuevos_pend = Pend
					)
				)
			),!
		);
		(
			Accion=ba(Oi),
			Nueva_pos=Pos,
			Nuevos_pend=Pend,
			(
				(
					Der=[],
					Nuevo_der=[Oi],
					Nuevo_izq = Izq,
					!
				);
				(
					Izq=[],
					Nuevo_izq=[Oi],
					Nuevo_der = Der
				)
			)
		)
	),
    append(Plan,[Accion],Nuevo_plan),
	/* Checar si es necesario el + 1 */
	Nuevo_Id is LastId + 1,
	calcula_costo_de_plan(KB,Nuevo_plan,CostoTotal),
    calcula_probabilidad_de_plan(KB,Nuevo_plan,ProbabilidadTolal),
    length(Pend,L),
	Key is CostoTotal * L / ProbabilidadTolal,
	calcula_sucesores(
		KB,
		Acciones,
		node(Id,IdPadre,H,edo(Pos,Izq,Der,Pend,Plan)),
		Nuevo_Id,
		Sucesores),
	!.

calcula_sucesores(_,[],_,_,[]).
