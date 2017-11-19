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
	cargando_der(KB,Der),
	plan(KB,[0=>node(0,nil,0,edo(Pos,Izq,Der,Desiciones,[]))],[],Plan).

/* Caso base si ya no hay pendientes ya terminamos y el plan esta dentro del estado */
plan(_,[_=>node(_,_,_,edo(_,_,_,[],Plan))|_],_,Plan):-!.

/* Caso en que no es meta */

plan(KB,[_=>node(Id,Padre,G,edo(Pos,Izq,Der,Pend,PlanActual))|Resto],Cerrados,Plan):-
	lista_de_valores(Resto,Abiertos),
	append(Abiertos,Cerrados,Nodos),
	findall(Id,member(node(Id,_,_),Nodos),Ids),
	max_list(Ids,LastId),
	plan_suc(KB,node(Id,Padre,G,edo(Pos,Izq,Der,Pend,PlanActual)),LastId,Sucesores),

	agrega_pq_muchos(Resto,Sucesores,Nuevos_abiertos),
	plan(KB,Nuevos_abiertos,[node(Id,Padre,G,edo(Pos,Izq,Der,Pend,PlanActual))|Cerrados],Plan).

plan_suc(KB,node(Id,Padre,G,edo(mostrador,Izq,Der,Pend,Plan)),LastId,Sucesores):-
	/*Movernos */ extension_de_clase(KB,estante,Estantes),
	calcula_mover(mostrador,Estantes,Movimientos),
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
	/*entregar*/
	calcula_sucesores(KB,Acciones,node(Id,Padre,G,edo(mostrador,Izq,Der,Pend,Plan)),LastId,Sucesores),!.

plan_suc(KB,node(Id,Padre,G,edo(Estante,[Oi],Der,Pend,Plan)),LastId,Sucesores):-
	dif(Estante,mostrador),
	/*Movernos */ extension_de_clase(KB,estante,Estantes),
	delete(Estantes,Estante,Estantes_validos),
	calcula_mover(Estante,[mostrador|Estantes_validos],Movimientos),
	obten_creencias(KB,Creencias),
	filtra_por_valor(Creencias,Estante,Filtrados),
	lista_de_atributos(Filtrados,Objetos),
	calcula_ba(Objetos,Ba),
	append(Ba,[colocar(Oi)|Movimientos],Acciones),
	/*entregar*/
	calcula_sucesores(KB,Acciones,node(Id,Padre,G,edo(Estante,[Oi],Der,Pend,Plan)),LastId,Sucesores),!.


plan_suc(KB,node(Id,Padre,G,edo(Estante,Izq,[Oi],Pend,Plan)),LastId,Sucesores):-
	dif(Estante,mostrador),
	/*Movernos */ extension_de_clase(KB,estante,Estantes),
	delete(Estantes,Estante,Estantes_validos),
	calcula_mover(Estante,[mostrador|Estantes_validos],Movimientos),
	obten_creencias(KB,Creencias),
	filtra_por_valor(Creencias,Estante,Filtrados),
	lista_de_atributos(Filtrados,Objetos),
	calcula_ba(Objetos,Ba),
	append(Ba,[colocar(Oi)|Movimientos],Acciones),
	/*entregar*/
	calcula_sucesores(KB,Acciones,node(Id,Padre,G,edo(Estante,Izq,[Oi],Pend,Plan)),LastId,Sucesores),!.


/* Calcula la secuencia de movientos posibles dado un origen y una lista de destinos */
calcula_mover(Origen,[Destino|Resto],[mover(Origen,Destino)|Movimientos]):-
	calcula_mover(Origen,Resto,Movimientos),!.
calcula_mover(_,[],[]).

/*ba- buscar y agarrar*/
calcula_ba([Objeto|Resto],[ba(Objeto)|Ba_resto]):-
	calcula_ba(Resto,Ba_resto),!.
calcula_ba([],[]).

calcula_sucesores(KB,[Accion|Acciones],node(Id,_,G,edo(Pos,Izq,Der,Pend,Plan)),LastId,
		[Key=>node(Nuevo_Id,Id,Nueva_G,edo(Nueva_pos,Nuevo_izq,Nuevo_der,Nuevos_pend,Nuevo_plan))|Sucesores]):-
	write(node(Id,_,G,edo(Pos,Izq,Der,Pend,Plan))),nl,
	write(Accion),nl,
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
			)
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
	obten_costo(KB,Accion,C),
	/* Checar si es necesario el + 1 */
	Nuevo_Id is LastId + 1,
	Nueva_G is G + C,
	Key is G + C + 1,
	calcula_sucesores(
		KB,
		Acciones,
		%node(Nuevo_Id,Id,Nueva_G,edo(Nueva_pos,Nuevo_izq,Nuevo_der,Nuevos_pend,Nuevo_plan)),
		node(Id,_,G,edo(Pos,Izq,Der,Pend,Plan)),
		Nuevo_Id,
		Sucesores),
	!.

calcula_sucesores(_,[],_,_,[]).
