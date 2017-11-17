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

plan_suc(KB,node(Id,Padre,G,edo(mostrador,[Oi],_,Pend,Plan)),Sucesores):-
	/*Movernos */ extension_de_clase(KB,estante,Estantes).
	calcula_mover(mostrador,Estantes,Movimientos),
	/*entregar*/[entregar(Oi)|Estantes],

	.

/*plan_suc(KB,node(Id,Padre,G,mostrador,Izq,Der,Pend))
plan_suc(KB,node(Id,Padre,G,mostrador,Izq,Der,Pend))
plan_suc(KB,node(Id,Padre,G,mostrador,Izq,Der,Pend))
*/
/* Calcula la secuencia de movientos posibles dado un origen y una lista de destinos */
calcula_mover(Origen,[Destino|Resto],[mover(Origen,Destino)|Movimientos]):-
	calcula_mover(Origen,Resto,Movimientos),!.
calcula_mover(_,[],[]).

calcula_sucesores(KB,[Accion|Acciones],node(Id,Padre,G,edo(Pos,Izq,Der,Pend,Plan)),LastId,
		[Key=>node(Nuevo_Id,Id,Nueva_G,edo(Nueva_pos,Nuevo_izq,Nuevo_der,Nuevos_pend,Nuevo_plan))|Sucesores]):-
	(
		(
			Accion=mover(Pos,Lj),
			Nueva_pos=Lj,
			Nuevo_izq =  Izq,
			Nuevo_der = Der,
			Nuevos_pend = Pend,
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
						delete(Pendientes,entregar(Oi),Nuevos_pend)
					);
					(
						lugar_correcto_de_producto(KB,Oi,Pos),
						delete(Pendientes,reacomodar(Oi),Nuevos_pend)
					)
				),
			)
		);
		(
			Accion=agarrar(Oi),
			(
				(
					Der=[],
					Nuevo_der=[Oi]
				);
				(
					Izq=[],
					Nuevo_izq=[Oi]
				)
			)
		);
		(
			Accion=buscar(Oi),
			(
				Nueva_pos=Pos,
				Nuevo_izq=Izq,
				Nuevo_der=Der,
				Nuevos_pend=Pend
			)
		)
	),
	append(Plan,[Accion],Nuevo_plan),
	Nuevo_Id is LastId + 1,
	obten_costo(KB,Accion,C),
	/* Checar si es necesario el + 1 */
	Nueva_G is G + C,
	Key is G + C + 1,
	calcula_sucesores()
calcula_sucesores([],_,_,[]).
