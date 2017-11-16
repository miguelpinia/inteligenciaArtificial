:- [util, consulta, modifica].

/*
 * Obtiene la posición del robot.
 * ?- open_kb('kb_proy2.txt', KB), posicion(KB, Pos).
 */
posicion(KB, Posicion) :-
    propiedades_de_objeto(KB, golem, Props),
    filtra_por_atributo(Props, pos, [pos=>Posicion]).
/*
 * Lugar actual de un producto.
 * ?- open_kb('kb_proy2.txt', KB), lugar_actual_de_producto(KB, bisquits, Lugar).
 */
lugar_actual_de_producto(KB, Producto, Lugar) :-
    obten_creencias(KB, Creencias),
    filtra_por_atributo(Creencias, Producto, [Producto=>Lugar]).

/*
 * Dado el un producto, encuentra el lugar donde debe de ir.
 *
 * ?- open_kb('kb_proy2.txt', KB), lugar_correcto(KB, noodles, Lugar).
 */
lugar_correcto(KB, Producto, Producto=>Lugar) :-
    lugar_correcto_de_producto(KB, Producto, Lugar).

/*
 * Dada una lista de productos, devuelve la lista con los lugares
 * donde deben de ir.
 * ?- open_kb('kb_proy2.txt', KB), lugares_correctos(KB, [coca, heineken, noodles, bisquits], L).
 */
lugares_correctos(KB, Prods, Lugs) :-
    maplist(lugar_correcto(KB), Prods, Lugs).

/*
 * Dado un estante, encuentra todos los objetos que están en posición
 * incorrecta.
 * ?- open_kb('kb_proy2.txt', KB), objetos_pos_incorrecta(KB, bread_shelf, Incorrectos).
 */
objetos_pos_incorrecta(KB, Estante, Incorrectos) :-
    obten_creencias(KB, Creencias),
    filtra_por_valor(Creencias, Estante, CreenciasEstante),
    lista_de_atributos(CreenciasEstante, Objetos),
    lugares_correctos(KB, Objetos, Correctos),
    subtract(CreenciasEstante, Correctos, Incorrectos_Com),
    lista_de_atributos(Incorrectos_Com, Incorrectos).

/*
 * Obtiene la lista de todos los objetos en estado incorrecto.
 * ?- open_kb('kb_proy2.txt', KB), objetos_pos_incorrecta(KB, Incorrectos).
 */
objetos_pos_incorrecta(KB, Incorrectos) :-
    obten_estantes_observados(KB, Estantes),
    maplist(objetos_pos_incorrecta(KB), Estantes, Incs),
    flatten(Incs, Incorrectos).

/*
 * Lista de objetos pendientes de entregar.
 * ?- open_kb('kb_proy2.txt', KB), por_entregar(KB, Entregas).
 */
por_entregar(KB, Entregas) :-
    propiedades_de_objeto(KB, golem, Props),
    filtra_por_atributo(Props, pendientes, [pendientes=>val(Pendientes)]),
    findall(X, (member(entregar(X), Pendientes)), Entregas).

/*
 * Devuelve el objeto que está cargando en la mano derecha.
 */
cargando_der(KB, Der) :-
    (relaciones_de_objeto(KB, der, PropsDer),
     filtra_por_atributo(PropsDer, tiene, [tiene=>Der]));
    identidad([], Der).

/*
 * Devuelve el objeto que está cargando en la mano izquierda.
 */
cargando_izq(KB, Izq) :-
    (relaciones_de_objeto(KB, izq, PropsIzq),
     filtra_por_atributo(PropsIzq, tiene, [tiene=>Izq]));
    identidad([], Izq).

/*
 * Obtiene los elementos que está cargando el robot.
 * ?- open_kb('kb_proy2.txt', KB), cargando(KB, Carga).
 */
cargando(KB, Carga) :-
    cargando_der(KB, Der), cargando_izq(KB, Izq),
    ((is_list(Der), is_list(Izq), append(Der, Izq, Carga), !);
    (is_list(Der), not(is_list(Izq)), append(Der, [Izq], Carga), !);
    (append([Der], [Izq], Carga), !)).


/*
 * Obtiene la lista de reacomodos pendientes del robot para un
 * estante.
 * ?- open_kb('kb_proy2.txt', KB), reacomodos_pendientes(KB, bread_shelf, Pendientes).
 */
reacomodos_pendientes(KB, Estante, Pendientes) :-
    objetos_pos_incorrecta(KB, Estante, Incorrectos),
    findall(reacomodar(X), (member(X, Incorrectos)), Pendientes).

/*
 * Obtiene la lista de tareas pendientes para todos los estantes observados.
 * ?- open_kb('kb_proy2.txt', KB), tareas_pendientes(KB, Pendientes).
 */
tareas_pendientes(KB, Pendientes) :-
    obten_estantes_observados(KB, Estantes),
    maplist(reacomodos_pendientes(KB), Estantes, Pends),
    flatten(Pends, NPends),
    por_entregar(KB, Entregas),
    findall(reacomodar(X), (member(reacomodar(X), NPends), not(member(X, Entregas))), ReacomodaFiltrado),
    findall(entregar(X), (member(X, Entregas)), EntregasFiltrado),
    append(EntregasFiltrado, ReacomodaFiltrado, Parcial),
    cargando(KB, Carga),
    findall(reacomodar(X), (member(X, Carga), not(member(X, Entregas))), NCarga),
    append(Parcial, NCarga, Pendientes).

/*
 * Dada una decisión, expande las posibles decisiones a partir de las
 * tareas pendientes.
 * ?- open_kb('kb_proy2.txt', KB), expande_decision(KB, [], X).
 */
expande_decision(KB, Decisiones, Expansion) :-
    tareas_pendientes(KB, Pendientes),
    findall(Parcial, (member(Decision, Pendientes),
                not(member(Decision, Decisiones)),
                append(Decisiones, [Decision], Parcial)),
            Expansion).

/*
 * Obtiene la distancia actual de un lugar a otro.
 * ?- open_kb('kb_proy2.txt', KB), distancia(KB, mostrador, bread_shelf, D).
 */
distancia(KB, Lugar1, Lugar2, Distancia) :-
    relaciones_de_objeto(KB, Lugar1, Rels),
    filtra_por_atributo(Rels, distancia, [distancia=>ListaDistancias]),
    member([Lugar2, Distancia], ListaDistancias).

/*
 * Costo de moverse de un lugar a otro.
 * ?- open_kb('kb_proy2.txt', KB), costo_movimiento(KB, [entregar(coca), reacomodar(noodles), reacomodar(bisquits)], mostrador, X).
 */
costo_movimiento(_, [], _, 0) :- !.
costo_movimiento(KB, [reacomodar(Prod)|Decisiones], Posicion, Costo) :-
    lugar_actual_de_producto(KB, Prod, Lugar),
    lugar_correcto_de_producto(KB, Prod, CLugar),
    distancia(KB, Posicion, Lugar, Dist1),
    distancia(KB, Lugar, CLugar, Dist2),
    Dist3 is Dist1 + Dist2,
    costo_movimiento(KB, Decisiones, CLugar, NCosto),
    Costo is NCosto + Dist3, !.
costo_movimiento(KB, [entregar(Prod)|Decisiones], Posicion, Costo) :-
    lugar_actual_de_producto(KB, Prod, Lugar),
    distancia(KB, Posicion, Lugar, Dist1),
    distancia(KB, Lugar, mostrador, Dist2),
    Dist3 is Dist1 + Dist2,
    costo_movimiento(KB, Decisiones, mostrador, NCosto),
    Costo is NCosto + Dist3, !.

/*
 * Costo de la espera del cliente.
 * ?- open_kb('kb_proy2.txt', KB), costo_espera(KB, [entregar(coca), entregar(noodles)], [], X).
 */
costo_espera(_, [], _, 0) :- !.
costo_espera(KB, [entregar(Prod)|Decisiones], Decs, Costo) :-
    lugar_actual_de_producto(KB, Prod, Lugar),
    costo_movimiento(KB, Decs, Lugar, C1),
    append(Decs, [entregar(Prod)], L2),
    costo_espera(KB, Decisiones, L2, C2),
    Costo is C1 + C2, !.
costo_espera(KB, [Dec|Decisiones], Decs, Costo) :-
    append(Decs, [Dec], L1),
    costo_espera(KB, Decisiones, L1, Costo).

/*
 * Calcula el costo total de una lista de decisiones a partir de una
 * posición inicial.
 * ?- open_kb('kb_proy2.txt', KB), costo(KB, [entregar(coca), reacomodar(noodles), reacomodar(bisquits)], mostrador, X).
 */
costo(KB, Decisiones, Posicion, Costo) :-
    costo_movimiento(KB, Decisiones, Posicion, C1),
    costo_espera(KB, Decisiones, [], C2),
    Costo is C1 + C2.

/*
 * Heuristica de expansión de un nodo de decisión. Esta heuristica va
 * tomar en cuenta la lista de entregas pendientes que aún no han sido
 * tomadas en cuenta más la lista de las acciones de los pendientes
 * que no se han tomado en cuenta. Multiplica el costo de las
 * decisiones actuales por el que toma moverse para satisfacerlas y le
 * suma el costo de las entregas más el número de acciones restantes
 * por 2.
 * open_kb('kb_proy2.txt', KB), heuristica(KB, [entregar(coca)], X).
 */
heuristica(KB, Decisiones, Costo) :-
    tareas_pendientes(KB, Pendientes),
    findall(Prod, (member(entregar(Prod), Pendientes), not(member(entregar(Prod), Decisiones))), Lista1),
    findall(Decision, (member(Decision, Pendientes), not(member(Decision, Decisiones))), Lista2),
    length(Lista1, Costo1),
    length(Lista2, Costo2),
    posicion(KB, Posicion),
    costo_movimiento(KB, Decisiones, Posicion, Costo3),
    Costo4 is Costo1 * Costo3,
    Costo5 is (Costo1 * (Costo1 - 1)) / 2,
    Costo is Costo4 + Costo5 + (Costo2 * 2).

/*
 * Costo de la estimación de expansión de un nodo de decisión. Esto es
 * el costo total de evaluar la decisión más la heurística de lo que
 * falta por terminar.
 */
costo_estimacion(KB, Decision, Costo) :-
    posicion(KB, Posicion),
    costo(KB, Decision, Posicion, Costo1),
    heuristica(KB, Decision, Costo2),
    Costo is Costo1 + Costo2.

/*
 * Dada una lista de decisiones, calcula la mejor decisión de todas.
 */
selecciona_decision(KB, Decisiones, Decision) :-
    maplist(costo_estimacion(KB), Decisiones, Costos),
    min_list(Costos, MinCosto),
    findall(Dec, (member(Dec, Decisiones), costo_estimacion(KB, Dec, MinCosto)), [Decision|_]).

/*
 * Calcula todas las decisiones que debe tomar el robot.
 * ?- open_kb('kb_proy2.txt', KB), decision(KB, Decisiones, NKB).
 */
decision(KB, Decisiones, NuevaKB) :-
    (Decisiones == [],
     tareas_pendientes(KB, []),
     modifica_propiedad_de_objeto(KB, decisiones=>val([]), golem, NuevaKB), !);
    (decision_(KB, [], Decisiones),
     modifica_propiedad_de_objeto(KB, decisiones=>val(Decisiones), golem, NuevaKB), !).
decision_(KB, Decs, Decisiones) :-
    (Decs == [],
     expande_decision(KB, [], Dcs),
     decision_(KB, Dcs, Decisiones));
    (selecciona_decision(KB, Decs, Decision),
     expande_decision(KB, Decision, []),
     Decisiones = Decision, !);
    (selecciona_decision(KB, Decs, Decision),
     findall(R, (member(R, Decs), R \= Decision), L1),
     expande_decision(KB, Decision, L2),
     append(L1, L2, L3),
     decision_(KB, L3, Decisiones), !).
