:- [util, consulta, modifica].

/*
 * Obtiene la lista decisiones que debe tomar el robot. Este es el
 * predicado principal del módulo.
 */
decision(KB, Decisiones) :-
    calcula_decisiones(KB, [], Decisiones), !.

/*
 * Obtiene el lugar Lugar actual del producto Producto.
 */
lugar_actual_de_producto(KB, Producto, Lugar) :-
    obten_creencias(KB, Creencias),
    filtra_por_atributo(Creencias, Producto, [Producto=>Lugar]).

/*
 * Calcula la distancia existentes entre Lugar1 y Lugar2.
 */
distancia(KB, Lugar1, Lugar2, Distancia) :-
    extension_de_propiedad_(KB, c, [c=>val(Vals)]),
    (filtra_por_atributo(Vals, mover(Lugar1, Lugar2), [_=>Distancia]), !;
     filtra_por_atributo(Vals, mover(_, _), [_=>Distancia])).

probabilidad_agarre(KB, Producto, Probabilidad) :-
    extension_de_propiedad_(KB, p, [p=>val(Vals)]),
    (filtra_por_atributo(Vals, agarrar(Producto), [_=>Probabilidad]), !;
     filtra_por_atributo(Vals, agarrar(_), [_=>Probabilidad])).

probabilidad_colocar(KB, Producto, Probabilidad) :-
    extension_de_propiedad_(KB, p, [p=>val(Vals)]),
    (filtra_por_atributo(Vals, colocar(Producto), [_=>Probabilidad]), !;
     filtra_por_atributo(Vals, colocar(_), [_=>Probabilidad])).

probabilidad_buscar(KB, Producto, Probabilidad) :-
    extension_de_propiedad_(KB, p, [p=>val(Vals)]),
    (filtra_por_atributo(Vals, buscar(Producto), [_=>Probabilidad]), !;
     filtra_por_atributo(Vals, buscar(_), [_=>Probabilidad])).

recompensa_agarre(KB, Producto, Recompensa) :-
    extension_de_propiedad_(KB, r, [r=>val(Vals)]),
    (filtra_por_atributo(Vals, agarrar(Producto), [_=>Recompensa]), !;
     filtra_por_atributo(Vals, agarrar(_), [_=>Recompensa])).

recompensa_colocar(KB, Producto, Recompensa) :-
    extension_de_propiedad_(KB, r, [r=>val(Vals)]),
    (filtra_por_atributo(Vals, colocar(Producto), [_=>Recompensa]), !;
     filtra_por_atributo(Vals, colocar(_), [_=>Recompensa])).

recompensa_buscar(KB, Producto, Recompensa) :-
    extension_de_propiedad_(KB, r, [r=>val(Vals)]),
    (filtra_por_atributo(Vals, buscar(Producto), [_=>Recompensa]), !;
     filtra_por_atributo(Vals, buscar(_), [_=>Recompensa])).

/*
 * Calcula el costo de una serie de decisiones.
 */
costo(_, _, [], 0) :- !.
costo(KB, Posicion, [entregar(Producto)|Decisiones], Costo) :-
    lugar_actual_de_producto(KB, Producto, Lugar),
    distancia(KB, Posicion, Lugar, D1),
    distancia(KB, Lugar, mostrador, D2),
    recompensa_agarre(KB, Producto, R1),
    recompensa_colocar(KB, Producto, R2),
    recompensa_buscar(KB, Producto, R3),
    probabilidad_agarre(KB, Producto, P1),
    probabilidad_colocar(KB, Producto, P2),
    probabilidad_buscar(KB, Producto, P3),
    R is (1 / R1) + (1 / R2) + (1 / R3),
    P is (1 / P1) + (1 / P2) + (1 / P3),
    costo(KB, mostrador, Decisiones, C1),
    Costo is D1 + D2 + C1 + R + P.
costo(KB, Posicion, [reacomodar(Producto)|Decisiones], Costo) :-
    lugar_actual_de_producto(KB, Producto, Lugar),
    lugar_correcto_de_producto(KB, Producto, CLugar),
    distancia(KB, Posicion, Lugar, D1),
    distancia(KB, Lugar, CLugar, D2),
    recompensa_agarre(KB, Producto, R1),
    recompensa_colocar(KB, Producto, R2),
    recompensa_buscar(KB, Producto, R3),
    probabilidad_agarre(KB, Producto, P1),
    probabilidad_colocar(KB, Producto, P2),
    probabilidad_buscar(KB, Producto, P3),
    R is (1 / R1) + (1 / R2) + (1 / R3),
    P is (1 / P1) + (1 / P2) + (1 / P3),
    costo(KB, Lugar, Decisiones, C1),
    Costo is D1 + D2 + C1 + R + P.

/*
 * Separa la lista de pendientes en entregas y reacomodos.
 */
separa(Pendientes, Entregar, Reacomodar) :-
    findall(entregar(P), member(entregar(P), Pendientes), Entregar),
    findall(reacomodar(P), member(reacomodar(P), Pendientes), Reacomodar).

/*
 * Dada una decisión y una lista de acciones pendientes, expande los
 * posibles estados siguientes de la decisión actual con las acciones
 * pendientes.
 */
expande_nodo(Decs, Pends, Exp) :-
    findall(Parcial, (member(D, Pends),
                      not(member(D, Decs)),
                      append(Decs, [D], Parcial)),
            Exp).

/*
 * Dada una lista de listas de decisiones, calcula la lista de
 * decisiones de peso mínimo.
 */
calcula_minimo(KB, Decs, MinDec) :-
    obten_posicion(KB, Posicion),
    maplist(costo(KB, Posicion), Decs, Mins),
    min_list(Mins, Min),
    findall(D, (member(D, Decs),
                costo(KB, Posicion, D, Min)),
            [MinDec|_]), !.

/*
 * Dado un estado y una lista de pendientes, calcula su estado
 * siguiente.
 */
estado_siguiente(KB, Estado, Pendientes, Siguiente) :-
    expande_nodo(Estado, Pendientes, Expansion),
    calcula_minimo(KB, Expansion, Siguiente), !.

/*
 * Filtra los reacomodos que no están en el estado.
 */
filtra_reacomodos(Estado, Reacomodar, Reacomodos) :-
    findall(reacomodar(P), (member(reacomodar(P), Reacomodar),
                            not(member(entregar(P), Estado)),
                            not(member(reacomodar(P), Estado))),
            Reacomodos).

/*
 * Filtra las entregas que no están en el estado.
 */
filtra_entregas(Estado, Entregar, Entregas) :-
    findall(entregar(P), (member(entregar(P), Entregar),
                          not(member(entregar(P), Estado)),
                          not(member(reacomodar(P), Estado))),
            Entregas).

/*
 * Obtiene el siguiente paso.
 */
siguiente_paso(KB, Estado, Pendientes, Decisiones) :-
    estado_siguiente(KB, Estado, Pendientes, Siguiente),
    calcula_decisiones(KB, Siguiente, Decisiones).

/*
 * Calcula la lista de decisiones que debe tomar el robot. Esta lista
 * será de tamaño a lo más 4, donde serán a lo más dos entregas y a lo
 * más dos reacomodos. Estas entregas y reacomodos están seleccionadas
 * en el mínimo costo de moverse desde su posición actual a los
 * estantes para realizar las tareas de entrega y reacomodos. Tiene
 * mayor prioridad las entregas sobre los reacomodos.
 */
calcula_decisiones(KB, Estado, Decisiones) :-
    obten_acciones_pendientes(KB, Pendientes),
    ((Estado = [],
      separa(Pendientes, Entregar, Reacomodar),
      ((not(es_vacia(Entregar)),
        siguiente_paso(KB, Estado, Entregar, Decisiones));
       (not(es_vacia(Reacomodar)),
        siguiente_paso(KB, Estado, Reacomodar, Decisiones));
       (Decisiones = Estado))), !;
     (length(Estado, 1),
      (Estado = [reacomodar(P)],
       separa(Pendientes, _, Reacomodar),
       ((es_vacia(Reacomodar),
         Decisiones = Estado, !);
        (findall(reacomodar(Pi), (member(reacomodar(Pi), Reacomodar),
                                  not(member(entregar(Pi), Estado)),
                                  not(member(reacomodar(Pi), Estado)),
                                  lugar_correcto_de_producto(KB, P, L),
                                  lugar_correcto_de_producto(KB, Pi, L)),
                    Reacomodos),
         not(es_vacia(Reacomodos)),
         estado_siguiente(KB, Estado, Reacomodos, Decisiones), !);
        (filtra_reacomodos(Estado, Reacomodar, Reacomodos),
         not(es_vacia(Reacomodos)),
         estado_siguiente(KB, Estado, Reacomodos, Decisiones), !)));
      (Estado = [entregar(_)],
       separa(Pendientes, Entregar, Reacomodar),
       ((filtra_entregas(Estado, Entregar, Entregas),
         not(es_vacia(Entregas)),
         siguiente_paso(KB, Estado, Entregas, Decisiones));
        (filtra_entregas(Estado, Entregar, Entregas),
         filtra_reacomodos(Estado, Reacomodar, Reacomodos),
         es_vacia(Entregas),
         not(es_vacia(Reacomodos)),
         siguiente_paso(KB, Estado, Reacomodos, Decisiones)), !;
        (filtra_entregas(Estado, Entregar, Entregas),
         filtra_reacomodos(Estado, Reacomodar, Reacomodos),
         es_vacia(Entregas),
         es_vacia(Reacomodos),
         Decisiones = Estado)))), !;
     (length(Estado, 2),
      ((Estado = [reacomodar(_), reacomodar(_)],
        Decisiones = Estado), !;
       (Estado = [entregar(_), entregar(_)],
        separa(Pendientes, _, Reacomodar),
        ((es_vacia(Reacomodar),
          Decisiones = Estado), !;
         (filtra_reacomodos(Estado, Reacomodar, Reacomodos),
          siguiente_paso(KB, Estado, Reacomodos, Decisiones))));
       (Estado = [entregar(_), reacomodar(P)],
        separa(Pendientes, _, Reacomodar),
        ((findall(reacomodar(Pi), (member(reacomodar(Pi), Reacomodar),
                                  not(member(entregar(Pi), Estado)),
                                  not(member(reacomodar(Pi), Estado)),
                                  lugar_correcto_de_producto(KB, P, L),
                                  lugar_correcto_de_producto(KB, Pi, L)),
                 Reacomodos),
         not(es_vacia(Reacomodos)),
         estado_siguiente(KB, Estado, Reacomodos, Decisiones), !);
        (separa(Pendientes, _, Reacomodar),
         ((es_vacia(Reacomodar),
           Decisiones = Estado), !;
         (filtra_reacomodos(Estado, Reacomodar, Reacomodos),
          siguiente_paso(KB, Estado, Reacomodos, Decisiones)))))))), !;
     (length(Estado, 3),
      (Estado = [entregar(_), reacomodar(_), reacomodar(_)],
       Decisiones = Estado);
      (Estado = [entregar(_), entregar(_), reacomodar(_)],
       separa(Pendientes, _, Reacomodar),
       filtra_reacomodos(Estado, Reacomodar, Reacomodos),
       ((es_vacia(Reacomodos),
         Decisiones = Estado);
        (not(es_vacia(Reacomodos)),
         estado_siguiente(KB, Estado, Reacomodos, Decisiones)))));
     (Estado = Decisiones,
      Decisiones = Estado), !).
