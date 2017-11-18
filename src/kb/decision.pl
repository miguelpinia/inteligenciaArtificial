:- [util, consulta, modifica].

/*
 * Obtiene la lista decisiones que debe tomar el robot. Este es el
 * predicado principal del módulo.
 */
decision(KB, Decisiones, NuevaKB) :-
    calcula_decisiones(KB, [], Decisiones),
    modifica_propiedad_de_objeto(KB,decisiones=>val(Decisiones),golem,NuevaKB), !.

/*
 * Verifica si una lista es vacía.
 */
es_vacia(Lista) :- Lista = [].

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

/*
 * Calcula el costo de una serie de decisiones.
 */
costo(_, _, [], 0) :- !.
costo(KB, Posicion, [entregar(Producto)|Decisiones], Costo) :-
    lugar_actual_de_producto(KB, Producto, Lugar),
    distancia(KB, Posicion, Lugar, D1),
    distancia(KB, Lugar, mostrador, D2),
    costo(KB, mostrador, Decisiones, C1),
    Costo is D1 + D2 + C1.
costo(KB, Posicion, [reacomodar(Producto)|Decisiones], Costo) :-
    lugar_actual_de_producto(KB, Producto, Lugar),
    lugar_correcto_de_producto(KB, Producto, CLugar),
    distancia(KB, Posicion, Lugar, D1),
    distancia(KB, Posicion, CLugar, D2),
    costo(KB, CLugar, Decisiones, C1),
    Costo is D1 + D2 + C1.

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
        expande_nodo(Estado, Entregar, Expansion),
        calcula_minimo(KB, Expansion, Minimo),
        calcula_decisiones(KB, Minimo, Decisiones));
       (not(es_vacia(Reacomodar)),
        expande_nodo(Estado, Reacomodar, Expansion),
        calcula_minimo(KB, Expansion, Minimo),
        calcula_decisiones(KB, Minimo, Decisiones));
       (Decisiones = Estado))), !;
     (length(Estado, 1),
      (Estado = [reacomodar(_)],
       separa(Pendientes, _, Reacomodar),
       ((es_vacia(Reacomodar),
         Decisiones = Estado, !);
        (findall(reacomodar(P), (member(reacomodar(P), Reacomodar),
                                 not(member(entregar(P), Estado)),
                                 not(member(reacomodar(P), Estado))),
                 Reacomodos),
         not(es_vacia(Reacomodos)),
         expande_nodo(Estado, Reacomodos, Expansion),
         calcula_minimo(KB, Expansion, Decisiones), !)));
      (Estado = [entregar(_)],
       separa(Pendientes, Entregar, Reacomodar),
       ((findall(entregar(P), (member(entregar(P), Entregar),
                               not(member(entregar(P), Estado)),
                               not(member(reacomodar(P), Estado))),
                 Entregas),
         not(es_vacia(Entregas)),
         expande_nodo(Estado, Entregas, Expansion),
         calcula_minimo(KB, Expansion, Minimo),
         calcula_decisiones(KB, Minimo, Decisiones));
        (findall(entregar(P), (member(entregar(P), Entregar),
                               not(member(entregar(P), Estado))),
                 Entregas),
         findall(reacomodar(P), (member(reacomodar(P), Reacomodar),
                                 not(member(entregar(P), Estado)),
                                 not(member(reacomodar(P), Estado))),
                 Reacomodos),
         es_vacia(Entregas),
         not(es_vacia(Reacomodos)),
         expande_nodo(Estado, Reacomodos, Expansion),
         calcula_minimo(KB, Expansion, Minimo),
         calcula_decisiones(KB, Minimo, Decisiones)), !;
        (findall(entregar(P), (member(entregar(P), Entregar),
                               not(member(entregar(P), Estado)),
                               not(member(reacomodar(P), Estado))),
                 Entregas),
         findall(reacomodar(P), (member(reacomodar(P), Reacomodar),
                                 not(member(entregar(P), Estado)),
                                 not(member(reacomodar(P), Estado))),
                 Reacomodos),
         es_vacia(Entregas),
         es_vacia(Reacomodos),
         Decisiones = Estado)))), !;
     (length(Estado, 2),
      ((Estado = [reacomodar(_), reacomodar(_)],
        Decisiones = Estado), !;
       ((Estado = [entregar(_), entregar(_)];
         Estado = [entregar(_), reacomodar(_)]),
        separa(Pendientes, _, Reacomodar),
        ((es_vacia(Reacomodar),
          Decisiones = Estado), !;
         (findall(reacomodar(P), (member(reacomodar(P), Reacomodar),
                                  not(member(entregar(P), Estado)),
                                  not(member(reacomodar(P), Estado))),
                  Reacomodos),
          expande_nodo(Estado, Reacomodos, Expansion),
          calcula_minimo(KB, Expansion, Minimo),
          calcula_decisiones(KB, Minimo, Decisiones)))))), !;
     (length(Estado, 3),
      (Estado = [entregar(_), reacomodar(_), reacomodar(_)],
       Decisiones = Estado);
      (Estado = [entregar(_), entregar(_), reacomodar(_)],
       separa(Pendientes, _, Reacomodar),
       findall(reacomodar(P), (member(reacomodar(P), Reacomodar),
                               not(member(reacomodar(P), Estado)),
                               not(member(entregar(P), Estado))),
               Reacomodos),
       ((es_vacia(Reacomodos),
         Decisiones = Estado);
        (not(es_vacia(Reacomodos)),
         expande_nodo(Estado, Reacomodos, Expansion),
         calcula_minimo(KB, Expansion, Decisiones)))));
     (Estado = Decisiones,
      Decisiones = Estado), !).
