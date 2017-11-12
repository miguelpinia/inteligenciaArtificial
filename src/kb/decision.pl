:- [util].

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
 */
objetos_pos_incorrecta(KB, Incorrectos) :-
    extension_de_clase(KB, estante, Estantes),
    maplist(objetos_pos_incorrecta(KB), Estantes, Incs),
    flatten(Incs, Incorrectos).
