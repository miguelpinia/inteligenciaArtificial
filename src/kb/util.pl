/*
* primera(X,Lista,Y)
* Si la primera aparición de X en Lista es X entonces Y = X 
* Si la primera aparición de X en Lista es not(X) entonces Y = not(X)
*/
primera(X,[X|_],X):-!.
primera(X,[not(X)|_],not(X)):-!.
primera(not(X),[X|_],X):-!.
primera(not(X),[not(X)|_],not(X)):-!.
primera(X,[Y|Tail],Z):-dif(X,Y),dif(not(X),Y),dif(X,not(Y)),primera(X,Tail,Z).

/*
* reemplaza(Lista,Actual,Nuevo,NuevaLista)
* Reemplazar todas las apariciones de Actual por Nuevo en Lista da como resultado NuevaLista
*/
reemplaza([],_,_,[]):-!.
reemplaza([X|Resto],X,Y,[Y|RestoReemplazado]):-
	!,
	reemplaza(Resto,X,Y,RestoReemplazado).
reemplaza([W|Resto],X,Y,[W|RestoReemplazado]):-
	!,
	reemplaza(Resto,X,Y,RestoReemplazado).


/* imprime(Lista)
 * Imprime Lista un elemento por linea
 */
imprime([]).
imprime([H|T]):-
    write(H),
    nl,
    imprime(T).

/*
* ids_numericos(Ids,IdsNumericos)
* los id's numricos de la lista Ids es IdsNumericos
*/

ids_numericos([X|Resto],[X|NumericosEnResto]):-
    number(X),
    ids_numericos(Resto,NumericosEnResto),
    !.
ids_numericos([_|Resto],NumericosEnResto):-
    ids_numericos(Resto,NumericosEnResto),
    !.
ids_numericos([],[]).

/*
 * Niega la lista las propiedades o relaciones de una lista.
 * true si la ListaNegada contiene las mismas propiedades o relaciones que Lista pero en su forma negada.
 * niega_lista(Lista,ListaNegada)
 */

/*
 * Primero eliminamos doble negación para que todo funcione mejor
 */
niega_lista([not(not(X))|Resto],Res):-
    niega_lista([X|Resto],Res),!.
/*
 * Caso not(X) se convierte en X
 */
niega_lista([not(X)|Resto],[X|RestoNegado]):-
    niega_lista(Resto,RestoNegado).
/*
 * Caso X se convierte en not(X)
 */
niega_lista([X|Resto],[not(X)|RestoNegado]):-
    niega_lista(Resto,RestoNegado).
/*
 * Caso base.
 * Si ya no hay nada ya acabamos.
 */
niega_lista([],[]).

/*
 *
 * Fusiona los atibutos contenidos en una lista respetando el orden de especificidad
 * fusiona([bonita,color=>blanco,color=>val([azul,negro]),not(color=>rojo),not(bonita)],Res).
 * Res = [color=>blanco, bonita, not(color=>[rojo])].
 */

fusiona(Lista,ListaFusionada):-
    clasifica(Lista,[],[],Positivas_,NegativasPositivas_),
    delete(Positivas_,_=>[],Positivas),
    delete(NegativasPositivas_,_=>[],NegativasPositivas),
    niega_lista(NegativasPositivas,Negativas),
    append(Positivas,Negativas,ListaFusionada).
