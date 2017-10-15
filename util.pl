/*
* construye_objeto_term(Ref,Clase,Props,Rels,Term)
* A partir de Ref,Clase,Props,Rels
* Construye el término objeto(o(Ref,Clase,Props,Rels))
* Donde Ref,Clase,Props,Rels se representan como su valor unificado
* esta construcción es unificada con Term
*/
construye_objeto_term(Ref,Clase,Props,Rels,Term):-
	atom_chars('objeto(o(',Pre),
	term_to_atom(Ref,Ref_),
	atom_chars(Ref_,Ref_c),
	term_to_atom(Clase,Clase_),
	atom_chars(Clase_,Clase_c),
	term_to_atom(Props,Props_),
	atom_chars(Props_,Props_c),
	term_to_atom(Rels,Rels_),
	atom_chars(Rels_,Rels_c),
	atom_chars(',',Coma),
	atom_chars('))',Suf),
	append(Pre,Ref_c,Pre2),
	append(Pre2,Coma,Pre3),
	append(Pre3,Clase_c,Pre4),
	append(Pre4,Coma,Pre5),
	append(Pre5,Props_c,Pre6),
	append(Pre6,Coma,Pre7),
	append(Pre7,Rels_c,Pre8),
	append(Pre8,Suf,Completo),
	read_from_chars(Completo,Term).


/*
* construye_clase_term(Clase,Super,Props,Rels,Objetos,Term)
* A partir de Clase,Super,Props,Rels,Objetos
* Construye un término clase(c(Clase,Super,Props,Rels,Objetos))
* Donde Clase,Super,Props,Rels,Objetos se representan como su valor unificado
* esta construcción es unificada con Term
*/
construye_clase_term(Clase,Super,Props,Rels,Objetos,Term):-
	atom_chars('clase(c(',Pre),
	term_to_atom(Clase,Clase_),
	atom_chars(Clase_,Clase_c),
	term_to_atom(Super,Super_),
	atom_chars(Super_,Super_c),
	term_to_atom(Props,Props_),
	atom_chars(Props_,Props_c),
	term_to_atom(Rels,Rels_),
	atom_chars(Rels_,Rels_c),
	term_to_atom(Objetos,Objetos_),
	atom_chars(Objetos_,Objetos_c),
	atom_chars(',',Coma),
	atom_chars('))',Suf),
	append(Pre,Clase_c,Pre2),
	append(Pre2,Coma,Pre3),
	append(Pre3,Super_c,Pre4),
	append(Pre4,Coma,Pre5),
	append(Pre5,Props_c,Pre6),
	append(Pre6,Coma,Pre7),
	append(Pre7,Rels_c,Pre8),
	append(Pre8,Coma,Pre9),
	append(Pre9,Objetos_c,Pre10),
	append(Pre10,Suf,Completo),
	read_from_chars(Completo,Term).

/*
* elimina_de_kb(Term)
* Elimina el término Term de la lista de predicados en KB
* con kb(KB)
* Al final recarga el predicado kb.
*/
elimina_de_kb(Term):-
	kb(KB),
	delete(KB,Term,NuevaKB),
	!,
	retractall(kb(_)),
	assertz(kb(NuevaKB)).

/*
* agrega_a_kb(Term)
* Agrega el término Term de la lista de predicados en KB
* con kb(KB)
* Al final recarga el predicado kb.
*/
agrega_a_kb(Term):-
	kb(KB),
	append([Term],KB,NuevaKB),
	retractall(kb(_)),
	assertz(kb(NuevaKB)).

/*
* reemplaza_en_kb(Term)
* Reemplaza el término TermViejo por TermNuevo en la lista de predicados de KB
* con kb(KB)
* Al final recarga el predicado kb.
*/
reemplaza_en_kb(TermViejo,TermNuevo):-
    kb(KB),
    reemplaza(KB,TermViejo,TermNuevo,NuevaKB),
    retractall(kb(_)),
	assertz(kb(NuevaKB)).

/*
* Sobreescribe la base de datos con los términos en KB con kb(KB)
*/

reloadKB(KB):-
	kb(KB),
	!.

reloadKB(KB):-
	retractall(clase(_)),
	retractall(objeto(_)),
	assertKB(KB),
	assertall(KB).



/*
* ref_to_obj(Ref,Obj)
* Relaciona la referencia de un objeto con su construcción completa.
*/
ref_to_obj(Ref,o(Ref,Clase,Propiedades,Relaciones)):-
    objeto(o(Ref,Clase,Propiedades,Relaciones)),
    !.

/*
* listRef_listObj(Referencias,Objetos)
* Relaciona Una lista de referencias de objetos a una lista con los objetos correspondientes a esas referencias
*/
listRef_listObj([],[]).
listRef_listObj([Ref|RestRels],[Obj|RestObjs]):-
    ref_to_obj(Ref,Obj),
    !,
    listRef_listObj(RestRels,RestObjs).

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

/* Para imprimir listas*/
imprime([]).
imprime([H|T]):-
    write(H),
    nl,
    imprime(T).






/*
* busca_en_lista(Atr,Lista,Encontradas)
* Busca las apariciones del atributo Atr en la lista Lista.
* Encontradas es la lista de todas las ocurrencias de Atr posiblemente con su respectivo valor.
* Atr es de la forma
* Atr not(Atr)
* Atr=>Valor(es)
* not(Atr=>Valor(es))
*/
busca_en_lista(_,[],[]):-!.
/*casos not(ATR) y not(Atr=>Valor(es))*/
busca_en_lista(not(Atr),[not(Atr=>Valor)|Resto],[not(Atr=>Valor)|EncontradasEnResto]):-
    !,
    busca_en_lista(not(Atr),Resto,EncontradasEnResto).
busca_en_lista(not(Atr),[not(Atr)|Resto],[not(Atr)|EncontradasEnResto]):-
    !,
    busca_en_lista(not(Atr),Resto,EncontradasEnResto).
/*Casos Atr y Atr=>Valor(es)*/
busca_en_lista(Atr,[Atr=>Valor|Resto],[Atr=>Valor|EncontradasEnResto]):-
    !,
    busca_en_lista(Atr,Resto,EncontradasEnResto).
busca_en_lista(Atr,[Atr|Resto],[Atr|EncontradasEnResto]):-
    !,
    busca_en_lista(Atr,Resto,EncontradasEnResto).
/* En otro caso*/
busca_en_lista(Atr,[_|Resto],EncontradasEnResto):-
    busca_en_lista(Atr,Resto,EncontradasEnResto).

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
