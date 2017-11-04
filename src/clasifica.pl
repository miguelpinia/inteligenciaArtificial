/*
 * clasifica(Lista,Positivas,Negativas)
 * Categoriza las propiedades o relaciones de Lista en Negativas y positivas repetando la especificidad
 */
clasifica(Lista,Positivas,Negativas):-
    clasifica(Lista,[],[],Positivas_,NegativasPositivas_),
    delete(Positivas_,_=>[],Positivas),
    delete(NegativasPositivas_,_=>[],NegativasPositivas),
    niega_lista(NegativasPositivas,Negativas).

/*
* clasifica(Lista,PositivasEncontradas,NegativasEncontradas,Positivas,Negativas)
* Categoriza las propiedades o relaciones de Lista en Negativas y positivas repetando la especificidad
* Lista una lista con las propiedades o relaciones posibles a agregar (solo en caso de no estar ya agregadas anteriormente)
* PositivasEncontradas Las propiedades o relaciones positivas que ya se encontraron
* NegativasEncontradas Las propiedades o relaciones negativas que ya se encontraron
*/

clasifica([],PositivasEncontradas,NegativasEncontradas,PositivasEncontradas,NegativasEncontradas):-!.

/* Caso atributo=>ListaDeValores */

clasifica([not(not(X))|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	!,
	clasifica([X|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas).

clasifica([not(Atr=>ValoresNuevos)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	is_list(ValoresNuevos),
	primera(Atr=>ValoresPositivos,PositivasEncontradas,Atr=>ValoresPositivos),
	is_list(ValoresPositivos),
	primera(Atr=>ValoresNegativos,NegativasEncontradas,Atr=>ValoresNegativos),
	is_list(ValoresNegativos),
	!,
	subtract(ValoresNuevos,ValoresPositivos,NuevosMenosPositivos),
	subtract(NuevosMenosPositivos,ValoresNegativos,NuevosMenosPositivosMenosNegativos),
	append(ValoresNegativos,NuevosMenosPositivosMenosNegativos,NuevosNegativos),
	reemplaza(NegativasEncontradas,Atr=>ValoresNegativos,Atr=>NuevosNegativos,NuevasNegativasEncontradas),
	clasifica(Restantes,PositivasEncontradas,NuevasNegativasEncontradas,Positivas,Negativas).

clasifica([not(Atr=>ValoresNuevos)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	is_list(ValoresNuevos),
	primera(Atr=>ValoresPositivos,PositivasEncontradas,Atr=>ValoresPositivos),
	is_list(ValoresPositivos),
	!,
	subtract(ValoresNuevos,ValoresPositivos,NuevosMenosPositivos),
	clasifica(
		Restantes,
		PositivasEncontradas,
		[Atr=>NuevosMenosPositivos|NegativasEncontradas],
		Positivas,
		Negativas).
	
clasifica([not(Atr=>ValoresNuevos)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	is_list(ValoresNuevos),
	primera(Atr=>ValoresNegativos,NegativasEncontradas,Atr=>ValoresNegativos),
	is_list(ValoresNegativos),
	!,
	subtract(ValoresNuevos,ValoresNegativos,NuevosNegativos),
	reemplaza(NegativasEncontradas,Atr=>ValoresNegativos,Atr=>NuevosNegativos,NuevasNegativasEncontradas),
	clasifica(Restantes,PositivasEncontradas,NuevasNegativasEncontradas,Positivas,Negativas).
	
clasifica([not(Atr=>ValoresNuevos)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	is_list(ValoresNuevos),
	!,
	clasifica(
		Restantes,
		PositivasEncontradas,
		[Atr=>ValoresNuevos|NegativasEncontradas],
		Positivas,
		Negativas).


clasifica([Atr=>ValoresNuevos|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	is_list(ValoresNuevos),
	primera(Atr=>ValoresPositivos,PositivasEncontradas,Atr=>ValoresPositivos),
	is_list(ValoresPositivos),
	primera(Atr=>ValoresNegativos,NegativasEncontradas,Atr=>ValoresNegativos),
	is_list(ValoresNegativos),
	!,
	subtract(ValoresNuevos,ValoresPositivos,NuevosMenosPositivos),
	subtract(NuevosMenosPositivos,ValoresNegativos,NuevosMenosPositivosMenosNegativos),
	append(ValoresPositivos,NuevosMenosPositivosMenosNegativos,NuevosPositivos),
	reemplaza(PositivasEncontradas,Atr=>ValoresPositivos,Atr=>NuevosPositivos,NuevasPositivasEncontradas),
	clasifica(Restantes,NuevasPositivasEncontradas,NegativasEncontradas,Positivas,Negativas).

clasifica([Atr=>ValoresNuevos|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	is_list(ValoresNuevos),
	primera(Atr=>ValoresPositivos,PositivasEncontradas,Atr=>ValoresPositivos),
	is_list(ValoresPositivos),
	!,
	subtract(ValoresNuevos,ValoresPositivos,NuevosMenosPositivos),
	append(ValoresPositivos,NuevosMenosPositivos,NuevosPositivos),
	reemplaza(PositivasEncontradas,Atr=>ValoresPositivos,Atr=>NuevosPositivos,NuevasPositivasEncontradas),
	clasifica(Restantes,NuevasPositivasEncontradas,NegativasEncontradas,Positivas,Negativas).
	
clasifica([Atr=>ValoresNuevos|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	is_list(ValoresNuevos),
	primera(Atr=>ValoresNegativos,NegativasEncontradas,Atr=>ValoresNegativos),
	is_list(ValoresNegativos),
	!,
	subtract(ValoresNuevos,ValoresNegativos,NuevosMenosNegativos),
	clasifica(Restantes,[Atr=>NuevosMenosNegativos|PositivasEncontradas],NegativasEncontradas,Positivas,Negativas).
	
clasifica([Atr=>ValoresNuevos|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	is_list(ValoresNuevos),
	!,
	clasifica(
		Restantes,
		[Atr=>ValoresNuevos|PositivasEncontradas],
		NegativasEncontradas,
		Positivas,
		Negativas).

/* 
* Caso menos complicado valor=>atributo
* Puede haber muchos negados pero solo un positivo
*/

clasifica([not(Atr=>Valor)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	not(is_list(Valor)),
	not(member(Atr=>Valor,PositivasEncontradas)),
	not(member(Atr=>_,NegativasEncontradas)),
	!,
	clasifica(Restantes,PositivasEncontradas,[Atr=>[Valor]|NegativasEncontradas],Positivas,Negativas).

clasifica([not(Atr=>Valor)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	not(is_list(Valor)),
	not(member(Atr=>Valor,PositivasEncontradas)),
	primera(Atr=>ValoresNegativos,NegativasEncontradas,Atr=>ValoresNegativos),
	not(member(Valor,ValoresNegativos)),
	reemplaza(NegativasEncontradas,Atr=>ValoresNegativos,Atr=>[Valor|ValoresNegativos],NuevasNegativasEncontradas),
	!,
	clasifica(Restantes,PositivasEncontradas,NuevasNegativasEncontradas,Positivas,Negativas).

clasifica([not(_=>_)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	!,
	clasifica(Restantes,PositivasEncontradas,NegativasEncontradas,Positivas,Negativas).

clasifica([Atr=>Valor|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	not(is_list(Valor)),
	not(member(Atr=>_,PositivasEncontradas)),
	primera(Atr=>ValoresNegativos,NegativasEncontradas,Atr=>ValoresNegativos),
	not(member(Valor,ValoresNegativos)),
	!,
	clasifica(Restantes,[Atr=>Valor|PositivasEncontradas],NegativasEncontradas,Positivas,Negativas).

clasifica([Atr=>Valor|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	not(is_list(Valor)),
	not(member(Atr=>_,PositivasEncontradas)),
	not(primera(Atr=>_,NegativasEncontradas,Atr=>_)),
	!,
	clasifica(Restantes,[Atr=>Valor|PositivasEncontradas],NegativasEncontradas,Positivas,Negativas).

clasifica([_=>_|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	!,
	clasifica(Restantes,PositivasEncontradas,NegativasEncontradas,Positivas,Negativas).


/*Caso propiedades simples propiedad o not(propiedad)*/

clasifica([not(Atr)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	not(member(Atr,PositivasEncontradas)),
	not(member(Atr,NegativasEncontradas)),
	clasifica(Restantes,PositivasEncontradas,[Atr|NegativasEncontradas],Positivas,Negativas),
	!.
clasifica([not(_)|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	clasifica(Restantes,PositivasEncontradas,NegativasEncontradas,Positivas,Negativas),
	!.
clasifica([Atr|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	not(member(Atr,PositivasEncontradas)),
	not(member(Atr,NegativasEncontradas)),
	clasifica(Restantes,[Atr|PositivasEncontradas],NegativasEncontradas,Positivas,Negativas),
	!.
clasifica([_|Restantes],PositivasEncontradas,NegativasEncontradas,Positivas,Negativas):-
	clasifica(Restantes,PositivasEncontradas,NegativasEncontradas,Positivas,Negativas).
	



