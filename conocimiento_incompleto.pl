/*
* objeto_tiene_propiedad(Ref,Prop,Respuesta,Valor)
* Calcula las propiedades del objeto con referencia Ref,
* clasifica el conocimiento en positivo y negativo,
* y determina si la propiedad está o no en la base de conocimiento.
* Respuesta puede ser 'Si', 'No' o 'No se'
* Valor corresponde a la información que se tenga o udf en caso de no existir.
*/

objeto_tiene_propiedad(Ref,Prop,Respuesta,Valor):-
	propiedades_de_objeto_clasificadas(Ref,Positivas,Negativas),
	tiene(Positivas,Negativas,Prop,Respuesta,Valor).

/*
* clase_tiene_propiedad(Ref,Prop,Respuesta,Valor)
* Calcula las propiedades de la clase Clase,
* clasifica el conocimiento en positivo y negativo,
* y determina si la propiedad está o no en la base de conocimiento.
* Respuesta puede ser 'Si', 'No' o 'No se'
* Valor corresponde a la información que se tenga o udf en caso de no existir.
*/

clase_tiene_propiedad(Clase,Prop,Respuesta,Valor):-
	propiedades_de_clase_clasificadas(Clase,Positivas,Negativas),
	tiene(Positivas,Negativas,Prop,Respuesta,Valor).

/*
* objeto_tiene_relacion(Ref,Rel,Respuesta,Valor)
* Calcula las relaciones del objeto con referencia Ref,
* clasifica el conocimiento en positivo y negativo,
* y determina si la propiedad está o no en la base de conocimiento.
* Respuesta puede ser 'Si', 'No' o 'No se'
* Valor corresponde a la información que se tenga o udf en caso de no existir.
*/

objeto_tiene_relacion(Ref,Rel,Respuesta,Valor):-
	relaciones_de_objeto_clasificadas(Ref,Positivas,Negativas),
	tiene(Positivas,Negativas,Rel,Respuesta,Valor).
	

/*
* clase_tiene_relacion(Clase,Rel,Respuesta)
* Calcula las relaciones de laclase Clase,
* clasifica el conocimiento en positivo y negativo,
* y determina si la propiedad está o no en la base de conocimiento.
* Respuesta puede ser 'Si', 'No' o 'No se'
* Valor corresponde a la información que se tenga o udf en caso de no existir.
*/
clase_tiene_relacion(Clase,Rel,Respuesta,Valor):-
	relaciones_de_clase_clasificadas(Clase,Positivas,Negativas),
	tiene(Positivas,Negativas,Rel,Respuesta,Valor).	

/* 
* Obtiene las propiedes del objeto con referencia Ref por órden de especificidad y las clasifica en positivas o negativas
*/
propiedades_de_objeto_clasificadas(Ref,Positivas,Negativas):-
	propiedades_de_objeto(Ref,Propiedades),
	clasifica(Propiedades,[],[],Positivas,Negativas).
/* 
* Obtiene las propiedes de la clase con referencia Ref por órden de especificidad y las clasifica en positivas o negativas
*/
propiedades_de_clase_clasificadas(Clase,Positivas,Negativas):-
	propiedades_de_clase(Clase,Propiedades),
	clasifica(Propiedades,[],[],Positivas,Negativas).
/* 
* Obtiene las relaciones del objeto con referencia Ref por órden de especificidad y las clasifica en positivas o negativas
*/
relaciones_de_objeto_clasificadas(Ref,Positivas,Negativas):-
	relaciones_de_objeto(Ref,Relaciones),
	clasifica(Relaciones,[],[],Positivas,Negativas).
/* 
* Obtiene las relaciones de la clase Clase por órden de especificidad y las clasifica en positivas o negativas
*/
relaciones_de_clase_clasificadas(Clase,Positivas,Negativas):-
	relaciones_de_clase(Clase,Relaciones),
	clasifica(Relaciones,[],[],Positivas,Negativas).

/*
* tiene(Positivas,Negativas,Atr,Respuesta,Valor)
* Atr es de la forma Atr o not(Atr)
* Atr puede representar una propiedad o una relación
* Positivas es una lista de conocimiento positivo
* Negativas es una lista de conocimiento negativo
* Respuesta puede ser 'Si', 'No' o 'No se' dependiendo del conocimiento
* Valor es el valor que se conoce o udf en caso de no tener información.
*/

tiene(Positivas,Negativas,not(not(X)),Respuesta,Valor):-
	!,
	tiene(Positivas,Negativas,X,Respuesta,Valor).

tiene(_,Negativas,not(Atr),'Si',Valor):-
	member(Atr=>Valor,Negativas),
	!.
/*
tiene(Positivas,Negativas,not(Atr),'No',udf):-
	not(member(Atr=>_,Negativas)),
	member(Atr=>_,Positivas),
	!.
*/
tiene(_,Negativas,not(Atr),'Si','No'):-
	member(Atr,Negativas),
	!.

tiene(Positivas,Negativas,not(Atr),'No','Si'):-
	not(member(Atr,Negativas)),
	member(Atr,Positivas),
	!.

tiene(Positivas,_,Atr,'Si',Valor):-
	member(Atr=>Valor,Positivas),
	!.
/*
tiene(Positivas,Negativas,Atr,'No',udf):-
	not(member(Atr=>_,Positivas)),
	member(Atr=>_,Negativas),
	!.
*/
tiene(Positivas,_,Atr,'Si','Si'):-
	member(Atr,Positivas),
	!.

tiene(Positivas,Negativas,Atr,'No','No'):-
	not(member(Atr,Positivas)),
	member(Atr,Negativas),
	!.

tiene(_,_,_,'No se',udf).
