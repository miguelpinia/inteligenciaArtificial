/*
* eliminar_objeto(KB,Ref,NuevaKB)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Elimina el objeto con referencia Ref de la base de datos, 
* de igual forma elimina Ref de la lista de referencias a objetos de su clase.
* NuevaKB es la KB resultante después de los cambios.
*/
elimina_objeto(KB,Ref,NuevaKB):-
    reloadKB(KB),
    elimina_objeto(Ref),
    kb(NuevaKB).

/*
* elimina_clase(KB,Clase,NuevaKB)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Elimina la clase Clase y a cambia la clase de sus objetos ejemplares 
* y clases hijas a su clase padre.
* No podemos eliminar top.
* NuevaKB es la KB resultante después de los cambios.
*/
elimina_clase(KB,Clase,NuevaKB):-
    reloadKB(KB),
    elimina_clase(Clase),
    kb(NuevaKB).

/*
* elimina_propiedad_de_objeto(KB,Ref,Prop,NuevaKB)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Elimina la propiedad Prop de la lista de propiedades perteneciente al objeto con referencia Ref
* NuevaKB es la KB resultante después de los cambios.
*/
elimina_propiedad_de_objeto(KB,Ref,Prop,NuevaKB):-
    reloadKB(KB),
    elimina_propiedad_de_objeto(Ref,Prop),
    kb(NuevaKB).
    
/*
* elimina_relacion_de_objeto(KB,Ref,Prop,NuevaKB)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Elimina la relación Rel de la lista de relaciones perteneciente al objeto con referencia Ref
* NuevaKB es la KB resultante después de los cambios.
*/
elimina_relacion_de_objeto(KB,Ref,Prop,NuevaKB):-
    reloadKB(KB),
    elimina_relacion_de_objeto(Ref,Prop),
    kb(NuevaKB).

/*
* elimina_propiedad_de_clase(KB,Clase,Prop,NuevaKB)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Elimina la propiedad Prop de la lista de propiedades perteneciente a la clase Clase
* NuevaKB es la KB resultante después de los cambios.
*/
elimina_propiedad_de_clase(KB,Clase,Prop,NuevaKB):-
    reloadKB(KB),
    elimina_propiedad_de_clase(Clase,Prop),
    kb(NuevaKB).

/*
* elimina_relacion_de_clase(KB,Clase,Rel,NuevaKB)
* Recarga la KB en caso de diferir con la que se tiene actualmente.
* Elimina la relación Rel de la lista de relaciones perteneciente a la clase Clase
* NuevaKB es la KB resultante después de los cambios.
*/
elimina_relacion_de_clase(KB,Clase,Rel,NuevaKB):-
    reloadKB(KB),
    elimina_relacion_de_clase(Clase,Rel),
    kb(NuevaKB).
