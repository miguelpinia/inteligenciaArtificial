% Ejecución en bash:
% swipl -s prueba_simulador.pl -t 'halt(0)'

:- [main, agrega, consulta, elimina, modifica, util, decision, diagnostico, planeacion, simulador].

:- write('-----------------------Evaluación ejercicio 1-------------------'),nl,
   write('a) El robot observa el refresco y la cerveza en el estante de bebidas.'), nl, nl, nl.


:- write('open_kb(\'ejercicios/ejercicio1.a.txt\',KB), simulador(KB).'), nl, nl, nl.
:- open_kb('ejercicios/ejercicio1.a.txt',KB), simulador(KB).

:- write('-----------------------Evaluación ejercicio 2-------------------'),nl,
   write('b) El robot observa el refresco, la cerveza y la sopa en el estante de bebidas.'), nl, nl, nl.

:- write('open_kb(\'ejercicios/ejercicio1.b.txt\',KB), simulador(KB).'), nl, nl, nl.
:- open_kb('ejercicios/ejercicio1.b.txt',KB), simulador(KB).

:- write('-----------------------Evaluación ejercicio 3-------------------'), nl,
   write('c) El robot observa el refresco, la cerveza, la sopa, las galletas y el cereal en el estante de bebidas.'),nl, nl, nl.

:- write('open_kb(\'ejercicios/ejercicio1.c.txt\',KB), simulador(KB).'), nl, nl, nl.
:- open_kb('ejercicios/ejercicio1.c.txt',KB), simulador(KB).

:- write('-----------------------Evaluación ejercicio 4-------------------'), nl,
   write('d) El robot observa la cerveza y la sopa en el estante de bebidas.'), nl, nl, nl.

:- write('open_kb(\'ejercicios/ejercicio1.d.txt\',KB), simulador(KB).'), nl, nl, nl.
:- open_kb('ejercicios/ejercicio1.d.txt',KB), simulador(KB).

:- write('-----------------------Evaluación ejercicio 5-------------------'),nl,
   write('e) El robot observa la cerveza, la sopa y el cereal en el estante de bebidas.'),nl, nl, nl.

:- write('open_kb(\'ejercicios/ejercicio1.e.txt\',KB), simulador(KB).'), nl, nl, nl.
:- open_kb('ejercicios/ejercicio1.e.txt',KB), simulador(KB).

:- write('-----------------------Evaluación ejercicio 6-------------------'),nl,
   write('f) El robot observa que el estante de bebidas se encuentra vacío.'),nl, nl, nl.

:- write('open_kb(\'ejercicios/ejercicio1.f.txt\',KB), simulador(KB).'), nl, nl, nl.
:- open_kb('ejercicios/ejercicio1.f.txt',KB), simulador(KB).

:- write('-----------------------Evaluación ejercicio 7-------------------'), nl,
   write('g) El robot observa las galletas y la sopa en el estante de bebidas.'),nl, nl, nl.

:- write('open_kb(\'ejercicios/ejercicio1.g.txt\',KB), simulador(KB).'), nl, nl, nl.
:- open_kb('ejercicios/ejercicio1.g.txt',KB), simulador(KB).

:- write('-----------------------Evaluación ejercicio 8-------------------'), nl,
   write('h) El robot observa las galletas en el estante de bebidas.'),nl, nl, nl.

:- write('open_kb(\'ejercicios/ejercicio1.h.txt\',KB), simulador(KB).'), nl, nl, nl.
:- open_kb('ejercicios/ejercicio1.h.txt',KB), simulador(KB).

:- write('-----------------------Evaluación ejercicio 9-------------------'),nl,
   write('i) El robot observa la sopa y el cereal en el estante de bebidas.'),nl, nl, nl.

:- write('open_kb(\'ejercicios/ejercicio1.i.txt\',KB), simulador(KB).'), nl, nl, nl.
:- open_kb('ejercicios/ejercicio1.i.txt',KB), simulador(KB).

:- write('-----------------------Evaluación ejercicio 10------------------'),nl,
    write('j) El robot observa la cerveza, la sopa, las galletas y el cereal en el estante de bebidas.'),nl, nl, nl.

:- write('open_kb(\'ejercicios/ejercicio1.j.txt\',KB), simulador(KB).'), nl, nl, nl.
:- open_kb('ejercicios/ejercicio1.j.txt',KB), simulador(KB).

:- write('-----------------------Fin de la Evaluación------------------'), nl, nl, nl.
