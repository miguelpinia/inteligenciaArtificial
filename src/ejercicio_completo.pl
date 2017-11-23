% Ejecución en bash:
% swipl -s ejercicio_completo.pl -t 'halt(0)'
:- [main, agrega, consulta, elimina, modifica, util, decision, diagnostico, planeacion, simulador].

:- write('% -----------------------Evaluación ejercicio 1-------------------'),nl.

:- write('% a) El robot observa el refresco y la cerveza en el estante de bebidas.'), nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/kb_tmp.txt\', drink_shelf, [coca,heineken]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/kb_tmp.txt', drink_shelf, [coca,heineken]).

:- write('% b) El robot observa el refresco, la cerveza y la sopa en el estante de bebidas.'), nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/kb_tmp.txt\', drink_shelf, [coca,heineken,noodles]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/kb_tmp.txt', drink_shelf, [coca,heineken,noodles]).

:- write('% c) El robot observa el refresco, la cerveza, la sopa, las galletas y el cereal en el estante de bebidas.'),nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/kb_tmp.txt\', drink_shelf, [coca,heineken,noodles,bisquits,kellogs]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/kb_tmp.txt', drink_shelf, [coca,heineken,noodles,bisquits,kellogs]).

:- write('% d) El robot observa la cerveza y la sopa en el estante de bebidas.'), nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/kb_tmp.txt\', drink_shelf, [heineken,noodles]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/kb_tmp.txt', drink_shelf, [heineken,noodles]).

:- write('% e) El robot observa la cerveza, la sopa y el cereal en el estante de bebidas.'),nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/kb_tmp.txt\', drink_shelf, [heineken,noodles,kellogs]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/kb_tmp.txt', drink_shelf, [heineken,noodles,kellogs]).

:- write('% f) El robot observa que el estante de bebidas se encuentra vacío.'),nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/kb_tmp.txt\', drink_shelf, []).'), nl, nl, nl.
:- simula_un_paso('ejercicios/kb_tmp.txt', drink_shelf, []).

:- write('% g) El robot observa las galletas y la sopa en el estante de bebidas.'),nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/kb_tmp.txt\', drink_shelf, [bisquits,noodles]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/kb_tmp.txt', drink_shelf, [bisquits,noodles]).

:- write('% h) El robot observa las galletas en el estante de bebidas.'),nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/kb_tmp.txt\', drink_shelf, [bisquits]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/kb_tmp.txt', drink_shelf, [bisquits]).

:- write('% i) El robot observa la sopa y el cereal en el estante de bebidas.'),nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/kb_tmp.txt\', drink_shelf, [noodles,kellogs]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/kb_tmp.txt', drink_shelf, [noodles,kellogs]).

:- write('% j) El robot observa la cerveza, la sopa, las galletas y el cereal en el estante de bebidas.'),nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/kb_tmp.txt\', drink_shelf, [heineken,noodles,bisquits,kellogs]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/kb_tmp.txt', drink_shelf, [heineken,noodles,bisquits,kellogs]).

:- nl, write('% -----------------------Evaluación ejercicio 2-------------------'), nl.
:- write('% a) El robot observa el refresco, la sopa y el cereal en el estante de comida.'),nl, nl, nl.
:- write('?- simula_dos_pasos(\'ejercicios/kb_tmp.txt\',[drink_shelf,food_shelf],[[heineken],[coca,noodles,kellogs]]).'),nl,nl,nl.
:- simula_dos_pasos('ejercicios/kb_tmp.txt',[drink_shelf,food_shelf],[[heineken],[coca,noodles,kellogs]]).

:- write('% b) El robot observa la sopa y el cereal en el estante de comida.'),nl, nl, nl.
:- write('?- simula_dos_pasos(\'ejercicios/kb_tmp.txt\',[drink_shelf,food_shelf],[[heineken],[noodles,kellogs]]).'),nl,nl,nl.
:- simula_dos_pasos('ejercicios/kb_tmp.txt',[drink_shelf,food_shelf],[[heineken],[noodles,kellogs]]).

:- write('% c) El robot observa el cereal, la sopa y las galletas en el estante de comida.'),nl, nl, nl.
:- write('?- simula_dos_pasos(\'ejercicios/kb_tmp.txt\',[drink_shelf,food_shelf],[[heineken],[kellogs,noodles,bisquits]]).'),nl,nl,nl.
:- simula_dos_pasos('ejercicios/kb_tmp.txt',[drink_shelf,food_shelf],[[heineken],[noodles,kellogs,bisquits]]).

:- nl, write('% -----------------------Evaluación ejercicio 3-------------------'), nl.

:- write('% a) El robot observa las galletas y la sopa en el estante de bebidas. El estante de pan está muy lejos del estante de bebidas. El estante de comida está muy cerca del estante de bebidas.'),nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/ejercicio3.a.txt\', drink_shelf, [bisquits,noodles]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/ejercicio3.a.txt', drink_shelf, [bisquits,noodles]).

:- write('% b) El robot observa las galletas y la sopa en el estante de bebidas. El estante de pan está muy cerca del estante de bebidas. El estante de comida está muy lejos del estante de bebidas.'),nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/ejercicio3.b.txt\', drink_shelf, [bisquits,noodles]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/ejercicio3.b.txt', drink_shelf, [bisquits,noodles]).

:- write('% c) El robot observa las galletas y la sopa en el estante de bebidas. Las galletas son muy difíciles de agarrar. La sopa es muy fácil de agarrar.'),nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/ejercicio3.c.txt\', drink_shelf, [bisquits,noodles]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/ejercicio3.c.txt', drink_shelf, [bisquits,noodles]).

:- write('% d) El robot observa las galletas y la sopa en el estante de bebidas. Las galletas son muy fáciles de agarrar. La sopa es muy difícil de agarrar.'),nl, nl, nl.
:- write('?- simula_un_paso(\'ejercicios/ejercicio3.d.txt\', drink_shelf, [bisquits,noodles]).'), nl, nl, nl.
:- simula_un_paso('ejercicios/ejercicio3.d.txt', drink_shelf, [bisquits,noodles]).

:- nl, write('% -----------------------Evaluación ejercicio 3-------------------'), nl.
:- write('% Simulación story board'),nl, nl, nl.
:- write('?- open_kb(\'kb_proy2.txt\',KB), simulador(KB).'), nl, nl, nl.
:- open_kb('kb_proy2.txt',KB), simulador(KB).