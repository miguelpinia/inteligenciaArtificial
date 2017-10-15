%--------------------------------------------------
% Load and Save from files
%--------------------------------------------------


%KB open and save

open_kb(Route,KB):-
	open(Route,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term(X,KB).

save_kb(Route,KB):-
	open(Route,write,Stream),
	writeq(Stream,KB),
	close(Stream).

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars).

checkCharAndReadRest(-1,[],_) :- !.  % End of Stream
checkCharAndReadRest(end_of_file,[],_) :- !.

checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).

%compile an atom string of characters as a prolog term
atom_to_term(ATOM, TERM) :-
	atom(ATOM),
	atom_to_chars(ATOM,STR),
	atom_to_chars('.',PTO),
	append(STR,PTO,STR_PTO),
	read_from_chars(STR_PTO,TERM).

:- op(800,xfx,'=>').

/*
* assertall(ListOfTerms)
* carga dinamicamente todos los Terminos de ListOfTerms
*/

assertall([]).
assertall([Head|Tail]):-assertz(Head),assertall(Tail).

/*
* assertKB(KB)
* Predicado auxiliar usado al cargar una KB de un archivo.
* Povoca la asserci+on del predicado kb(KB)
* que funciona para tener en toda la consulta la base de datos a la mano.
* si hay definido algun predicado kb de aridad 1 lo eliminará
* sólo debe haber uno durante toda la consulta.
*/

assertKB(KB):-
	atom_chars('kb(',Pre),
	term_to_atom(KB,Atom),
	atom_chars(Atom,Chars),
	append(Pre,Chars,PreChars),
	atom_chars(')',Post),
	append(PreChars,Post,PreCharsPost),
	read_from_chars(PreCharsPost,Term),	
	retractall(kb(_)),
	assert(Term).

/*
* loadKB(File)
* Carga la KB guardada en File
* Sobreescribe la KB existente
*/
loadKB(File):-
	loadKB(File,1),
	!.

/*
* loadKB(File,Overwrite)
* Carga la KB guardada en File
* Si Overwrite = 1 reescribe la base de datos
* Si es Overwrite = 0 agrega el conocimiento a la KB actual.
*/

loadKB(File,1):-
	open_kb(File,KB),
	retractall(clase(_)),
	retractall(objeto(_)),
	assertKB(KB),
	assertall(KB),
	!.

loadKB(File,0):-
	open_kb(File,NKB),
	kb(CKB),
	append(CKB,NKB,KB),
	assertKB(KB),
	assertall(KB),
	!.

/*
* Guarda en File la KB actual
*/
storeKB(File):-
	kb(KB),
	save_kb(File,KB),
	!.

