alumno_prode('Fernandez','Diaz','Pablo','V130148').


%Pablo Fernández Díaz v130148


%%%%%%%%%%%%%%%%%%  COMPRIMIR/2  %%%%%%%%%%%%%%%%%%
%comprimir de una lista vacía será siempre vacía

comprimir([],[]).

%comprimir de una lista Entrada será una lista con cabeza Elem y cola S si:
%	- contRep de una lista Entrada con cabeza Elem que se repite Rep veces y devuelve una
%	lista Entrada2 actualizada.
%	y
%	- Req es equal de s(0)
%	y
%	-comprimir de la lista actualizada Entrada2 es S.

comprimir(Entrada,[Elem|S]):-
	contRep(Entrada,Elem,Rep,Entrada2),
	equal(Rep,s(0)),
	comprimir(Entrada2,S).

%comprimir de una lista Entrada será una lista con cabeza rlec(Elem,Rep) y cola S si:
%	- contRep de una lista Entrada con cabeza Elem que se repite Rep veces y devuelve una
%	lista Entrada2 actualizada.
%	y
%	- Req es mayor que s(0)
%	y
%	-comprimir de la lista actualizada Entrada2 es S.

comprimir(Entrada,[rlec(Elem,Rep)|S]):-
	contRep(Entrada,Elem,Rep,Entrada2),
	mayor(Rep,s(0)),
	comprimir(Entrada2,S).

%%%%%%%%%%%%%%%%%%  contRep/4  %%%%%%%%%%%%%%%%%%
%contRep de una lista con un único elemento Elem, que se repite s(0) veces 
%y devuelve una lista vacía será siempre true.	

contRep([Elem|[]],Elem,s(0),[]).

%contRep de una lista [Elem,Elem2|Lis], con cabeza Elem, que se repite Rep veces, y 
%devuelve una lista L actualizada será true si:
%	- Elem unifica con Elem2
%	y
%	-contRep de una lista [Elem2|Lis], con cabeza Elem2, que se repite Rep veces, y 
%	devuelve una lista L actualizada

contRep([Elem,Elem2|Lis],Elem,s(Rep),L):-
	Elem = Elem2,
	contRep([Elem2|Lis],Elem2,Rep,L).

%contRep de una lista [Elem,Elem2|Lis], con cabeza Elem, que se repite Rep veces, y 
%devuelve la lista [Elem2|Lis] será true si:
%	- Elem NO unifica con Elem2
	
contRep([Elem,Elem2|Lis],Elem,s(0),[Elem2|Lis]):-
	Elem \= Elem2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  DESCOMPRIMIR/2  %%%%%%%%%%%%%%%%%%	
%descomprimir de una lista vacía será siempre vacía	

descomprimir([],[]).

%descomprimir de una lista con cabeza rlec(Elem,Rep) y cola Lis será una lista Res sí:
%	- Req es mayor que s(0)
%	y
%	-contar el elemento Elem, Rep veces, devuelve una lista L 
%	compuesta de dicho elemento reptido el numero de veces que se indica.
%	y
%	-hacer append de la lista L y un ResParcial devuelve Res.
%	y
%	-descomprimir de Lis devuelve ResParcial

descomprimir([rlec(Elem,Rep)|Lis],Res):-
	mayor(Rep,s(0)),  				
	contar(Rep,Elem,L),
	miAppend(L, ResParcial, Res),
	descomprimir(Lis,ResParcial).

%descomprimir de una lista con cabeza A y cola L será una lista con cabeza A
%y cola Res sí:
%	- A NO unifica con rlec(_,_)
%	y
%	-descomprimir de Lis devuelve Res

descomprimir([A|L],[A|Res]):-   	%Actualizar resultado
	A \= rlec(_,_),%Para que solo lo haga con letras
	descomprimir(L,Res).			%Las modificaciones aqui no sirven de nada, no se guarda

%%%%%%%%%%%%%%%%%%  contar/3  %%%%%%%%%%%%%%%%%%	
%contar de un elemento Elem repetido s(0) veces devuelve una L si:
%	-hacer append de Elem con una lista vacía devuelve una lista 
%	resultado con un único elemento Elem 

contar(s(0),Elem,L):-
	miAppend([Elem],[],L).

%contar de un elemento Elem repetido s(N) veces devuelve una L si:
%	- contar un elemento Elem repetido N veces devuelve una lista L1
%	y
%	-hacer append de Elem con la lista L1 devuelve una lista 
%	resultado L
	
contar(s(N),Elem, L):-
	contar(N,Elem,L1),
	miAppend([Elem],L1,L).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%%%%%%%%%%%%%%%%%%  SUMA/2  %%%%%%%%%%%%%%%%%%%%%%%%%%
%La suma los nodos y hojas de un arbol Arbol es un resultado Res si:
%	-recorrer un arbol Arbol devuleve una lista Lista con el valor de todos sus
%	nodos y hojas en ella
%	y
%	-sumarHojas de una lista Lista devuelve la suma de todos sus elementos en Res

suma(Arbol,Res):-
	recorrer(Arbol,Lista),
	sumarHojas(Lista,Res).

%%%%%%%%%%%%%%%%%%  sumarHojas/2  %%%%%%%%%%%%%%%%%%%%%%%%%%

%sumarHojas de una lista vacía es 0 :
	
sumarHojas([],0).

%sumarHojas de una lista con cabeza Cabeza y cola Cola tiene Res como resultado si:
%	-sumarHojas de una lista Cola tiene Res2 como resultado 
%	y
%	-hacer plus de Res2 y Cabeza devuelve Res

sumarHojas([Cabeza|Cola],Res):-
	sumarHojas(Cola,Res2),
	miPlus(Cabeza,Res2,Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  MENORES/2  %%%%%%%%%%%%%%%%%%%%%%%
%menores de un arbol Arbol con valor máximo Max es true si:
%	-recorrer un arbol Arbol devuleve una lista Lista con el valor de todos sus
%	nodos y hojas en ella
%	y
%	-menorAr de una lista Lista devuelve true si todos los elementos de dicha lista
%	son menores o iguales a Max

menores(Arbol,Max):-
	recorrer(Arbol,Lista),
	menorAr(Lista,Max).

%%%%%%%%%%%%%%%%%%  menorAr/2  %%%%%%%%%%%%%%%%%%%%%%%
%menorAr de un arbol con un único elemento A con valor máximo Max es true si:
%	-hacer menIgual del elemento A y Max devuelve true.

menorAr([A|[]],Max):-
	menIgual(A,Max).

%menorAr de una lista con cabeza C y cola L con valor máximo Max es true si:
%	-menorAr de la cabeza C con el valor máximo Max es true
%	y
%	-menorAr de la cola L con el valor máximo Max es true
	
menorAr([C|L],Max):-
	menIgual(C,Max),
	menorAr(L,Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%%%%%%%  Auxiliares  %%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  nat/1  %%%%%%%%%%%%%%%%%%%%%%%%%%%
% 0 siempre sera un natural

nat(0).

% El sucesor de X es natural si X lo es

nat(s(X)) :- nat(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  equal/2  %%%%%%%%%%%%%%%%%%%%%%%%%
%0 siempre es equal de 0

equal(0,0).

%el sucesor de X es equal del sucesor de Y si:
%	- X es equal de Y

equal(s(X),s(Y)) :-
	 equal(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  menIgual/2  %%%%%%%%%%%%%%%%%%%%%%
% 0 es menor o igual que X si:
%	- X es natural

menIgual(0,X) :-
	nat(X).

% el sucesor de X es menor o igual que el sucesor de Y si:
%	-  X es menor o igual que Y

menIgual(s(X),s(Y)) :-
	menIgual(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  mayor/2  %%%%%%%%%%%%%%%%%%%%%%%%%
% el sucesor de X es mayor que 0 si:
%	- X es un natural

mayor(s(X),0):-
	nat(X).

% el sucesor de X es mayor que el sucesor de Y si:
%	- X es mayor que Y

mayor(s(X),s(Y)) :-
	mayor(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  miPlus/3  %%%%%%%%%%%%%%%%%%%%%%%%%
%plus de X con 0 es X siempre que X sea un natural

miPlus(0,X,X):-
	nat(X).
	
%plus del sucesor de X con Y es igual al sucesor de Z si:
%	-plus de X con Y es Z	

miPlus(s(X),Y,s(Z)):-
	miPlus(X,Y,Z).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  miAppend/3  %%%%%%%%%%%%%%%%%%%%%%
%append de una lista vacía con Ys será siempre Ys	

miAppend([],Ys,Ys).

%append de una lista con cabeza X y cola Xs con Ys sera una
%lista con cabeza X y cola Zs si:
%	-append de Xs con Ys es Zs

miAppend([X|Xs],Ys,[X|Zs]) :- miAppend(Xs,Ys,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  lisNoVacia/1  %%%%%%%%%%%%%%%%%%%%
%lisNoVacia de A sera true si No unifica con lista vacía

lisNoVacia(A):-
	A\=[].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  recorrer/2  %%%%%%%%%%%%%%%%%%%%%%
% Predicado auxiliar utilizado en suma/2 y menores/2 para recorrer arboles
%recorrer de una lista vacía devuelve una lista vacía.
	
recorrer([],[]).

%recorrer de una hoja devuelve una lista con el valor de la hoja en ella

recorrer(hoja(N),[N]).

%recorrer de una lista con cabeza X y cola Y devuelve una lista Res si:
%	-recorrer de una lista X devuelve una lista ResCab
%	y
%	-recorrer de una lista Y devuelve una lista ResCola
%	y
%	-append de ResCab y ResCola devuelve Res
		
recorrer([X|Y],Res):-
	recorrer(X,ResCab),
	recorrer(Y, ResCola),
	miAppend(ResCab,ResCola,Res).

%recorrer un nodo con valor A y lista L devuelve una 
%lista con cabeza A y cola Res si:
%	- L es una lista NO vacía
%	y
%	- recorrer la lista L devuelve Res

recorrer(nodo(A,L),[A|Res]):-
	lisNoVacia(L),
	recorrer(L,Res).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
