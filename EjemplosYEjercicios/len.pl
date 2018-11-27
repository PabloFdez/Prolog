length(Xs,N):-
	var(Xs), integer(N), length_num(N,Xs).

%En el caso de que la lista que se le pase sea una variable
%crea una lista de _ de longitud N
%	?- length(A,4).
%	A = [_,_,_,_] ? 

length(Xs,N):-
	nonvar(Xs), length_list(Xs,N).

%En el caso de que la longitud que se le pase sea una variable
%recorre la lista y devuelve su longitud
%	?- length([a,b,c],R).
%	R = 3 ? 

length_num(0,[]).
length_num(N,[_|Xs]):-
	N > 0, N1 is N - 1, length_num(N1,Xs).

length_list([],0).
length_list([X|Xs],N):-
	length_list(Xs,N1), N is N1 + 1.