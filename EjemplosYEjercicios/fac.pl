factorial(0,1).
factorial(N,F):-
N > 0,
N1 is N-1,

%OJO
%Si se pone el F is F1* antes de volver a llamar a factorial
%Da un error: {ERROR: arithmetic:is/2, arg 2 - instantiation_error}
%En las operaciones aritméticas el F1 debe estar calculado antes de hacerla

factorial(N1,F1),
F is F1*.