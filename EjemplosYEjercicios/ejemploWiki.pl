%%
%% declaraciones
%%
padrede('juan', 'maria'). % juan es padre de maria
padrede('pablo', 'juan'). %pablo es padre de juan
padrede('pablo', 'marcela'). % pablo es padre de marcela
padrede('carlos', 'debora'). % carlos es padre de debora

% A es hijo de B si B es padre de A
hijode(A,B) :- padrede(B,A).
% A es abuelo de B si A es padre de C y C es padre B
abuelode(A,B) :- 
   padrede(A,C), 
   padrede(C,B).
% A y B son hermanos si el padre de A es también el padre de B y si A y B no son lo mismo
hermanode(A,B) :- 
   padrede(C,A) , 
   padrede(C,B), 
   A \== B.        

% A y B son familiares si A es padre de B o A es hijo de B o A es hermano de B
familiarde(A,B) :- 
   padrede(A,B).
familiarde(A,B) :-
   hijode(A,B). 
familiarde(A,B) :- 
   hermanode(A,B).
%%
%% consultas
%%
% juan es hermano de marcela?
%?- hermanode('juan', 'marcela').
%yes

% carlos es hermano de juan?
%?- hermanode('carlos', 'juan').
%no

% pablo es abuelo de maria?
%?- abuelode('pablo', 'maria').
%yes

% maria es abuela de pablo?
%?- abuelode('maria', 'pablo').
%no