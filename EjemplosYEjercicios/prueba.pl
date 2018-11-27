%(load <CIAOLIBDIR>/ciao-mode-init)
% (load <CIAOLIBDIR>/DOTemacs)
   
% Este es mi primer programa en Prolog
% Se trata de un arbol genealogico muy simple
% Primero defino los parentescos basicos
% de la familia.
% padre(A,B) significa que B es el padre de A...
    
 padre(juan,alberto).
 padre(luis,alberto).
 padre(alberto,leoncio).
 padre(geronimo,leoncio).
 padre(luisa,geronimo).
    
% Ahora defino las condiciones para que dos individuos sean hermanos
% hermano(A,B) significa que A es hermano de B...
 
 hermano(A,B) :-
 padre(A,P),
 padre(B,P),
 A \== B.
    
% Ahora defino el parentesco abuelo-nieto.
% nieto(A,B) significa que A es nieto de B...

 nieto(A,B) :-
 padre(A,P),
 padre(P,B). 


% Para consultar este fichero en el shell:
%  1- Abrir la consola
%  2- Situarse donde se encuantra el fichero .pl
%  3- ciaosh (en la consola)
%  4- consult('prueba.pl'). (intro)
%  Si no nos situamos haria falta poner la ruta del fichero.

    
