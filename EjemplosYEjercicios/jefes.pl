:- module(_,_).

    jefe(juan,sonia).
    jefe(juan,susana).
    jefe(sonia,pedro).
    jefe(sonia,luis).
    jefe(susana,jaime).
    jefe(susana,sara).

% juan es jefe de sonia -> jefe(A,B) A es el jefe de B    

% Ahora escribirmos el predicado curritos
% Dos empleados son curritos si tienen el mismo jefe   

curritos(X,Y):-
    jefe(Z,X),
    jefe(Z,Y),
    X \= Y,
    X \= Z,
    Y \= Z.

 % Ahora escribimos el predicado jefazo
 % "Tiene que estar por encima en el arbol"

jefazo(X,Y):-
    jefe(X,Y).
    
jefazo(X,Y):-
    jefe(X,Z),
    jefe(Z,Y).

% En este caso jefazo(juan,A). nos lista a todos los empleados
% ya que juan es el jefe de todos.

    
    
