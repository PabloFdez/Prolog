/* suma de los elementos de una lista */ 
total([],0). 
total([C|L],T):- 
total(L,T1), 
T is T1+C. 

/* longitud de una lista */ 
lenght([],0). 
lenght([_|L],T):- 
lenght(L,T1), 
T is T1+1. 

/* adicionar un elemento de la cabeza de una lista */ 
addhead(X, L, [X|L]). 

/* borrar la cabeza de una lista*/ 
deletehead(L,L1):- 
addhead(_,L1,L). 

/* adicionar al final de una lista */ 
addend(X, [], [X]). 
addend(X, [C|R], [C|R1]):- 
addend(X, R, R1). 

/* borrar el ultimo elemento de una lista */ 
deleteend(L,L1):- 
addend(_,L1,L). 

/* borrar un elemento de una lista dado el indice */ 
delete(Indice,L,L1):- 
insert(_,Indice,L1,L). 

/* insertar un elemento en una lista dado el indice en que se quiere insertar*/ 
insert(X,0,L1,[X|L1]). 
insert(X,Pos,[C|R],[C|R2]):- 
Pos1 is Pos-1, 
insert(X,Pos1,R,R2). 

/* devuelve las posiciones en que se encuentra un elemento X*/ 
pos(X,[X|_],0). 
pos(_,[],_):- 
!,fail. 
pos(X,[_|R],Pos):- 
pos(X,R,Pos1), 
Pos is Pos1+1. 

/* clonar lista*/ 
clonlist([], []). 
clonlist([C|R], [C|R1]):- 
clonlist(R, R1). 
/* elemento X de una Lista*/ 
getElem(0,[C|_],C):-!. 
getElem(X,[_|R],Sol):- 
X1 is X -1, 
getElem(X1,R,Sol) 
. 

/* existencia de un elemento en una lista */ 
existe(_,[]):-fail. 
existe(X,[X|_]):-!. 
existe(X,[_|R]):- 
existe(X,R). 

/* elminar un elemento de la lista */ 
eliminar(_,[],[]):-fail. 
eliminar(X,[X|R],R). 
eliminar(X,[C|R],[C|R1]):- 
eliminar(X,R,R1) 
. 

/* subconjuntos de una lista */ 
subconjunto([],[]). 
subconjunto([C|R],[C|R1]):- 
subconjunto(R,R1). 
subconjunto(L,[_|R1]):- 
subconjunto(L,R1). 

/* permutaciones de una lista*/ 
permutaciones([],[]). 
permutaciones([C1|R1],L):- 
eliminar(C1,L,Rest), 
permutaciones(R1,Rest). 

/* laboratorio */ 
vacia([]). 
subcPerm(L,L1):- 
subconjunto(T,L1), 
permutaciones(L,T) 
. 

check([],[],A,A). 
check([C|R],[+C|R1],SumaTemp,Suma):- 
M is SumaTemp + C, 
check(R,R1,M,Suma) 

. 
check([C|R],[-C|R1],SumaTemp,Suma):- 
M is SumaTemp - C, 
check(R,R1,M,Suma) 
. 

lab(ListaLarga,Suma,[C|R1]):- 
subcPerm([C|R],ListaLarga), 
check(R,R1,C,Suma) 
. 

/* invertir una lista*/ 
invertir([],[]). 
invertir([C|R],L):- 
invertir(R,X), 
addend(C,X,L). 

/* mayor de una lista */ 
mayor([C|[]],C):-!. 
mayor([C|R],C1):- 
mayor(R,C2), 
C>C2 , 
C1 is C,!. 
mayor([_|R],C1):- 
mayor(R,C1) 
. 

/* menor de una lista */ 
menor([C|[]],C):-!. 
menor([C|R],C1):- 
menor(R,C2), 
C<C2 , 
C1 is C,!. 
menor([_|R],C1):- 
menor(R,C1) 
. 

/* sublistas de una lista*/ 
prim([],_). 
prim([C|R],[C|R1]):- 
prim(R,R1) 
. 
sublista([],[]). 
sublista([C|R],[C1|R1]):- 
prim([C|R],[C1|R1]); 
sublista([C|R],R1) 
. 

/* verifica si una lista es creciente*/ 
creciente([_|[]]). 
creciente([C|[C1|R1]]):- 
C < C1, 
creciente([C1|R1]) 
. 

/* calcula los intervalos crecientes de una lista */ 
intervalosCrec(Inter,L):- 
sublista(Inter,L), 
creciente(Inter) 
. 

may(Inter,L, Long):- 
( intervalosCrec(Inter,L),lenght(inter,Long) ); 
( Long1 is Long -1,may(Inter,L,Long1) ) 
. 

l(Inter,L):- 
lenght(L,M); 
may(Inter,L,M) 
. 
/* producto de 2 vectores */ 
prodEscalar([],[],0). 
prodEscalar([C|R],[C1|R1],Result):- 
prodEscalar(R,R1,Result1), 
Result is C * C1 + Result1 
. 
/* cantidad columnas de una matriz */ 
cantCol([C|_],CC):- 
lenght(C,CC) 
. 

/* cantidad filas de ina matriz */ 
cantFil(L,CF):- 
lenght(L,CF) 
. 

/* columna Num de una matriz [C|R] */ 
getCol([],_,[]). 
getCol([C|R],Num,[C1|R1]):- 
getElem(Num,C,C1), 
getCol(R,Num,R1) 
. 
/* fila Num de una matriz [C|R] */ 
getFil(L,Num,L1):- 
getElem(Num,L,L1) 
. 

/* multiplicar matrices */ 
crearFila(_,Col,[],_,M2):- 
cantCol(M2,Cant), Cant= Col,! 
. 

crearFila(Fil,Col,[C|R],M1,M2):- 
getFil(M1,Fil,Fila), 
getCol(M2,Col,Columna), 
prodEscalar(Fila,Columna,C), 
ColTemp is Col +1, crearFila(Fil,ColTemp,R,M1,M2) 
. 

mult(Fil,[],M1,_):- 
cantFil(M1,Cant),Cant= Fil, 
!. 
mult(Fil,[C|R],M1,M2):- 
crearFila(Fil,0,C,M1,M2), 
FilTemp is Fil +1, 
mult(FilTemp,R,M1,M2) 
. 

multiplicar(M1,M2,M):- 
mult(0,M,M1,M2) 
. 
/* cantidad que se repite X en una lista*/ 
cantRep(_,[],0). 

cantRep(X,[X|R],Cant):- 
cantRep(X,R,Cant1), 
Cant is Cant1+1,!. 
cantRep(X,[_|R],Cant):- 
cantRep(X,R,Cant) 
. 

/* jkkjhk no pincha */ 

mayr([X],1,X). 

mayr([C|R],Cant,Elem):- 
cantRep(C,[C|R],Cant1), 
mayr(R,Cant2,Elem1), 
(((Cant1>= Cant2) ,(Cant is Cant1,Elem is C)) ;( Cant is Cant2 , Elem is Elem1)) 
. 
/* concatenar dos listas */ 

concat([],L,L). 
concat([C|R],L,[C|R1]):- 
concat(R,L,R1) 
. 

/* obtener todos los elementos atomicos de una lista de listas de listas de... */ 
flatten([],[]):-!. 
flatten([C|R],L):- 
flatten(C,L1), 
flatten(R,L2), 
concat(L1,L2,L),! 
. 
flatten(X,[X]). 

/* suma de matrices */ 
crearfila([],[],[]). 
crearfila([C|R],[C1|R1],[C2|R2]):- 
C2 is C + C1, 
crearfila(R,R1,R2) 
. 
sumaMat([],[],[]). 
sumaMat([C|R],[C1|R1],[C2|R2]):- 
crearfila(C,C1,C2), 
sumaMat(R,R1,R2) 
. 
/* elemento de una matriz */ 
elemMat(Fila,Col,[C|R],X):- 
getElem(Fila,[C|R],L), 
getElem(Col,L,X) 
. 

/* sobreescribir un elemento en una lista */ 
sobreescribirEn(_,_,[],[]). 
sobreescribirEn(Elem,0,[_|R],[Elem|R1]):- 
sobreescribirEn(Elem,-1,R,R1),! 
. 
sobreescribirEn(Elem,Pos,[C|R],[C|R1]):- 
ColTemp is Pos -1, 
sobreescribirEn(Elem,ColTemp,R,R1) 
. 

/* sobreescribir un elemnto en una matriz */ 

sobreescribirMat(_,_,_,[],[]):-!. 
sobreescribirMat(0,Col,Elem,[C|R],[C1|R1]):- 
sobreescribirEn(Elem,Col,C,C1), FilTemp is -1, 
sobreescribirMat(FilTemp,Col,Elem,R,R1),! 
. 
sobreescribirMat(Fil,Col,Elem,[C|R],[C|R1]):- 
FilTemp is Fil - 1, 
sobreescribirMat(FilTemp,Col,Elem,R,R1) 
. 

/* intercambiar elemntos de una matriz */ 
exchange(pto(Fila1,Col1),pto(Fila2,Col2),[C|R],[C1|R1]):- 
elemMat(Fila1,Col1,[C|R],Pos1), 
elemMat(Fila2,Col2,[C|R],Pos2), 
sobreescribirMat(Fila1,Col1,Pos2,[C|R],M), 
sobreescribirMat(Fila2,Col2,Pos1,M,[C1|R1]) 
. 



/* sublistas de una lista*/ 
prim3([C,C1,C2|[]],[C,C1,C2|_]). 

sublista3([],[]). 
sublista3([C|R],[C1|R1]):- 
prim3([C|R],[C1|R1]); 
sublista3([C|R],R1) 
.