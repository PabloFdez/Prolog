alumno_prode('Fernandez','Diaz','Pablo','V130148').

%Pablo Fernández Díaz v130148

%%%%%%%%%%%%%%%%%%  GENERADOR_DE_CODIGO/3  %%%%%%%%%%%%%%%%%%
%generador_de_codigo será true si dado un EI y un EF devuelve una 
%lista de instrucciones Lis que ejecutadas sobre el EI produce el EF si:
%	- EI y EF posee el mismo nombre y aridad
%	- comprobar_elementos de EI y EF es true
%	- asterisco_EI_EF de EI y EF es true 
%	- La lista de instrucciones Lis tiene una longitud creciente de 0 a N
%		(Siendo N el número de instrucciones mínino que resuelve el problema)
%	y
%	- comprobar_instrucciones de la lista Lis sobre los estados EI y EF, con 
%	  numero de registros NRegs, es true


generador_de_codigo(EI,EF,Lis):-
	functor(EI,regs,NRegs),
	functor(EF,regs,NRegs),
	comprobar_elementos(EI,EF),
	asterisco_EI_EF(EI,EF),
	length(Lis,_),
	comprobar_instrucciones(Lis,NRegs,EI,EF),!. %SOLO SACA UNA INST


%%%%%%%%%%%%%%%%%%  GENERADOR_DE_CODIGO/3  %%%%%%%%%%%%%%%%%%
%generador_de_codigo será true si dado un EI y un EF devuelve una 
%lista de instrucciones Lis que ejecutadas sobre el EI produce el EF si:
%	- EI y EF posee el mismo nombre y aridad
%	- comprobar_elementos de EI y EF es true
%	- asterisco_EF de EI y EF es true 
%	- La lista de instrucciones Lis tiene una longitud creciente de 0 a N
%		(Siendo N el número de instrucciones mínino que resuelve el problema)
%	y
%	- comprobar_instrucciones2 de la lista Lis sobre los estados EI y EF, con 
%	  numero de registros NRegs, es true	
	
generador_de_codigo(EI,EF,Lis):-
	functor(EI,regs,NRegs),
	functor(EF,regs,NRegs),
	comprobar_elementos(EI,EF),
	asterisco_EF(EI,EF),
	length(Lis,_),
	comprobar_instrucciones2(Lis,NRegs,EI,EF),!.

%%%%%%%%%%%%%%%%%%  GENERADOR_DE_CODIGO/3  %%%%%%%%%%%%%%%%%%
%generador_de_codigo será true si dado un EI y un EF devuelve una 
%lista de instrucciones Lis que ejecutadas sobre el EI produce el EF si:
%	- EI y EF posee el mismo nombre y aridad
%	- comprobar_elementos de EI y EF es true
%	- asterisco_EI de EI y EF es true 
%	- La lista de instrucciones Lis tiene una longitud creciente de 0 a N
%		(Siendo N el número de instrucciones mínino que resuelve el problema)
%	y
%	- comprobar_instrucciones2 de la lista Lis sobre los estados EI y EF, con 
%	  numero de registros NRegs, es true	

generador_de_codigo(EI,EF,Lis):-
	functor(EI,regs,NRegs),
	functor(EF,regs,NRegs),
	comprobar_elementos(EI,EF),
	asterisco_EI(EI,EF),
	length(Lis,_),
	comprobar_instrucciones2(Lis,NRegs,EI,EF),!.	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  Auxiliares  %%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%  COMPROBAR_INSTRUCCIONES/4  %%%%%%%%%%%%%%%%
%comprobar_instrucciones de una lista vacía de instrucciones será true si
%los dos estados son iguales

comprobar_instrucciones([],_,EF,EF).

%%%%%%%%%%%%%%%%  COMPROBAR_INSTRUCCIONES/4  %%%%%%%%%%%%%%%%
%comprobar_instrucciones de una de instrucciones lista con 
%cabeza Inst y cola Insts será true si:	
%	- seleccionar_instruccion de NRegs devuelve una posible instruccion
%	- ejecutar_instruccion ejecuta sobre el EI la instruccion Inst y devuelve
%	  un nuevo estado de los registros
%	y
%	- comprobar_instrucciones de la lista Insts,numero de registros NRegs y el estado actual
%	  actualizado es true

comprobar_instrucciones([Inst|Insts],NRegs,EI,EF):-
	seleccionar_instruccion(NRegs,Inst),
	ejecutar_instruccion(EI,Inst,ESiguiente),
	comprobar_instrucciones(Insts,NRegs,ESiguiente,EF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%  COMPROBAR_INSTRUCCIONES2/4  %%%%%%%%%%%%%%%%
%comprobar_instrucciones de una lista vacía de instrucciones será true si:
%	- comprobar_iguales de los dos estados es true

comprobar_instrucciones2([],_,EAct,EF):-
	comprobar_iguales(EAct,EF).

%%%%%%%%%%%%%%%%  COMPROBAR_INSTRUCCIONES2/4  %%%%%%%%%%%%%%%%
%comprobar_instrucciones de una de instrucciones lista con 
%cabeza Inst y cola Insts será true si:	
%	- seleccionar_instruccion de NRegs devuelve una posible instruccion
%	- ejecutar_instruccion ejecuta sobre el EI la instruccion Inst y devuelve
%	  un nuevo estado de los registros
%	y
%	- comprobar_instrucciones2 de la lista Insts,numero de registros NRegs y el estado actual
%	  actualizado es true

comprobar_instrucciones2([Inst|Insts],NRegs,EI,EF):-
	seleccionar_instruccion(NRegs,Inst),
	ejecutar_instruccion(EI,Inst,ESiguiente),
	comprobar_instrucciones2(Insts,NRegs,ESiguiente,EF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%  COMPROBAR_ELEMENTOS/2  %%%%%%%%%%%%%%%%%%%
%comprobar_elementos de EI y EF será true si:
%	- EI y EF tienen el mismo nombre y aridad
%	- comprobar_elementos_b devuelve N elementos de EI en Lis
%	y
%	- comprobar_member_b devuelve true si los N elementos de EF pertenecen a Lis

comprobar_elementos(EI,EF):-
	functor(EI,regs,N),
	functor(EF,regs,N),
	comprobar_elementos_b(N,EI,Lis),
	comprobar_member_b(N,EF,Lis),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%  COMPROBAR_ELEMENTOS_B/3  %%%%%%%%%%%%%%%%%
%comprobar_elementos_b de 0 devuelve una lista vacía
	
comprobar_elementos_b(0,_,[]).

%comprobar_elementos_b para N y EI devuelve una lista con 
%cabeza Arg y cola Col si:
%	- N es mayor que 0
%	- en N1 se guarda la resta de N-1
%	- el argumento N de EI es Arg
%	y
%	- comprobar_elementos_b de N1 con EI devuelve Col

comprobar_elementos_b(N,EI,[Arg|Col]):-
	N>0,
	N1 is N-1,
	arg(N,EI,Arg),
	comprobar_elementos_b(N1,EI,Col).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  COMPROBAR_MEMBER_B/3  %%%%%%%%%%%%%%%%%%%	
%comprobar_member_b de 0 siempre será true.
	
comprobar_member_b(0,_,_).

%%%%%%%%%%%%%%%%%%  COMPROBAR_MEMBER_B/3  %%%%%%%%%%%%%%%%%%%	
%comprobar_member_b de N, EF y Lis será true si:
%	- N es mayor que 0
%	- el argumento N de EF es Arg
%	- Arg es miembro de la lista Lis
%	- en N1 se guarda la resta N-1
%	y
%	- comprobar_member_b de N1 y EF devuelve Lis
	
comprobar_member_b(N,EF,Lis):-
	N>0,
	arg(N,EF,Arg),
	member(Arg,Lis),
	N1 is N-1,
	comprobar_member_b(N1,EF,Lis).

%%%%%%%%%%%%%%%%%%  COMPROBAR_MEMBER_B/3  %%%%%%%%%%%%%%%%%%%	
%comprobar_member_b de N, EF y Lis será true si:
%	- N es mayor que 0
%	- el argumento N de EF es Arg
%	- Arg es igual a *
%	- en N1 se guarda la resta N-1
%	y
%	- comprobar_member_b de N1 y EF devuelve Lis
	
comprobar_member_b(N,EF,Lis):-
	N>0,
	arg(N,EF,Arg),
	Arg=='*',
	N1 is N-1,
	comprobar_member_b(N1,EF,Lis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  ASTERISCO_EI_EF/2  %%%%%%%%%%%%%%%%%%%%%%
%asterisco_EI_EF de EI y EF será true si:
%	- EF tiene nombre regs y aridad N
%	- hay_asterisco devuelve en L1 los registros de EF donde hay asterisco
%	- hay_asterisco devuelve en L2 los registros de EI donde hay asterisco
%	- las longitudes de L1 y L2 se guardan en N1 y N2 respectivamente
%	y
%	- tanto N1 como N2 son mayores que 0
	
asterisco_EI_EF(EI,EF):-
	functor(EF,regs,N),
	hay_asterisco(N,EF,L1),
	hay_asterisco(N,EI,L2),
	length(L1,N1),
	length(L2,N2),
	N1>0,N2>0.	
	
%%%%%%%%%%%%%%%%%%  ASTERISCO_EI_EF/2  %%%%%%%%%%%%%%%%%%%%%%
%asterisco_EI_EF de EI y EF será true si:
%	- EF tiene nombre regs y aridad N
%	y
%	- tanto EF como EI no tienen asteriscos
	
asterisco_EI_EF(EI,EF):-
	functor(EF,regs,N),
	hay_asterisco(N,EF,[]),
	hay_asterisco(N,EI,[]).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%  ASTERISCO_EF/2  %%%%%%%%%%%%%%%%%%%%%%%%
%asterisco_EF de EI y EF será true si:
%	- EF tiene nombre regs y aridad N
%	- hay_asterisco devuelve en L1 los registros de EF donde hay asterisco
%	- EI no contiene asteriscos
%	- la longitud de L1 se guarda en N1
%	y
%	- N1 es mayor que 0	
	
asterisco_EF(EI,EF):-
	functor(EF,regs,N),
	hay_asterisco(N,EF,L1),
	hay_asterisco(N,EI,[]),
	length(L1,N1),
	N1>0.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%  ASTERISCO_EI/2  %%%%%%%%%%%%%%%%%%%%%%%%
%asterisco_EI de EI y EF será true si:
%	- EF tiene nombre regs y aridad N
%	- hay_asterisco devuelve en L1 los registros de EI donde hay asterisco
%	- EF no contiene asteriscos
%	- la longitud de L1 se guarda en N1
%	y
%	- N1 es mayor que 0		
	
asterisco_EI(EI,EF):-
	functor(EF,regs,N),
	hay_asterisco(N,EF,[]),
	hay_asterisco(N,EI,L1),
	length(L1,N1),
	N1>0.
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%%%%%%%%%%%%%%%%%%%  COMPROBAR_IGUALES/2  %%%%%%%%%%%%%%%%%%%	
%comprobar_iguales de EAct y EF será true si:
%	- EF tiene nombre regs y aridad N
%	- hay_asterisco de EF guarda los asteriscos de EF en ListaAst
%	- la lista ListaAst tiene longitud Lon
%	- Lon es mayor que 0
%	y
%	- comprobar_iguales_aste_EF de EAct, ListaAst y EF es true

comprobar_iguales(EAct,EF):-
	functor(EF,regs,N),
	hay_asterisco(N,EF,ListaAst),
	length(ListaAst,Lon),
	Lon>0,
	comprobar_iguales_aste_EF(EAct,ListaAst,EF),!.
	
comprobar_iguales(EAct,EF):-
	functor(EF,regs,N),
	hay_asterisco(N,EAct,ListaAst),
	length(ListaAst,Lon),
	Lon>0,
	comprobar_iguales_aste_EAct(EAct,ListaAst,EF),!.	

	comprobar_iguales_aste_EAct(EF,[],EF).
	
	comprobar_iguales_aste_EAct(EAct,[Cab|ListaAst],EF):-
	functor(EAct,regs,N),
	functor(ENuevo,regs,N),
	arg(Cab,ENuevo,'*'),
	rellenar(EF,ENuevo),
	comprobar_iguales_aste_EAct(EAct,ListaAst,ENuevo).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%%%%%%%%%%%%%%  COMPROBAR_IGUALES_ASTE_EF/3  %%%%%%%%%%%%%%%%
%comprobar_iguales_aste_EF de una lista vacía es true si EAct es igual a EF

comprobar_iguales_aste_EF(EF,[],EF).

%%%%%%%%%%%%%%  COMPROBAR_IGUALES_ASTE_EF/3  %%%%%%%%%%%%%%%%
%comprobar_iguales_aste_EF de EAct, una lista de asteriscos y EF será true si:
%	- EF tiene nombre regs y aridad N
%	- ENuevo tiene nombre regs y aridad N
%	- el argumento numero Cab de ENuevo es *
%	- rellenar de EAct y ENuevo es true
%	y
%	- comprobar_iguales_aste_EF de ENuevo, ListaAst y EF es true

comprobar_iguales_aste_EF(EAct,[Cab|ListaAst],EF):-
	functor(EF,regs,N),
	functor(ENuevo,regs,N),
	arg(Cab,ENuevo,'*'),
	rellenar(EAct,ENuevo),
	comprobar_iguales_aste_EF(ENuevo,ListaAst,EF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%%%%%%%%%%%%%%%%%%  HAY_ASTERISCO/3  %%%%%%%%%%%%%%%%%%%%%%%%
%hay_asterisco de N devuelve una lista vacia si:
%	- N es igual a 0
		
hay_asterisco(N,_,[]):-
	N=0.
	
%%%%%%%%%%%%%%%%%%  HAY_ASTERISCO/3  %%%%%%%%%%%%%%%%%%%%%%%%
%hay_asterisco de N, EF y ListaAst será true si:
%	- N es mayor a 0
%	- el argumento N de EF es Arg
%	- Arg no es igual a *	
%	- en N1 se guarda la resta de N-1
%	y
%	- hay_asterisco de N1, EF y ListaAst es true
	
hay_asterisco(N,EF,ListaAst):-
	N>0,
	arg(N,EF,Arg),
	Arg\=='*',
	N1 is N-1,
	hay_asterisco(N1,EF,ListaAst).

%%%%%%%%%%%%%%%%%%  HAY_ASTERISCO/3  %%%%%%%%%%%%%%%%%%%%%%%%
%hay_asterisco de N, EF y una lista con cabeza L y cola ListaAst será true si:
%	- N es mayor a 0
%	- el argumento N de EF es Arg
%	- Arg es igual a *	
%	- en N1 se guarda la resta de N-1
%	y
%	- hay_asterisco de N1, EF y ListaAst es true
	
hay_asterisco(N,EF,[N|ListaAst]):-
	N>0,
	arg(N,EF,Arg),
	Arg=='*',
	N1 is N-1,
	hay_asterisco(N1,EF,ListaAst).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%  SELECCIONAR_INSTRUCCION/2  %%%%%%%%%%%%%%%%
%seleccionar_instruccion de numero de registros NRegs e instruccion Inst
%será true si:
%	- obtener_numerosMove para NRegs devuelve N
%	y
%	- imprime_move de dicho numero N devuelve una instruccion Inst	

seleccionar_instruccion(NRegs,Inst):-
	obtener_numerosMove(NRegs,N),
	imprime_move(N,Inst).

%%%%%%%%%%%%%%%%  SELECCIONAR_INSTRUCCION/2  %%%%%%%%%%%%%%%%
%seleccionar_instruccion de numero de registros NRegs e instruccion Inst
%será true si:
%	- obtener_numerosSwap para NRegs devuelve N1 y N2
%	y
%	- imprime_swap de dichos numeros N1 y N2 devuelve una instruccion Inst
	
seleccionar_instruccion(NRegs,Inst):-
	obtener_numerosSwap(NRegs,N1,N2),
	imprime_swap(N1,N2,Inst).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%  OBTENER_NUMEROSMOVE/2  %%%%%%%%%%%%%%%%%%
%obtener_numerosMove de NRegs devuelve NRegs será siempre true
	
obtener_numerosMove(NRegs,NRegs).

%%%%%%%%%%%%%%%%%%  OBTENER_NUMEROSMOVE/2  %%%%%%%%%%%%%%%%%%
%obtener_numerosMove de NRegs devuelve ID_Registro si:
%	- NRegs es mayor que 1
%	- N1 es la resta de N-1
%	y
%	- obtener_numerosMove de N1 es ID_Registro

obtener_numerosMove(NRegs,ID_Registro) :-
	NRegs > 1,
	N1 is NRegs-1,
	obtener_numerosMove(N1,ID_Registro).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%%%%%%%%%%%%%%%%%%  OBTENER_NUMEROSSWAP/3  %%%%%%%%%%%%%%%%%%
%obtener_numerosSwap para un numero de registros X será Y y Z si:
%	- X2 es la suma de X+1
%	- ran de 1, X2 es Z
%	y
%	- ran de 1, Z es y
	
obtener_numerosSwap(X,Y,Z) :- 
	X2 is X+1, 
	ran(1,X2,Z), 
	ran(1,Z,Y).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%%%%%%%%%%%%%%%%%%%%% RAN/3  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%ran de X será Y y X si:
%	- X es menor que Y	

ran(X,Y,X) :- 
	X < Y.

%%%%%%%%%%%%%%%%%%%%% RAN/3  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%ran de X será Y y Z si:
%	- X es menor que Y	
%	- X2 es la suma X+1
%	y
%	- ran de X2 es Y y Z

ran(X,Y,Z) :- 
	X < Y, 
	X2 is X+1, 
	ran(X2,Y,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
%%%%%%%%%%%%%%%%%%%%%  IMPRIME_MOVE/2  %%%%%%%%%%%%%%%%%%%%%%	
%imprime_move de NRegs será Inst si:
%	- Inst tiene nombre move y 1 argumento
%	y
%	- el primer argumento de Inst es NRegs

imprime_move(NRegs,Inst):-
	functor(Inst,move,1),
	arg(1,Inst,NRegs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	
%%%%%%%%%%%%%%%%%%%%%  IMPRIME_SWAP/3  %%%%%%%%%%%%%%%%%%%%%%	
%imprime_move de NRegs será Inst si:
%	- Inst tiene nombre swap y 2 argumentos
%	- el primer argumento de Inst es N1
%	y
%	- el segundo argumento de Inst es N2
	
imprime_swap(N1,N2,Inst):-
	functor(Inst,swap,2),
	arg(1,Inst,N1),
	arg(2,Inst,N2).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	
%%%%%%%%%%%%%%%%%  EJECUTAR_INSTRUCCION/3  %%%%%%%%%%%%%%%%%%
%ejecutar_instruccion Inst sobre el EActual será ESig si:
%	- Inst tiene nombre move y 1 argumento
%	- el primer argumento de Inst se guarda en Reg
%	- move de Reg sobre EActual devuelve ESig
%	y
%	- rellenar de EActual devuelve ESig relleno

ejecutar_instruccion(EActual,Inst,ESig):-
	functor(Inst,move,1),
	arg(1,Inst,Reg),
	move(Reg,EActual,ESig),
	rellenar(EActual,ESig).

%%%%%%%%%%%%%%%%%  EJECUTAR_INSTRUCCION/3  %%%%%%%%%%%%%%%%%%
%ejecutar_instruccion Inst sobre el EActual será ESig si:
%	- Inst tiene nombre swap y 2 argumentos
%	- el primer argumento de Inst se guarda en Reg1
%	- el segundo argumento de Inst se guarda en Reg2
%	- swap de Reg1 y Reg2 sobre EActual devuelve ESig
%	y
%	- rellenar de EActual devuelve ESig relleno
	
ejecutar_instruccion(EActual,Inst,ESig):-
	functor(Inst,swap,2),
	arg(1,Inst,Reg1),
	arg(2,Inst,Reg2),
	swap(Reg1,Reg2,EActual,ESig),
	rellenar(EActual,ESig).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	
%%%%%%%%%%%%%%%%%%%%%  MOVE/3  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%move de A sobre el EI devuelve EF si:
%	- A es mayor o igual a 1
%	- EI tiene nombre regs y NREGISTROS argumentos
%	- A es menor que NREGISTROS
%	- EF tiene nombre regs y NREGISTROS argumentos
%	- en A1 se guarda la suma de A+1
%	- el argumento A del EI es Arg
%	y
%	- el argumento A1 del EF es Arg

move(A,EI,EF):-
	1 =< A,
	functor(EI,regs,NREGISTROS),
	A<NREGISTROS,
	functor(EF,regs,NREGISTROS),
	A1 is A+1,
	arg(A,EI,Arg),
	arg(A1,EF,Arg).

%%%%%%%%%%%%%%%%%%%%%  MOVE/3  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%move de A sobre el EI devuelve EF si:
%	- EI tiene nombre regs y NREGISTROS argumentos
%	- A es igual que NREGISTROS
%	- EF tiene nombre regs y NREGISTROS argumentos
%	- el argumento A del EI es Arg
%	y
%	- el argumento 1 del EF es Arg
	
move(A,EI,EF):-
	functor(EI,regs,NREGISTROS),
	A == NREGISTROS,
	functor(EF,regs,NREGISTROS), 
	arg(A,EI,Arg),
	arg(1,EF,Arg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
		
%%%%%%%%%%%%%%%%%%%%%  SWAP/4  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%swap de A y B es true si:
%	- A y B son numeros
%	y
%	- A es igual que B
	
swap(A,B,_,_):-
	number(A),
	number(B),
	A==B,!,fail.

%%%%%%%%%%%%%%%%%%%%%  SWAP/4  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%swap de A y B sobre la Entrada es la Salida si:
%	- A y B son numeros
%	- A es menor que B	
%	- Entrada tiene nombre regs y N argumentos
%	- Salida tiene nombre regs y N argumentos
%	- el argumento A de la Entrada es Primero	
%	- el argumento B de la Entrada es Segundo
%	- el argumento B de la Salida es Primero
%	y
%	- el argumento A de la Salida es Segundo

swap(A,B,Entrada,Salida):-
	number(A),
	number(B),
	A<B,
	functor(Entrada,regs,N),
	functor(Salida,regs,N),
	arg(A,Entrada,Primero), 
	arg(B,Entrada,Segundo),
	arg(B,Salida,Primero), 
	arg(A,Salida,Segundo).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
		
%%%%%%%%%%%%%%%%%%%  RELLENAR/2  %%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%rellenar de Entrada es Salida si:
%	- Entrada y Salida son ground
	
rellenar(Entrada,Salida):-
		ground(Entrada),
		ground(Salida),!.

%%%%%%%%%%%%%%%%%%%  RELLENAR/2  %%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%rellenar de Entrada es Salida si:
%	- Entrada no es nonvar
%	- Entrada tiene nombre regs y registros A
%	y
%	-rellenar de A y Entrada es Salida
		
rellenar(Entrada,Salida):-
	nonvar(Entrada),
	functor(Entrada,regs,A),
	rellenar(A,Entrada,Salida).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
		
%%%%%%%%%%%%%%%%%%%  RELLENAR/3  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%rellenar de A y Entrada es Salida si:
%	- A es igual a 1
%	- el argumento A de Salida es Arg
%	- Arg es ground
%	y
%	- rellenar de Entrada es Salida
	
rellenar(A,Entrada,Salida):-
	A==1,
	arg(A,Salida,Arg),
	ground(Arg),
	rellenar(Entrada,Salida).
	
%%%%%%%%%%%%%%%%%%%  RELLENAR/3  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%rellenar de A y Entrada es Salida si:
%	- A es igual a 1
%	- el argumento A de Salida es Arg
%	- el argumento A de Entrada es Arg2
%	- Arg es igual a Arg2
%	y
%	- rellenar de Entrada es Salida
	
rellenar(A,Entrada,Salida):-
	A==1,
	arg(A,Salida,Arg),	%Cojo el argumento A de la Salida
	arg(A,Entrada,Arg2),
	Arg=Arg2,
	rellenar(Entrada,Salida).

%%%%%%%%%%%%%%%%%%%  RELLENAR/3  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%rellenar de A y Entrada es Salida si:
%	- A es menor que 1
%	- el argumento A de Salida es Arg
%	- Arg es ground
%	- en A1 se guarda la resta de A-1
%	y
%	- rellenar de A1 y Entrada es Salida	
	
rellenar(A,Entrada,Salida):-
	A>1,
	arg(A,Salida,Arg),
	ground(Arg),
	A1 is A-1,
	rellenar(A1,Entrada,Salida).

%%%%%%%%%%%%%%%%%%%  RELLENAR/3  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%rellenar de A y Entrada es Salida si:
%	- A es mayor que 1
%	- el argumento A de Salida es Arg
%	- el argumento A de Entrada es Arg2
%	- Arg es igual a Arg2
%	- en A1 se guarda la resta de A-1
%	y
%	- rellenar de A1 y Entrada es Salida	
	
rellenar(A,Entrada,Salida):-
	A>1,
	arg(A,Salida,Arg),
	arg(A,Entrada,Arg2),
	Arg=Arg2,
	A1 is A-1,
	rellenar(A1,Entrada,Salida).
