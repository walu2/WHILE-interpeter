:- module(interpreter_imp, [run/2]).

% Interpreter jezyka Imp.

% Przyklad uzycia:
%    ?- run("/*Przykladowy program*/{ var N := 5;  while( N > 1) { write 2*N - 4; N := N - 1;}", true).
% Gdy drugi parametr jest  rowny true, jestesmy w trybie debugowania, w przeciwnym razie  
% uruchamiamy program w zwyklym trybie. 
% Kolejne linie w trybie debugowania tak jak maszynie abstrakcyjnej jezyka while 
% zawieraja kolejne konfiguracje. 

/*
						  LEKSER 
   Lekser jest rozszerzeniem tego dla jezyka while napisanego przez TWI
*/

lexer(Tokens) -->
   white_space,
   ( (  "/*",        comment, !,lexer(Tokens)
	;
	 (  ";",       !, { Token = tokSColon }
      ;  "(",       !, { Token = tokLParen }
	  ;  ")",       !, { Token = tokRParen }
	  ;  "{",       !, { Token = tokLParenCurly }
	  ;  "}",       !, { Token = tokRParenCurly }
	  ;  "[",       !, { Token = tokLParenArr }
	  ;  "]",       !, { Token = tokRParenArr }
	  ;  ",",       !, { Token = tokComma }
	  ;  "||",       !, { Token = tokOr }
	  ;  "&&",       !, { Token = tokAnd }
	  ;  "!=",      !, { Token = tokNeq }
	  ;  "!",       !, { Token = tokFactor }
      ;  "+",       !, { Token = tokPlus }
      ;  "-",       !, { Token = tokMinus }
      ;  "*",       !, { Token = tokTimes }
      ;  "=",       !, { Token = tokEq }
	  ;  "/",       !, { Token = tokDiv }
	  ;  "%",       !, { Token = tokMod }
      ;  "<=",      !, { Token = tokLeq }
      ;  "<",       !, { Token = tokLt }
      ;  ">=",      !, { Token = tokGeq }
      ;  ">",       !, { Token = tokGt }
      ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      ;  letter(L), !, identifier(L, Id),
            {  member((Id, Token), [ (else, tokElse),
									 (if, tokIf),
									 (write, tokWrite),									
									 (read, tokRead),
									 (abort, tokAbort),
									 (var, tokVarI),
                                     (while, tokWhile)]),
               !
            ;  Token = tokVar(Id)
            }
      ;  [X],
            { Token = tokUnknown(X) }
      ),
      !,
         { Tokens = [Token | TokList] },
      lexer(TokList)
   ;  [],
         { Tokens = [] }
   )
   ).
   
/* Predykat decode/2 jest przydatna przy informacji o bledach i rozkodowuje tokeny*/
decode(X,Y):- member((Y,X), [(else, tokElse),
							(if, tokIf),
							(write, tokWrite),									
							(read, tokRead),
							(abort, tokAbort),
							(var, tokVarI),
                            (while, tokWhile),
							('";"',tokSColon),
							('"("', tokLParen),
							('")"', tokRParen),
							(  '"{"',        tokLParenCurly),
							(  '"}"',        tokRParenCurly ), 
							(  '"["',        tokLParenArr ), 
							(  '"]"',        tokRParenArr ), 
							(  '","',        tokComma ), 
							(  '"||"',        tokOr ), 
							(  '"&&"',        tokAnd ), 
							(  '"!="',       tokNeq ), 
							(  '"!"',        tokFactor ), 
							(  '"+"',        tokPlus ), 
							(  '"-"',        tokMinus ), 
							(  '"*"',        tokTimes ), 
							(  '"="',        tokEq ), 
							(  '"/"',        tokDiv ), 
							(  '"%"',        tokMod ), 
							(  '"<="',       tokLeq ), 
							(  '"<"',        tokLt ), 
							(  '">="',       tokGeq ),
							(  '">"',        tokGt)]),!.
decode(tokUnknown(X),Y):-char_code(Y,X).
decode(tokVar(X),X).				
decode(tokNumber(X),X).
	  
/*Dodajemy obsluge komentarzy*/	  
comment --> "*/",!;
			[_],!, comment.
 
white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
white_space -->
   [].
   
digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha) }.

alphanum([A|T]) -->
   [A], { code_type(A, alnum) }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
      { atom_codes(Id, [L|As]) }.

	  
	  

/*
							PARSER dla jezyka IMP z obsluga bledow
przyjmujemy, ze w jezyku doposzczalna jest deklaracja 
	var a[5], a;
tj. jednoczesnie moze funkcjonowac zmienna a i tablica a
*/
/*
	operatory ulatwiajace konstrukcje i pozniejsza analize drzewa rozbioru syntaktycznego
*/
:- op(990, xfy, ';;').
:- op(820, xfy, and).
:- op(840, xfy, or).
:- op(700, xfy, <=).
:- op(400, fy, '--').

/* 
w kazdym predykacie wyrzycamy jakis wyjatek, by zakonczyc dzialanie pasera lub interpretera

 se/1 Wyswietla komunikat o bledzie syntaktycznym,  X - opis bledu
*/

se(X):- write('Syntax error: '), write(X), throw(ble).

/*
Wyswietla komunikat o bledzie niezadeklarowanej zmiennej lub tablicy
 Z -  opis zmienna czy tablica
 X  - nazwa zmiennej
*/
uv(Z, X):- write('Undeclared '), write(Z), write(X), throw(ble).

/* errorMy/4 sprawdza czy lista tokenow X zaczyna sie od tokenu Z, jesli tak zwraca 
ogon X w zmiennej T w przeciwnym razie wyswietla komunikat o bledzie ze szczegolami Y
*/ 
errorMy(Z,Y,X,T):- X = [Z|T],!;
				 write('Syntax error: '), write(Y),  write(' expected'), 
				 (X = [Z1|_]-> write(', but there is '), decode(Z1,Z2), write(Z2);  
							   write('')
				 ), 
				 throw(ble).

				 
test(Z,X,X):- X = [Z|_].					





program(Ast) --> blockMy([], Block), !, ([_] -> {se('unexpected sequence after "}"')}
											 ; {Ast = Block} 
).

/*Kolejne produkcje gramatyki przy mechanizmu DCG,
  w porownaniu do parsera jezyka while, przykazujemy rowniez tablice zadeklarowanych zmiennych,
  a czasmi rowniez zwracamy taka zmodyfikowana tablice
*/  
blockMy(Var, Block) --> ([tokLParenCurly], !, instructions(['$'|Var],_,Instr), 
															( [tokRParenCurly],!,   {Block = {Instr}}
															; {se('"}" expected')}
															)
					)

					; {se('"{" expected')}.


/*
 Predykat insructions definujemy recznie, aby uniknac nawrotow 
 (uzywamy mozliwosci parsera przewidujacego). 
*/
instructions(Var, VarO, Instrs, X, Y):- (X = [Z|_], 
										member(Z, [tokAbort, tokVarI, tokVar(_), tokRead, tokWrite, 
												   tokIf, tokWhile, tokLParenCurly]), /*insrukcja musi zaczyna sie
												   od jednego z tych tokenow*/
										instruction(Var, VarO1, Instr, X, X1), 
										instructions(VarO1, VarO, Rest, X1, X2), !, Instrs= (Instr';;'Rest), Y=X2
										)
										; !, VarO = Var, Instrs=empty, Y=X.

instruction(Var, VarO, Instr)-->(  
					    [tokAbort], !, 	errorMy(tokSColon, '";"'), {VarO = Var}, {Instr = abort}
						
					 ;  [tokVarI],  !, declarators(Var, VarO, Decls), errorMy(tokSColon, '";" or maybe "["'),  
								    {Instr = var(Decls)}  /*co dalej*/
									
					 ;  [tokRead],  !, cell(Var, Cell), errorMy(tokSColon, '";"'), {VarO = Var},{Instr = read(Cell)}
					 
					 ;  [tokWrite], !, expr(Var, Expr), errorMy(tokSColon, '";"'), {VarO = Var},
								    {Instr = write(Expr)}
									
					 ;  [tokIf], errorMy(tokLParen, '"("'),  
								 expr(Var, Expr),errorMy(tokRParen, '")"'),  instruction(Var, VarO1,InstrIf),
								 ( [tokElse], !,instruction(VarO1, VarO, InstrElse), 
									{Instr = if(Expr, InstrIf, InstrElse)}
								 ;  {VarO= VarO1}, {Instr = if(Expr, InstrIf, empty)}		
								 )
								 
					 ;  [tokWhile], !, errorMy(tokLParen, '"("'), 
									  expr(Var, Expr), errorMy(tokRParen, '")"') ,
									  instruction(Var, VarO, InstrWhile), {Instr = while(Expr, InstrWhile)} 
					 
					 ;  test(tokLParenCurly), !, blockMy(Var, Block), {VarO = Var}, {Instr = Block}
					 
					 ; cell(Var, Cell), errorMy(tokEq, '"="'), expr(Var,Expr),  errorMy(tokSColon, '";"'), 
					   {VarO = Var},{Instr = subs(Cell, Expr)}
					 
					 ;  {se('unknow/unexpected instruction or expected instruction. 
						 Check, whether "{" and "}" are necessary.')}			
					 ). 

					 

declarators(Var, VarO, Decls)--> declarator(Var, VarO1, Decl), 
					  (	
					    [tokComma],!, declarators(VarO1, VarO,Rest),   {Decls = [Decl|Rest]}
						; {VarO = VarO1}, {Decls = [Decl]}
					  ).
					  
					  
declarator(VarI, VarO, Decl)--> 
					  errorMy(tokVar(Var),'identifier'), 
								{
								member((Var,_), VarI) -> write('Variable/ Array '), write(Var), 
								write(' might already been defined in the scope'), nl; true
								}, 
					 (   [tokEq],!, expr(VarI,Expr), {VarO = [(Var,v)|VarI]}, 
										{Decl = variable(Var,Expr)}
					 
					  ; [tokLParenArr], !, expr(VarI, Expr), errorMy(tokRParenArr, '"]"'), 
									    {VarO = [(Var,a)|VarI]}, {Decl = array(Var, Expr)}
										
					  ;  {VarO = [(Var,v)|VarI]},
										{Decl = variable(Var)}
					 ).
					
cell(VarI, Cell) --> 
					errorMy(tokVar(Var),'identifier'), 
					(  [tokLParenArr], 
									{(\+ member((Var,a),VarI)) -> uv('array ', Var); true}, 
									!, expr(VarI,Expr), errorMy(tokRParenArr, '"]"'), 
									    {Cell = array(Var, Expr)}
					  ; {(\+ member((Var,v),VarI)) -> uv('variable ', Var); true}, 
									    {Cell = variable(Var)}
					).

					
expr(Var, Expr) -->      disjunct(Var, Disjunct), exprA(Var,Disjunct, Expr).


exprA(Var,Acc, Expr) -->  [tokOr], !, disjunct(Var, Disjunct),
						  { Acc1 =.. [or, Acc, Disjunct] }, exprA(Var, Acc1, Expr).
exprA(_,Acc, Acc) -->  [].		


disjunct(Var, Disjunct) -->   conjunct(Var, Conjunct), disjunctA(Var, Conjunct, Disjunct).


disjunctA(Var, Acc, Disjunct) -->   [tokAnd], !, conjunct(Var, Conjunct),
							  { Acc1 =.. [and, Acc, Conjunct] }, disjunctA(Var, Acc1, Disjunct).
disjunctA(_,Acc, Acc) -->   [].



conjunct(Var, Conjunct) --> arith_expr(Var, LExpr), 
					  (  rel_op(Op), arith_expr(Var, RExpr), { Conjunct =.. [Op, LExpr, RExpr] }
					  ;  [], {Conjunct = LExpr}
					  ).					

					  
arith_expr(Var, Expr) -->  summand(Var,Summand), arith_exprA(Var,Summand, Expr).



arith_exprA(Var, Acc, Expr) -->    additive_op(Op), !, summand(Var, Summand),
							{ Acc1 =.. [Op, Acc, Summand] }, arith_exprA(Var, Acc1, Expr).
arith_exprA(_,Acc, Acc) -->    [].



summand(Var, Expr) --> 	 factor(Var, Factor), summandA(Var, Factor, Expr).



summandA(Var, Acc, Expr) -->	 multiplicative_op(Op), !, factor(Var, Factor),
						{ Acc1 =.. [Op, Acc, Factor] }, summandA(Var, Acc1, Expr).
summandA(_,Acc, Acc) -->   [].
   

   
factor(Var, Expr) -->   (  [tokLParen], !, expr(Var,Expr), errorMy(tokRParen, '")"')
					;  [tokFactor],!, factor(Var,Factor), {Expr =not(Factor)}
					;  [tokMinus],!, factor(Var, Factor), {Expr =.. ['--', Factor]}
					;  [tokNumber(N)], !, { Expr = constant(N) }
					;  test(tokVar(_)), cell(Var,Expr)
					; {se('expresion expected. Try "!", "-", "(",")", number, variable or combination of them!')}
				   ).


				   
additive_op(+) -->   [tokPlus], !.
additive_op(-) -->   [tokMinus].

multiplicative_op(*) -->   [tokTimes], !.
multiplicative_op(//) -->   [tokDiv], !.
multiplicative_op(mod) -->   [tokMod].

rel_op(=) -->   [tokEq], !.
rel_op(<>) -->  [tokNeq], !.
rel_op(<) -->   [tokLt], !.
rel_op(<=) -->  [tokLeq], !.
rel_op(>) -->   [tokGt], !.
rel_op(>=) -->  [tokGeq].				  

/*Interpreter
 Konfiguracje maszyny definujemyjako trojki (C, R, M) tak jak proponowal TWI*/
 
/* Pojawil sie nowy indeks I oznaczajacy numer pozycji dla tablic oraz -1 dla zmiennych*/
lookup(M, X, I, N) :-
   member((X,I,N), M).

update([], X, I, N, [(X,I, N)]).
update([(X,I, _)|T], X, I, N, [(X,I, N)|T]) :-
   !.
update([A|T], X, I,N, [A|S]) :-
   update(T, X, I, N, S).

% Relacja przejscia miedzy stanami maszyny =>:

:- op(800, xfy, =>).

/*Dodatkowe predykaty dla obslugi bledow*/
errI(X,Y):- write('Error: index out of range '), write(X),write('['),write(Y),write(']'), throw(ble).
errN(X,Y):- write('Error: non-assigned variable '), write(X),write('['),write(Y),write(']'), throw(ble).
err(X):- write('Error: non-assigned variable '), write(X), throw(ble).


% Transitions for arithmetic expressions:


([constant(N) | C], R, M) => (C, [N|R], M).
([variable(X) | C], R, M) => (C, [N|R], M) :-
   lookup(M, X, -1, N), (integer(N)->true; err(X)).
([array(X,E) | C], R, M) => ([E, array(X)| C], R, M).
([array(X) | C], [N|R], M) => (C, [V|R], M):- ((N < 0) ->!, errI(X,N)
														; (lookup(M, X, N, V)-> (integer(V)->[]
																			   ; errN(X,N)
																			   )
																			   ; errI(X,N)
														  )
											  ).  
([A | C], R, M) => ([A1, A2, Op | C], R, M) :-
   A =.. [Op, A1, A2],
   member(Op, [+, -, *, //, mod]).
([A | C], R, M) => ([A1, -- | C], R, M):- A=..[--, A1].
([Op | C], [N2, N1 | R], M) => (C, [N | R], M) :-
   member(Op, [+, -, *, //, mod]),
   K =.. [Op, N1, N2],
   N is K.
([-- | C], [N1 | R], M) => (C, [N | R], M) :-
   N is -N1.


% Transitions for Boolean expressions:

([B | C], R, M) => ([A1, A2, Op | C], R, M) :-
   B =.. [Op, A1, A2],
   member(Op, [<, <=, >, >=, =, <>]).
([Op | C], [N2, N1 | R], M) => (C, [B | R], M) :-
   member(Op, [<, >, >=, =]),
   (call(Op, N1, N2) -> B = 1; B = 0).
([<= | C], [N2, N1 | R], M) => (C, [B | R], M) :-
   (N1 =< N2 -> B = 1; B = 0).
([<> | C], [N2, N1 | R], M) => (C, [B | R], M) :-
   (N1 =\= N2 -> B = 1; B = 0).
([not(B) | C], R, M) => ([B, not | C], R, M).
([not | C], [0 | R], M) => (C, [1 | R], M):-!.
([not | C], [_ | R], M) => (C, [0 | R], M).	/*kazda wartosc rozna od 0 traktowana jest jako true*/

([B | C], R, M) => ([B1, B2, Op | C], R, M) :-
   B =.. [Op, B1, B2],
   member(Op, [or, and]).
([or | C], [B2, B1 | R], M) => (C, [B | R], M) :-
   (B1 =\= 0 ; B2 =\= 0) -> B = 1; B = 0.
([and | C], [B2, B1 | R], M) => (C, [B | R], M) :-
   (B1 =\= 0 , B2 =\= 0) -> B = 1; B = 0.

% Transitions for commands:

([{C1}| C], R, M) => ([C1|C], R, M).
([empty | C], R, M) => (C, R, M).
([abort | _], R, M) => ([], R, M).
([C1 ';;' C2 | C], R, M) => ([C1, C2 | C], R, M).


([subs(variable(X),E) | C], R, M) => ([E,subs(variable(X))|C], R, M).
([subs(variable(X))|C], [V|R], M) => (C,R, M1):- update(M,X,-1,V,M1).
([subs(array(X,E1),E2) | C], R, M) => ([E1,E2,subs(array(X))|C], R, M).
([subs(array(X))|C], [V,N | R], M)=> (C,R,M1):- ((N < 0) -> !, errI(X,N)
														; (lookup(M, X, N, _) -> update(M, X, N, V, M1)
																			 ; errI(X,N)
														  )
												  ). 

([var([]) | C], R, M) => (C, R, M).
([var([variable(X)|Rest]) | C], R, M) => ([var(Rest) | C], R, M1):- update(M, X, -1, _,M1).
([var([variable(X,E)|Rest]) | C], R, M) => ([var(Rest), subs(variable(X), E) | C], R, M1):- update(M, X, -1, _,M1).
([var([array(X,E)|Rest]) | C], R, M) => ([E, var([array(X)|Rest]) | C], R, M).
([var([array(X)|Rest]) | C], [1 | R], M) => ([var(Rest) | C], R, M1):- !, update(M, X, 0, _,M1). 
([var([array(X)|Rest]) | C], [N | R], M) => ([var([array(X)|Rest]) | C], [N1| R], M1):- 
							((N > 1) -> N1 is N - 1, update(M, X, N1, _, M1)
									; err('Array must have positive size')
							).

([read(variable(X)) | C], R, M) => (C, R, M1):- read(N), update(M, X, -1, N, M1).
([read(array(X,E)) | C], R, M) => ([E, read(array(X))|C], R, M).
([read(array(X)) | C], [N|R], M) => (C, R, M1):- ((N < 0) -> !,  errI(X,N) /*moze byc zmienna o tej samej nazwie)*/
														;  (lookup(M, X, N, _) -> 
																		  read(V), update(M, X, N, V, M1)
																		  ; errI(X,N)
														  )
												  ). 
	
	
([write(E) | C], R, M) => ([E, write|C], R, M).
([write | C], [N|R], M) => (C, R, M):- write(N), nl.
											
([if(B,C1,empty) | C], R, M) => ([B, if | C], [C1 | R], M):-!.
([if | C], [0, _C1 | R], M) => (C, R, M):-!.
([if | C], [_, C1 | R], M) => ([C1 | C], R, M).

([if(B,C1,C2) | C], R, M) => ([B, if_else | C], [C1, C2 | R], M).
([if_else | C], [0, _C1, C2 | R], M) => ([C2 | C], R, M):-!.
([if_else | C], [_, C1, _C2 | R], M) => ([C1 | C], R, M).


([while(B,C1) | C], R, M) => ([B, while | C], [B, C1 | R], M).
([while | C], [0, _B, _C1 | R], M) => (C, R, M):- !.
([while | C], [_, B, C1 | R], M) => ([C1, while(B, C1) | C], R, M).


% Finally the reflexive and transitive closure:

debugger(InitState,X) :-
   InitState => NextState,
   !,
   ((X =true) ->  write(InitState),  nl
			; true
			),
   debugger(NextState,X).
debugger(FinState, _) :-
   write(FinState),
   nl.

run(CharCodeList,X):-
   phrase(lexer(TokList), CharCodeList),
   catch(phrase(program(Absynt), TokList), ble, false), catch(debugger(([Absynt], [], []),X), ble, false).