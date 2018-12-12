% INPUT:
%	Goal:
%	Background:
%		The background knowledge
%	Pos:
%		a set of positive example clauses
%	Neg:
%		a set of negative example clauses


:- dynamic n_ab/1.
:- dynamic d/1.
:- dynamic ab/1.
:- dynamic background/1.
:- dynamic predicates/1.

% Background global variable
add_background(X):-
	asserta(background(X)).

% Predicates global variable
add_predicates(X):-
	asserta(predicates(X)).
remove_predicate(X):-
	X =.. [Name, _],
	predicates(L),
	removehelper(Name,L,[],R),
	retract(predicates(_)),
	asserta(predicates(R)).

removehelper(_, [], R, R).
removehelper(X, [H|T], Prev, R):-
	X = H -> removehelper(X, T, Prev, R)
	; removehelper(X, T, [H|Prev], R).


% Current number of exception clause
n_ab(0).
add :- % add the exception predicate index by 1
	n_ab(X),
	NewX is X+1,
	retract(n_ab(X)),
	asserta(n_ab(NewX)).

% Positive rules global variable
d([]).
append_d(X):-
	d(L),
	retract(d(_)),
	append(L, [X], L1),
	asserta(d(L1)).
	
% Negative rules global variable
ab([]).
append_ab(X):-
	ab(L),
	retract(ab(_)),
	append(L, [X], L1),
	asserta(ab(L1)),
	asserta(X).
% ------------------------------------------------- %

append([],L, L).
append([L|M], N, [L|O]):-
	append(M,N,O).

unify_arg(L, Head, LRes):-
	L =.. [LName, _],
	Head =.. [_, HArg],
	LRes =.. [LName, HArg].

clause_body_list_aux([], Rest, Rest).
clause_body_list_aux(Elements, Prev, Rest) :-
	Elements = true ->
		clause_body_list_aux([],Prev,Rest)
	;
	Elements =.. [A, E | T],
	% remove ','
	(A = ',' ->% It means there have at least 2 clauses.
		( E = true ->
			[ClauseRest] = T,
			clause_body_list_aux(ClauseRest, Prev, Rest)
			;
			[ClauseRest] = T,
			clause_body_list_aux(ClauseRest, [E|Prev], Rest)
		)
		;
		( A = true ->
			clause_body_list_aux([], Prev, Rest)
			;
			( A = member ->
				[TBlock] = T,
				BodyPart =.. [A,E,TBlock],
				clause_body_list_aux([], [BodyPart|Prev], Rest)	
				;	
				BodyPart =.. [A,E],
				clause_body_list_aux([], [BodyPart|Prev], Rest)
			)
		)
	).

unify([], _, []).
unify([H|T], HeadArg, [NewH|Res]):-
	H =.. [CurrentName, _],
	NewH =.. [CurrentName, HeadArg],
	unify(T, HeadArg, Res).

% Given the goal and examples, return a list of rules(Clauses).
fold(Goal, Pos, Neg, Background, Predicates, D, AB, IsExcept):-
	add_background(Background),
	add_predicates(Predicates),
	background(B),
	predicates(P),

	writeln("*----------------------------------*"),
	writeln("Background: "), prettyprint(B),
	writeln("*----------------------------------*"),
	write("Example+: "), writeln(Pos),
	writeln("*----------------------------------*"),
	write("Example-: "), writeln(Neg),
	writeln("*----------------------------------*"),
	write("Predicates: "), writeln(P),
	writeln("*----------------------------------*"),
	write("Goal: "), writeln(Goal),
	writeln("*----------------------------------*"),
	writeln("=> Start the FOLD algorithm"),
	fold_loop(Goal, Pos, Neg, IsExcept, [], D1),
	D = D1,
	ab(Y),
	AB = Y,
	writeln("Final Answer:"),
	writeln("D = "), prettyprint(D1),
	writeln("AB = "), prettyprint(Y), nl.

fold_loop(Goal, Pos, Neg, IsExcept, Prev, D) :-
	( Pos = [] -> % if positive examples are empty, then stop.
		Prev = D
		;
		Goal_Predicate =.. [Goal, _],
		specialize(Pos, Neg, (Goal_Predicate:-true), PosClause1, true),
		uncovered_examples(PosClause1, Pos, Pos1),
		write("Updated uncovered positive exampels: "), writeln(Pos1),
		( IsExcept ->
			true
			;
			append_d(PosClause1)
		),
		fold_loop(Goal, Pos1, Neg, IsExcept, [PosClause1|Prev], D)
	).

specialize(Pos, Neg, Clause0, PosClause, Just_started):-
	add_best_literal(Clause0, Pos, Neg, C_def, IG),
	( IG >= 0 ->
		write("The best literal is chosen: "), write(C_def), write(" with IG = "), writeln(IG),
		add_literal(C_def, Clause0, New_Clause, true),
		remove_predicate(C_def)
		;
		( Just_started = true ->
			enumerate(Clause0, Pos, New_Clause)
			;
			writeln("=>Some negative examples are still covered. Do EXCEPTION"),
			exception(Clause0, Neg, Pos, New_Clause),
			( New_Clause = [] ->
				enumerate(Clause0, Pos, New_Clause)
				;
				true
			)
		)
	),
	write("The New Clause: "), writeln(New_Clause),
	uncovered_examples(New_Clause, Pos, Pos1),
	covered_examples(New_Clause, Neg, Neg1),
	write("=> Updated example+: "), writeln(Pos1),
	write("=> Updated example-: "), writeln(Neg1),
	predicates(L),
	write("=> Updated predicates: "), writeln(L),
	
	( Neg1 = [] ->
		PosClause = New_Clause
		;
		specialize(Pos1, Neg1, New_Clause, PosClause, false)
	).

% Pick the clause that generate the largest IG among examples+ and examples-.
add_best_literal(Clause0, Pos, Neg, Clause1, IG):-
	predicates(Ls),
	info_value(Clause0, Pos, Neg, Info),
	best_next_clause(Ls, Pos, Neg, Clause0, Info, -1, _, IG, Clause1).

best_next_clause([], _, _, _, _, Gain, Clause, Gain, Clause).
best_next_clause([L|Ls], Pos, Neg, Clause, Info, Gain0, Best0, Gain, Best):-
	Lliteral =.. [L, _],
	add_literal(Lliteral, Clause, Best1, true),
	compute_gain(Neg, Pos, Info, Best1, Gain1),
	( Gain1 > Gain0 ->
		best_next_clause(Ls, Pos, Neg, Clause, Info, Gain1, Lliteral, Gain, Best)
	; (Gain1 =:= Gain0, Gain1 >=0) ->
		choose_tie_clause(Clause, Lliteral, Best0, Best2),
	    best_next_clause(Ls, Pos, Neg, Clause, Info, Gain0, Best2, Gain, Best)
	 ; best_next_clause(Ls, Pos, Neg, Clause, Info, Gain0, Best0, Gain, Best)
	).

choose_tie_clause(Clause, B0, B1, C):-
	add_literal(B0, Clause, (_:-Body1), true),
	add_literal(B1, Clause, (_:-Body2), true),
	variables_in(Body1, V1),
	length(V1, N1),
	variables_in(Body2, V2),
	length(V2, N2),
	( N2 < N1 -> C = B1 ; C = B0 ).
/*
choose_tie_clause((A1:-B1), (A2:-B2), C) :-
	variables_in(B1, V1),
	length(V1, N1),
	variables_in(B2, V2),
	length(V2, N2),
	( N2 < N1 -> C = (A2:-B2)  ;  C = (A1:-B1) ).
*/
compute_gain(Nxs, Pxs, Info, Clause, Gain) :-
	covered_examples(Clause, Pxs, Retained),
	length(Retained, R),
	( R =:= 0 ->
	      Gain = -1
	; info_value(Clause, Pxs, Nxs, Info1),
	  Gain is R * (Info1 - Info)
	).

% --------------------------------------------------------- %
% Emuerate the positive clauses. Among the set of positive 
% examples (Pos), we pick one clause and insert into the
% clause.
% --------------------------------------------------------- %
enumerate(Clause0, [H|_], Clause1):-
	%Hliteral=.. [H, Arg],
	Clause0 = (Clause0Head :- Clause0Body),
	Clause0Head =.. [_, Arg],
	Hliteral =.. [member, Arg, [H]],
	( Clause0Body = true ->
		Clause1 = (Clause0Head :- Hliteral)
		;
		Clause1 = (Clause0Head :- (Clause0Body, Hliteral))
	).
	%add_literal(Hliteral, Clause0, Clause1, true).

exception((Goal :- Body), Pos, Neg, Clause1):-
	add_best_literal((Goal :- Body), Pos, Neg, _, IG),
	( IG >= 0 ->
		background(Background),
		predicates(Predicates),
		Goal =.. [GoalName, _],
		fold(GoalName, Pos, Neg, Background, Predicates, D_, _, true),
		n_ab(Current_n_ab),
		concat_atom([ab, Current_n_ab], '', C_abName),
		C_ab =.. [C_abName, _],
		add,
		append_rule_to_ab(D_, C_ab),
		add_literal(C_ab, (Goal:-Body), Clause1, false)
		;
		Clause1 = []
	),
	write("The clause generated by EXCEPTION: "), writeln(Clause1).

append_rule_to_ab([], _).
append_rule_to_ab([PosClause|Rest], C_ab):-
	% for each c in D_, create ( C_ab :- bodyof(c) )
	PosClause = (_ :- Body),
	add_literal(Body, (C_ab:-true), C_hat, true),
	append_ab(C_hat),
	append_rule_to_ab(Rest, C_ab).

% --------------------------------------------------------- %
% Construct a list representing the set of variables in Term.
% --------------------------------------------------------- %
variables_in(A, Vs) :- 
	variables_in(A, [], Vs).
	
variables_in(A, V0, V) :-
	var(A), !, 
	ord_add_element(V0, A, V).
variables_in(A, V0, V) :-
	ground(A), !, V = V0. 
variables_in(Term, V0, V) :-
	functor(Term, _, N),
	variables_in_args(N, Term, V0, V).

variables_in_args(N, Term, V0, V) :-
	( N =:= 0 ->
	      V = V0
	; arg(N, Term, Arg),
	  variables_in(Arg, V0, V1),
	  N1 is N-1,
	  variables_in_args(N1, Term, V1, V)
	).

% --------------------------------------------------------- %
% Generate the clause candidates.
% --------------------------------------------------------- %

generate_possible_extensions(Ls):-
	background(L),
	pickup_atoms(L, [], Ls).

pickup_atoms([], Res0, Res0).
pickup_atoms([H|T], Res0, Res1):-	
	( H =.. [_, _] ->
		pickup_atoms(T, [H|Res0], Res1)
	;
		pickup_atoms(T, Res0, Res1)
	).

% --------------------------------------------------------- %
% Compute the term of information gain with the given clause,
% i.e., the logarithm of the proportion of the positive examples
% --------------------------------------------------------- %
info_value(Clause, Pos, Neg, Info):-
	tuples(Clause, Pos, Ptuples),
	length(Ptuples, P),
	( P =:= 0 ->
	      Info = 0
	; tuples(Clause, Neg, Ntuples),
	  length(Ntuples, N),
	  Temp is P / (P + N),
	  log(Temp, Temp1),
	  Info is Temp1 * 1.442695
	).

tuples((_:-B), Xs, Tuples):-
	clause_body_list_aux(B, [], BodyList),
	check(BodyList, Xs, [], Tuples).

check(_, [], Tuples, Tuples).
check(BL, [H|T], Prev, Tuples):-
	(
		satisfy(BL, H)->
		check(BL, T, [H|Prev], Tuples)
		;
		check(BL, T, Prev, Tuples)
	).

satisfy([],_).

satisfy([H|T], Arg):-
	H =.. [HName, _, Arg2],
	HName = member,
	C =.. [HName, Arg, Arg2],
	C,
	satisfy(T, Arg),
	!.

satisfy([H|T], Arg):-
	H =.. [HName, HBody],
	( HName = (\+) ->
		HBody =.. [HHName, _],
		C0 =.. [HHName, Arg],
		C = (\+C0)
		;
		C =.. [HName, Arg]
	),
	C,
	satisfy(T, Arg).

% Xs1 are the examples from Xs that can be proved with the clause
covered_examples((A :- B), Xs, Xs1) :-
	tuples((A:-B), Xs, Xs1).
	
% Xs1 are the examples from Xs that cannot be proved with the clause.
uncovered_examples((A:-B), Xs, Xs1) :-
	tuples((A:-B), Xs, Xs0),
	uncoverhelper(Xs, Xs0, [], Xs1). % Xs - Xs0 = Xs1
	%findall(A, ( member(A, Xs), \+ B ), Xs1 ).

uncoverhelper([], _, Xs1, Xs1).
uncoverhelper([H|T], Xs0, Prev, Xs1):-
	( member(H, Xs0) ->
		uncoverhelper(T, Xs0, Prev, Xs1)
		;
		uncoverhelper(T, Xs0, [H|Prev], Xs1)
	).

add_literal(L, (A :- B), (A :- B1), IsPos) :-
	unify_arg(L, A, LRes0),
	( IsPos ->
		LRes = LRes0
		;
		LRes = (\+LRes0)
	),
	( B = true ->
		B1 = (LRes)
	    ;
		B1 = (B,LRes)
	).

prettyprint([]).
prettyprint([H|T]):-
	portray_clause(H),
	prettyprint(T).