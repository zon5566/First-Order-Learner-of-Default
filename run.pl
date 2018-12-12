create_background([]).
create_background([H|T]):-
	asserta(H),
	create_background(T).


/*
     To run the FOLD algorithm, you need to input the background, 
   positive examples, negative examples, the goal, and the predicates
   used in the run/0 to run the algorithm.
       After finishing the input, just import run.pl and fold.pl in
   the terminal and type "run.".
*/ 

run :-
	
	B = [
		sunny(d1), hot(d1), high(d1), weak(d1),
		sunny(d2), hot(d2), high(d2), strong(d2),
		overcast(d3), hot(d3), high(d3), weak(d3),
		rain(d4), mild(d4), high(d4), weak(d4),
		rain(d5), cool(d5), normal(d5), weak(d5),
		rain(d6), cool(d6), normal(d6), strong(d6),
		overcast(d7), cool(d7), normal(d7), strong(d7),
		sunny(d8), mild(d8), high(d8), weak(d8),
		sunny(d9), cool(d9), normal(d9), weak(d9),
		rain(d10), mild(d10), normal(d10), weak(d10),
		sunny(d11), mild(d11), normal(d11), strong(d11),
		overcast(d12), mild(d12), high(d12), strong(d12),
		overcast(d13), hot(d13), normal(d13), weak(d13),
		rain(d14), mild(d14), high(d14), strong(d14)
	],
	Pos = [d3,d4,d5,d7,d9,d10,d11,d12,d13],
	Neg = [d1,d2,d6,d8,d14],
	Predicates = [sunny,overcast,rain,hot,mild,cool,high,normal,strong,weak],
	Goal = ski,

	/*
	B = [
		bird(X):-(penguin(X),!),
		bird(a),
		bird(b),
		cat(c),
		penguin(d)
	],
	Pos = [a,b,jet],
	Neg = [c,d],
	Predicates = [bird, cat, penguin],
	*/
	/*
	B = [
		bird(X):-penguin(X),
		penguin(X):-superpenguin(X),
		bird(a),
		bird(b),
		penguin(c),
		penguin(d),
		superpenguin(e),
		cat(f),
		plane(g),
		plane(h),
		plane(i),
		damaged(h),
		damaged(i)
	],
	Pos = [a, b, e, g],
	Neg = [c,d,f,h,i],
	Predicates = [bird, penguin, superpenguin, cat, plane, damaged],
	Goal = fly,
	*/

	create_background(B),
	fold(Goal, Pos, Neg, B, Predicates, D, AB, false).
	%writeln(D),
	%writeln(AB).
	