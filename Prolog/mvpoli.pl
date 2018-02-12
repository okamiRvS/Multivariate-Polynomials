%%%% 806792 Busnelli Marco
%%%% 807191 Cocca Umberto

%The predicate is true if the expression is the intern representation
%of a varpower.
is_varpower(v(Power, VarSymbol)) :-
integer(Power),
Power >= 0,
atom(VarSymbol).

%The predicate is true if the expression is the intern representation
%of a monomial.
is_monomial(m(_C, TD, VPs)) :-
	integer(TD),
	TD >= 0,
	is_list(VPs).

%The predicate is true if the expression is the intern representation
%of a polynomial.
is_polynomial(poly(Monomials)) :-
	is_list(Monomials),
	foreach(member(M, Monomials), is_monomial(M)).

%The predicate is true when R is the intern representation resultant from 
%the parsing of the expression In. 
%The monomial generated is sorted and reduced.
as_monomial(In, R) :-
	get_monomial(In, ToBeSorted),
	sort_m(ToBeSorted, R).	

%The predicate is true when the output is the unsorted intern representation 
%resultant from the parsing of the expression In.
%Part of as_monomial.
get_monomial([], m(0,0,[])) :- !.

get_monomial(In, m(-1, Exp, [v( Exp, Var)])) :-
	In =.. [-, R],
	!,
	solve_pow(R, v(Exp, Var)).

get_monomial(In, m(1, Exp, [v(Exp, Var)])) :-	
	solve_pow(In, v(Exp, Var)),
	!.

%variables
get_monomial(In, m(Coef, NTD, [v(Exp,Var)|Vars])) :-
	In =.. [*, Sp1, Sp2],
	solve_pow(Sp2, v(Exp, Var)),
	!,
	get_monomial(Sp1, m(Coef, TD, Vars)),
	NTD is TD + Exp.

%coef
get_monomial(In, m(Num, 0, [])) :-
	Num is In.

%The predicate is true when the output is the inter representation 
%of X, which must be with variables only.
%Part of as_monomial.
solve_pow(X, v(1,X)) :-
	atom(X),
	!.

solve_pow(X, v(Exp, Var)) :-
	X =.. [^, Var, Exp],
	atom(Var).

%The predicate is true when the output is the sorted internal
%representation of monomial.
sort_m(m(0, _, _), m(0, 0, [])) :- !.

sort_m(m(Cf, TD, Vars), m(Cf, TD, RVars)) :-
	sort(2, @=<, Vars, SVars),
	reduce_same_vars(SVars, RVars).

%The predicate is true when Out is the reduced inter representation of 
%the varpower list, obtained by unifying the same variables.
reduce_same_vars([], []).

reduce_same_vars([v(0,_)| Tail], Out) :-
	!,
	reduce_same_vars(Tail, Out).

reduce_same_vars([v(Cf1, X), v(Cf2, X)|Tail], Out) :-
	!,
	NewCf is Cf1 + Cf2,
	reduce_same_vars([v(NewCf, X)|Tail], Out).

reduce_same_vars([H|Tail], [H|Out]) :-
	reduce_same_vars(Tail, Out).

%The predicate is true when the output, that is a polynomial, is
%the internal representation resultant from the parsing of the expression In.
%The polynomial generated is sorted and reduced.
as_polynomial(In, poly(Out)) :-
	list_monomials(In, Ms),
	sort_poly(Ms, SMs),
	reduce_monomials(SMs, Out).

%The predicate is true when the output is the internal
%representation resultant from the parsing of the expression In.
%Part of as_polynomial.
list_monomials([], []) :- !.

%split with +
list_monomials(In, [M|Out]) :-
	In =.. [Op, N1, N2],
	Op == '+',
	!,
	as_monomial(N2, M),
	list_monomials(N1, Out).

%split with -
list_monomials(In, [m(NCf,TD,Vars)|Out]) :-
	In =.. [Op, N1, N2],
	Op == '-',
	!,
	as_monomial(N2, m(Cf, TD, Vars)),
	NCf is Cf * -1,
	monomials(N1, Out).

%unsplittable with + and -
list_monomials(In, [M]) :-
	as_monomial(In, M).

%The predicate is true when Out is the polynomial In sorted and reduced.
sort_poly(In, Out) :-
	predsort(compare_monomials, In, Out).

%The Predicate compare_monomials(Ord, Monomial1, Monomial2) is true when
%Ord is < and Monomial1 is less than Monomial2 or otherwise when Ord
%is > and Monomial1 isn't less than Monomial2. Since this predicate will
%be used in presort, which removes duplicates, equal is treated as >. 
%Part of sort_poly.
compare_monomials(<, m(_, TD1, _), m(_, TD2, _)) :-
	compare(<, TD1, TD2),
	!.

compare_monomials(<, m(_, TD, [v(_, V1)|_]), m(_, TD, [v(_, V2)|_])) :-
	compare(<, V1, V2),
	!.

compare_monomials(< ,m(_, TD, [v(E1, V)|_]), m(_, TD, [v(E2, V)|_])) :-
	compare(<, E1, E2),
	!.

compare_monomials(Ord, m(_, TD, [v(E, V)|Vars1]), m(_, TD, [v(E, V)|Vars2])) :-
	!,
	compare_monomials(Ord, m(_, TD, Vars1), m(_, TD, Vars2)).

compare_monomials(>, _, _).

%The predicate is true when Out is the reduced version of the monomials In,
%obtained by calling both reduce_same_monomials and remove_zeros.
%Part of sort_poly.
reduce_monomials(In, Out) :-
	reduce_same_monomials(In, WithZeros),
	remove_zeros(WithZeros, Out).

%The predicate is true when Out is the reduced version of monomials in input,
%obtained by unifying the monomials with the same variables.
%Part of reduce_monomials.
reduce_same_monomials([], []).

reduce_same_monomials([m(Cf1, TD, Vars), m(Cf2, TD, Vars)|Tail], Out) :-
	!,
	TotCf is Cf1 + Cf2,
	reduce_same_monomials([m(TotCf, TD, Vars)|Tail], Out).

reduce_same_monomials([H|Tail], [H|Out]) :-
	reduce_same_monomials(Tail, Out).

%The predicate is true when Out is the reduced version of the monomials
%in input, obtained by removing the monomials with 0 for their coefficients.
%Part of reduce_monomials.
remove_zeros([], []).

remove_zeros([m(0,_,_)|Tail], Out) :-
	!,
	remove_zeros(Tail, Out).

remove_zeros([H|Tail], [H|Out]) :-
	remove_zeros(Tail, Out).

%The predicate is true when the polynomial in output is the intern 
%representation of the input.
check_input(poly(Ms), poly(Out)) :-
	is_polynomial(poly(Ms)),
	!,
	sort_poly(Ms, SMs),
	reduce_monomials(SMs, Out).

check_input(m(Cf, TD, Vars), poly([m(Cf, TD, RVars)])) :-
	is_monomial(m(Cf, TD, Vars)),
	!,
	sort(2, @=<, Vars, SVars),
	reduce_same_vars(SVars, RVars).

check_input(I, O) :-
	as_polynomial(I, O).

%The predicate is true when the monomials in Out are the sorted list of the 
%monomials in input.
monomials(poly(Ms), Out) :-
	!,
	sort_poly(Ms, SMs),
	reduce_monomials(SMs, Out).

monomials(In, Out) :-
	check_input(In, Ck),
	monomials(Ck, Out).

%The predicate is true when the coefficients in Out are the list of the 
%coefficients of polynomial's monomial in input.
coefficients(In, Out) :-
	check_input(In, Ck),
	coefficients_(Ck, Out).

coefficients_(poly([]), []) :- !.

coefficients_(poly([m(Coef, _, _)|Tail]), [Coef|Coefficients]) :-
	coefficients_(poly(Tail), Coefficients).

%The predicate is true when Variables is the list of the variables 
%in Poly, sorted in ascending alphabetical order.
variables([], []) :- !.

variables(Poly, Variables) :-
	check_input(Poly, Ck),
	variables(Ck, [], VarsWithDubs),
	list_to_set(VarsWithDubs, ToBeSorted),
	sort(ToBeSorted, Variables).

variables(poly([]), [], Out) :-
	!,
	variables([], Out).

variables(poly([m(_, _, [])|Monomials]), Acc, Out) :-
	!,
	reverse(Acc, Done),
	append(Done, NewOut, Out),
	variables(poly(Monomials), [], NewOut).

variables(poly([m(_, _, [v(_,Var)|Vars])|Monomials]), Acc , Out) :-
	variables(poly([m(_, _, Vars)|Monomials]), [Var|Acc], Out).

%%The predicate is true when MaxDegree is the highest degree of the 
%monomials appearing in Polynomial.
maxdegree(poly([]), 0) :- !.

maxdegree(poly(Monomials), MaxDegree) :-
	sort(2, @>=, Monomials, [m(_, MaxDegree, _)|_]),
	!.

maxdegree(In, Out) :-
	check_input(In, Ck),
	maxdegree(Ck, Out).

%The predicate is true when MinDegree is the lowest degree of the 
%monomials appearing in Polynomial.
mindegree(poly([]), 0) :- !.

mindegree(poly(Monomials), MinDegree) :-
	sort(2, @=<, Monomials, [m(_, MinDegree, _)|_]),
	!.

mindegree(In, Out) :-
	check_input(In, Ck),
	mindegree(Ck, Out).

%The predicate is true when MonsSum are the monomials obtained from
%the polinomial sum of the list of monomials Mons1 and Mons2.
polyplus(poly(Mons1), poly(Mons2), poly(MonsSum)) :-
	!,
	append(Mons1, Mons2, Raw),
	sort_poly(Raw, Sorted_ok),
	reduce_monomials(Sorted_ok, MonsSum).

polyplus(In1, In2, Out) :-
	check_input(In1, Ck1),
	check_input(In2, Ck2),
	polyplus(Ck1, Ck2, Out).

%The predicate is true when Out is the Poly1 less Poly2.
polyminus(P1, P2, Out) :-
	!,
	polytimes(P2, -1, Minus),
	polyplus(P1, Minus, Out).

polyminus(P1, P2, Out) :-
	check_input(P2, Ck),
	polyminus(P1, Ck, Out).

%The predicate is true when R is the polynomial product of Poly1 and Poly2.
polytimes(_, poly([]), poly([])) :- !.

polytimes(poly(Mons1), poly(Mons2), R) :-
	length(Mons1, L1),
	length(Mons2, L2),
	(L1 < L2),
	!,
	polytimes(poly(Mons2), poly(Mons1), R).

polytimes(poly(Mons1), poly([M|Mons2]), R) :-
	!,
	mtimes(Mons1, M, MT),
	polytimes(poly(Mons1), poly(Mons2), NewR),
	polyplus(poly(MT), NewR, R).


polytimes(X, Y, R) :-
	check_input(X, CkX),
	check_input(Y, CkY),
	polytimes(CkX, CkY, R).

%Given a list of monomial and a monomial, the output is the moltiplication
%of each monomial of the list and the monomial. 
%Part of polytimes. 
mtimes([], _, []).

mtimes([m(Cf1, TD1, Vars1)|Mons1], m(Cf2, TD2, Vars2), [NM|R]) :-
	NCf is Cf1 * Cf2,
	NTD is TD1 + TD2,
	append(Vars1, Vars2, NVars),
	sort_m(m(NCf, NTD, NVars), NM),
	mtimes(Mons1, m(Cf2, TD2, Vars2), R).

%The predicate is true when Out is the value of the polynomials in input
%in the n-dimensional point represented by the list Values, which contain
%a value for every variable obtained from the predicate variables/2.
polyval(poly(Mlist), Values, Out) :-
	!,
	variables(poly(Mlist), Vars),
	calc(Mlist, Values, Vars, Out).

polyval(In, Values, Out) :-
	check_input(In, Ck),
	polyval(Ck, Values, Out).

%Given a list of monomials, their variables and their value
%return the total value of the monomials.
calc([], _, _, 0) :- !.

calc([M|Ms], Values, Vars, Out) :-
	mval(M, Values, Vars, [], Val),
	calc(Ms, Values, Vars, NewOut),
	Out is NewOut + Val.

%Given a single monomial, its variables and values returns the total value
%of the monomial the variable in control is the first of the variables list.
mval(m(Coef, _, [v(E,V)|MVs]), [N|Ns], [V|Vs], Acc, Out) :-
	!,
	Exp =.. [^, N, E],
	P is Exp,
	mval(m(Coef, _, MVs), [N|Ns], [V|Vs], Acc, NewOut),
	Out is NewOut * P.

%the variable in control is not the first of the variables list.
mval(m(Coef, _, [MV|MVs]), Values, Vars, Acc, Out) :-
	!,
	mval(m(Coef, _, MVs), Values, Vars, [MV|Acc], Out).

%all variables have been compared with first variable of the list. 
mval(m(Coef, _, []), [_|Ns], [_|Vs], Acc, Out) :-	
	!,
	mval(m(Coef, _, Acc), Ns, Vs, [], Out).

%all variables have been compared with all variables of the list 
%all is multiplied by the coefficient.
mval(m(Coef, _, []), _, [], _, Coef).

%The predicate is true after printing to the standard output a representation
%traditional polynomial term associated with polynomial.
pprint_polynomial([]) :- !.

pprint_polynomial(poly([])) :-
	write(0),
	!.

pprint_polynomial(poly([m(pad, _, [])|[]])) :- !.

pprint_polynomial(poly([m(pad, _, [v(1, Var)| Vars])|Mons])) :-
	!,
	write(Var),
	write(" "),
	pprint_polynomial(poly([m(pad, _, Vars)| Mons])).

pprint_polynomial(poly([m(pad, _, [v(Pow, Var)| Vars])|Mons])) :-
	!,
	write(Var^Pow),
	write(" "),
	pprint_polynomial(poly([m(pad, _, Vars)| Mons])).

pprint_polynomial(poly([m(pad, _, []),m(Coef, _, Vars)|Mons])) :-
	Coef > 0,
	!,
	write("+ "),
	pprint_polynomial(poly([m(Coef, _, Vars)|Mons])).

pprint_polynomial(poly([m(pad, _, []),m(Coef, _, Vars)|Mons])) :-
	!,
	write("- "),
	Abs is Coef * -1,
	pprint_polynomial(poly([m(Abs, _, Vars)|Mons])).

pprint_polynomial(poly([m(Coef, _, Vars)|Mons])) :-
	Coef < 0,
	!,
	write(-),
	Abs is Coef * -1,
	pprint_polynomial(poly([m(Abs, _, Vars)| Mons])).


pprint_polynomial(poly([m(1, _, [])|Mons])) :-
	!,
	write(1),
	pprint_polynomial(poly([m(pad, _, [])|Mons])).

pprint_polynomial(poly([m(1, _, Vars)|Mons])) :-
	!,
	pprint_polynomial(poly([m(pad, _, Vars)| Mons])).

pprint_polynomial(poly([m(Coef, _, Vars)|Mons])) :-
	!,
	write(Coef),
	write(" "),
	pprint_polynomial(poly([m(pad, _, Vars)| Mons])).