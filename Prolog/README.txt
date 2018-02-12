This library lets you manipulate multivariable polynomials, utilizing [SWI-Prolog].
Here are listed the function in this library, with a brief description.

Predicate is_varpower(Expression)
The predicate is true if Expression is the intern representation of a varpower.

Predicate is_monomial(Expression)
The predicate is true if Expression is the intern representation of a monomial.

Predicate is_polynomial(Expression)
The predicate is true if Expression is the intern representation of a polynomial.

Predicate as_monomial(Expression, Monomial)
The predicate is true when Monomial is the intern representation resultant from the parsing of the expression Expression. The monomial generated is sorted and reduced.
Examples:
	1 ?- as_monomial(5 * 2/2 * x^2 * y^3, R).
	R = m(5, 5, [v(2, x), v(3, y)]).

	2 ?- as_monomial(42, R).
	R = m(42, 0, []).

	;as-monomial without coefficients
	3 ?- as_monomial(x^2 * y^3, R).
	R = m(1, 5, [v(2, x), v(3, y)]).

	;as-monomial variables with more symbols
	4 ?- as_monomial(5 * symbols^3, R).
	R = m(5, 3, [v(3, symbols)]).

	;as-monomial variables minimization
	21 ?- as_monomial(w * x^0 * i^6 * y^0, R).
	R = m(1, 7, [v(6, i), v(1, w)]).

Predicate get_monomial(Expression, UnsortedMonomial)
The predicate is true when UnsortedMonomial is the unsorted intern representation resultant from the parsing of the expression Expression.
Part of as_monomial.

Predicate solve_pow
(Expression, UnsortedVarpowerList)
The predicate is true when UnsortedVarpowerList is the inter representation of Expression, which must be with variables only.
Part of as_monomial.

Predicate sort_m
(Monomial, SortedMonomial)
The predicate is true when SortedMonomial is the sorted internal representation of Monomial.

Predicate reduce_same_vars(VarpowerList, ReducedVarpowerList)
The predicate is true when ReducedVarpowerList is the reduced inter representation of VarpowerList, obtained by unifying the same variables.
Part of sort_m.

Predicate as_polynomial(Expression, Polynomial)
The predicate is true when Polynomial is the internal representation resultant from the parsing of the expression Expression. The polynomial generated is sorted and reduced.
Examples:
	1 ?- as_polynomial(4*y^3+x^2, R).
	R = poly([m(1, 2, [v(2, x)]), m(4, 3, [v(3, y)])]).
	
	;as-polynomial with only a number 
	2 ?- as_polynomial(42, R).
	R = poly([m(42, 0, [])]).

	;as-polynomial with 0
	as_polynomial(0, R).
	R = poly([])

Predicate list_monomials(Expression, UnsortedListofMonomials)
The predicate is true when UnsortedListofMonomials is the internal representation resultant from the parsing of the expression Expression.
Part of as_polynomial.


Predicate sort_poly(UnsortedPolynomial, Polynomial)
The predicate is true when Polynomial is UnsortedPolynomial sorted and reduced.

Predicate compare_monomials(Ord, Monomial1, Monomial2)
The predicate is true when Ord is < and Monomial1 is less than Monomial2 or otherwise when Ord is > and Monomial1 isn't less than Monomial2.
Since this predicate will be used in presort, which removes duplicates, equal is treated as >.
Part of sort_poly.

Predicate reduce_monomials(Monomials, ReducedMonomials)
The predicate is true when ReducedMonomials is the reduced version of Monomials, obtained by calling both reduce_same_monomials and remove_zeros.
Part of sort_poly.

Predicate reduce_same_monomials(Monomials, ReducedMonomials)
The predicate is true when ReducedMonomials is the reduced version of Monomials, obtained by unifying the monomials with the same variables.
Part of reduce_monomials.

Predicate remove_zeros(Monomials, ReducedMonomials)
The predicate is true when Result is the reduced version of Monomials, obtained by removing the monomials with 0 for their coefficients.
Part of reduce_monomials.

Predicate check_input(Expression, Polynomial)
The predicate is true when Polynomial is the polynomial intern representation of Expression.

Predicate monomials(Polynomial, Monomials)
The predicate is true when Monomials is the sorted list of the monomials Polynomial.
Example:
	1 ?- monomials(-42 + 2 * x + y, R).
	R = [m(-42, 0, []), m(2, 1, [v(1, x)]), m(1, 1, [v(1, y)])].

Predicate coefficients(Polynomial, Coefficients)
The predicate is true when Coefficients is the list of the coefficients of Polynomial's monomials.
Example:
	1 ?- coefficients(-42 + 2 * x + y, R).
	R = [-42, 2, 1].

Predicate variables(Polynomial, Variables)
The predicate is true when Variables is the list of the variables in Polynomial, sorted in ascending alphabetical order.
Example:
	1 ?- variables(-42 + 2 * x + y, R).
	R = [x, y].

Predicate maxdegree (Polynomial, Degree)
The predicate is true when Degree is the highest degree of the monomials appearing in Polynomial.
Example:
	1 ?- maxdegree(-42 + 2 * x^2 + y, R).
	R = 2.

Predicate mindegree (Polynomial, Degree)
The predicate is true when Degree is the lowest degree of the monomials appearing in Polynomial.
Example:
	1 ?- mindegree(-42 + 2 * x^2 + y,R).
	R = 0.

Predicate polyplus(Poly1, Poly2, Result)
The predicate is true when Result is the polynomial sum of Poly1 and Poly2.
Example:
	1 ?- polyplus(x * y * z, -x * y * z, R).
	R = poly([]).

	2 ?- polyplus(x * y * z, -x, R).
	R = poly([m(-1, 1, [v(1, x)]), m(1, 3, [v(1, x), v(1, y), v(1, z)])]).

Predicate polyminus(Poly1, Poly2, Result)
The predicate is true when Result is the polynomial subtraction of Poly1 and Poly2.
Example:
	1 ?- polyminus(x * y * z, -x * y * z, R).
	R = poly([m(2, 3, [v(1, x), v(1, y), v(1, z)])]).

	2 ?- polyminus(x * y * z, -x, R).
	R = poly([m(1, 1, [v(1, x)]), m(1, 3, [v(1, x), v(1, y), v(1, z)])]).

Predicate polytimes(Poly1, Poly2, Result)
The predicate is true when Result is the polynomial product of Poly1 and Poly2.
Example:
	1 ?- polytimes(3 + x, -1, R).
	R = poly([m(-3, 0, []), m(-1, 1, [v(1, x)])]).

Predicate mtimes(Monomials, Monomial, Result)
The predicate mtimes is true when Result is a list of the polynomial products of the monomials in the list Monomials and the monomial Monomial.
Part of polytimes. 

Predicate polyval(Polynomial, VariableValues, Value)
The predicate is true when Value is the value of the polynomials Polynomial in the n-dimensional point represented by the list VariableValues, which contain a value for every variable obtained from the predicate variables/2.
Example:
	16 ?- polyval(-5 * x^2 * y^3, [2,3], Value).
	Value = -540.

Predicate pprint_polynomial(Polynomial)
The predicate is true after printing to the standard output a representation traditional polynomial term associated with polynomial.
Examples:
	1 ?- pprint_polynomial(poly([m(1, 2, [v(2, y)]), m(42, 3, [v(3, x)])])).
	y^2 + 42 x^3 
	true.