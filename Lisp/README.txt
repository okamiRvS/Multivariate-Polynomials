This library lets you manipulate multivariable polynomials, utilizing [Common-LISP].
Here are listed the function in this library, with a brief description.

Function is-monomial
Expression -> T/NIL
Given a symbolic expression, returns T if the input given is the intern representation of a monomial, NIL otherwise.

Function monomial-total-degree
Monomial -> Total Degree
Given a monomial in its intern representation, returns its total degree.
Part of is-monomial.

Function monomial-vars-and-powers
Monomial -> VP-List
Given a monomial in its intern representation, return a list with all its varpowers.
Part of is-monomial.

Function is-varpower
Expression -> T/NIL
Given a symbolic expression, returns T if the input given is the intern representation of a varpower, NIL otherwise.

Function varpower-power
VP-List -> Power
Given a varpower in its inten representation, return its power.
Part of is-varpower.

Function varpower-symbol
VP-List -> Symbol
Given a varpower in its inten representation, return its variable symbol.
Part of is-varpower.

Function is-polynomial
Expression -> T/NIL
Given a symbolic expression, returns T if the input given is the intern representation of a polynomial, NIL otherwise.

Function poly-monomials
Polynomial -> List of Monomials
Given a polynomial in its inten representation, return a list with its monomials.
Part of is-polynomial.

Function as-monomial
Expression -> Monomial
The as-monomial function returns the data structure (list) that represents the monomial resultant from "Parsing" expression Expression. The monomial generated is sorted.
Examples:
	CL-USER 1 > as-monomial '(* (* 5 (/ 2 2)) (expt x 2) (expt y 3))
	(M 5 5 ((V 2 X) (V 3 Y)))

	;as-monomial with only a number	
	CL-USER 3 >as-monomial 5
	(M 5 0 NIL)

	;as-monomial without coefficients
	CL-USER 4 > as-monomial '(* (expt x 2) (expt y 3))
	(M 1 5 ((V 2 X) (V 3 Y)))

	;as-monomial variables with more symbols
	CL-USER 5 > as-monomial '(* 5 (expt symbols 3))
	(M 5 3 ((V 3 SYMBOLS)))

	;as-monomial variables minimization
	CL-USER 6 > as-monomial '(* w (expt x 0) (expt i 6) (expt y 0)))
	(M 1 7 ((V 6 I) (V 1 W)))

Function get-monomial
Expression -> Unsorted Monomial
Given an monomial expression, return its unsorted intern representation.
Part of as-monomial.

Function solve-pow
Expression -> Unsorted Varpower List
Given an expression with only variables, returns its intern representation.
Part of as-monomial.

Function sort-m
Unsorted Monomial -> Monomial
Given a monomial in its intern representation, return the monomial properly sorted and reduced.

Function reduce-same-vars
Varpower List -> Varpower List
Given a varpower list in its inter representation, compacts it unifying the same variables.
Part of sort-m.

Function as-polynomial
Expression -> Polynomial
The as-polynomial function returns the data structure (list) that represents the monomial resultant from "Parsing" expression Expression. The polynomial generated is sorted.
Examples:
	CL-USER 1 > as-polynomial '(+ (* 4 (expt y 3) (expt x 2)))
	(POLY ((M 4 5 ((V 2 X) (V 3 Y)))))
	
	;as-polynomial with only a number 
	CL-USER 2 > as-polynomial 4
	(POLY ((M 4 0 NIL)))

	;as-polynomial of monomial expression
	CL-USER 3 > as-polynomial '(* 4)
	(POLY ((M 4 0 NIL)))

	;as-polynomial with null monomial
	CL-USER 4 > as-polynomial 0
	(POLY NIL)

Function list-monomials
Expression -> Unsorted list of Monomials
Given an expression, return an unsorted list of monomials in their intern representation.
Part of as-polynomial.

Function vars-of
Monomial -> Variables
Given a structure Monomial, returns list of variables.
Example:
	CL-USER 1 : 2 > vars-of '(M 1 2 ((V 1 X) (V 1 Y)))
	(X Y)

Function monomials
Poly -> Monomials
The monomials function returns list - sorted - the monomials that appear in Poly.
Example:
	CL-USER 1 > monomials '(POLY ((M -4 0 NIL) (M 2 1 ((V 1 X))) (M 1 1 ((V 1 Y)))))
	((M -4 0 NIL) (M 2 1 ((V 1 X))) (M 1 1 ((V 1 Y))))

Function variables
Poly -> Variables
The variables function returns a list of variable symbols that appear in Poly, sorted in ascending alphabetical order.
Example:
	CL-USER 1 > variables '(POLY ((M -2 0 NIL) (M 4 1 ((V 1 X))) (M 1 2 ((V 1 X) (V 1 Z)))))
	(X Z)

Function monomial-degree
Monomial -> TotalDegree
Given a structure Monomial, return its total degree TotalDegree.
Example:
	CL-USER 1 : 3 > monomial-degree '(M 1 7 ((V 3 S) (V 3 T) (V 1 Y)))
	7

Function monomial-coefficient
Monomial -> Coefficient
Given a Monomial structure, returns its coefficient Coefficient.
Example:	
	CL-USER 1 : 2 > monomial-coefficient '(M 4 6 ((V 6 X)))
	4


Function coefficients
Poly -> Coefficients
The coefficients function returns a list of coefficients of Poly.
Example:
	CL-USER 1 > coefficients '(POLY ((M -4 0 NIL) (M 2 1 ((V 1 X))) (M 1 1 ((V 1 Y)))))
	(-4 2 1)

Function varpowers
Monomial -> VP-list
Given a Monomial structure, returns list of varpowers VP-list.
Example:
	CL-USER 1 : 2 > varpowers '(M 1 3 ((V 2 X) (V 1 Y)))
	((V 2 X) (V 1 Y))	

Function get-monomials
Polynomial -> List of Monomials
Given a polynomial, return a list of its monomials.

Function maxdegree
Poly -> Degree
The maxdegree function returns the maximum degree of the monomials that appear in Poly.
Example:
	CL-USER 1 > maxdegree '(POLY ((M 3 0 NIL) (M 1 1 ((V 1 X))) (M 1 2 ((V 1 X) (V 1 Y)))))
	2

Function mindegree
Poly -> Degree
The mindegree function returns the minimum degree of the monomials that appear in Poly.
Example:
	CL-USER 1 > mindegree '(POLY ((M 3 0 NIL) (M 1 1 ((V 1 X))) (M 1 2 ((V 1 X) (V 1 Y)))))
	0

Function sort-poly
Unsorted Polynomial -> Polynomial
Given a polynomial in its intern representation, return the polynomial properly sorted and reduced.

Function monomial<
Given 2 monomials in their intern representation, return T if the first monomial is less than teh second, NIL otherwise.

Function reduce-same-monomials
List of Monomials -> List of Monomials
Given a list of monomials in its inter representation, compacts it unifying the same variables.
Part of sort-poly.

Function remove-zereos
List of Monomials -> List of Monomials
Given a list of monomials in its inter representation, removes the monomials with 0 for their coefficients.
Part of sort-poly.

Function check-input
Expression -> Polynomial
Given a symbolic Expression, return its polynomial inter representation.

Function polyplus
Poly1 Poly2 -> Result
The polyplus function generates the polynomial sum of Poly1 and Poly2.
Example:
	CL-USER 1 > setq p1 (as-polynomial '(+ (* -1 x x y)))
	(POLY ((M -1 3 ((V 2 X) (V 1 Y)))))

	CL-USER 2 > setq a (as-polynomial '(+ (* x x y)))
	(POLY ((M 1 3 ((V 2 X) (V 1 Y)))))

	CL-USER 3 > polyplus p1 p2
	(POLY NIL)

Function polyminus
Poly1 Poly2 -> Result
The polyminus function generates the polynomial difference of Poly1 and Poly2.
Example:
	CL-USER 1 > setq p1 (as-polynomial '(+ -1 (* x x y)))
	(POLY ((M -1 0 NIL) (M 1 3 ((V 2 X) (V 1 Y)))))

	CL-USER 2 > setq p2 (as-polynomial '(+ -1 (* x x y)))
	(POLY ((M -1 0 NIL) (M 1 3 ((V 2 X) (V 1 Y)))))

	CL-USER 3 > polyminus p1 p2
	(POLY NIL)

Function polytimes
Poly1 Poly2 -> Result
The polytimes function returns the polynomial in inter representation resultant from the multiplication of Poly1 and Poly2.
Example:
	CL-USER 1 > setq p1 (as-polynomial '(+ (* x) 3))
	(POLY ((M 3 0 NIL) (M 1 1 ((V 1 X)))))

	CL-USER 2 > setq p2 (as-polynomial 1)
	(POLY ((M 1 0 NIL)))

	CL-USER 3 > polytimes p1 p2
	(POLY ((M 3 0 NIL) (M 1 1 ((V 1 X)))))

Function mtimes
Monomial1 Monomial2 -> Result
Given 2 monomials in their intern representation, return the monomial result of their product.

Function polyval
Polynomial VariableValues -> Value
The polyval function returns the value Value of the polynomial Polynomial, in n-dimensional point from the list represented VariableValues, which contains a value for each variable obtained with the variables function.
Example:
	CL-USER 1 > (polyval '(+ (* -5 (expt x 3) (expt y 2))) '(2 3))
	-360

Function mval
Monomial VariableValues Variables -> Value
Given a single monomial in its intern representation Monomial, its variables Variables and their value VariableValues, mval returns its value Value.
Part of polyval.

Function pprint-polynomial
Polynomial -> NIL
The pprint-polynomial function returns NIL after printing to the standard output a representation traditional polynomial term associated with polynomial.
Examples:
	CL-USER 1 > setq p1 (as-polynomial '(+ (* 4 (expt y 3) (expt x 2))))
	(POLY ((M 4 5 ((V 2 X) (V 3 Y)))))

	CL-USER 2 > setq p2 (as-polynomial 0)
	(POLY NIL)

	CL-USER 3 > pprint-polynomial p1
	4 X^2 Y^3 
	NIL

	CL-USER 4 > pprint-polynomial p2
	0
	NIL