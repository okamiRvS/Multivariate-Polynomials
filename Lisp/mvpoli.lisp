;;;; 806792 Busnelli Marco
;;;; 807191 Cocca Umberto

;Given a symbolic expression, returns T if the input given is the intern 
;representation of a monomial, NIL otherwise.
(defun is-monomial (m)
	(and (listp m)
		(eq 'm (first m))
		(let ((mtd (monomial-total-degree m))
			(vps (monomial-vars-and-powers m)))
		(and (integerp mtd)
			(>= mtd 0)
			(listp vps)
			(every #'is-varpower vps)))))

;Given a monomial in its intern representation, returns its total degree.
;Part of is-monomial.
(defun monomial-total-degree (in)
	(third in))

;Given a monomial in its intern representation, return a list with all its 
;varpowers. Part of is-monomial.	
(defun monomial-vars-and-powers (in)
	(fourth in))

;Given a symbolic expression, returns T if the input given is the intern
;representation of a varpower, NIL otherwise.
(defun is-varpower(vp)
	(and (listp vp)
		(eq 'v (first vp))
		(let ((p (varpower-power vp))
			(v (varpower-symbol vp)))
		(and (integerp p)
			(>= p 0)
			(symbolp v)))))

;Given a varpower in its inten representation, return its power.
;Part of is-varpower.
(defun varpower-power (in)
	(second in))

;Given a varpower in its inten representation, return its variable symbol.
;Part of is-varpower.
(defun varpower-symbol (in)
	(third in))

;Given a symbolic expression, returns T if the input given is the intern 
;representation of a polynomial, NIL otherwise.
(defun is-polynomial (p)
	(and (listp p)
		(eq 'poly (first p))
		(let ((ms (poly-monomials p)))
		(and (listp ms)
			(every #'is-monomial ms)))))

;Given a polynomial in its inten representation, return a list with its
;monomials. Part of is-polynomial.
(defun poly-monomials (in)
	(second in))

;The as-monomial function returns the data structure (list) that represents
;the monomial resultant from "Parsing" expression Expression. 
;The monomial generated is sorted.
(defun as-monomial (in) 
	(let ((mon (get-monomial in)))
		(sort-m mon)))

;Given an monomial expression, return its unsorted intern representation.
;Part of as-monomial.
(defun get-monomial (in)
	(cond
		((numberp in) (list 'm in 0 nil))
		((symbolp in) (list 'm 1 1 (list (list 'v 1 in))))
		((equal (first in) 'expt)
			(list 'm 1 (third in) (list (list 'v (third in) (second in)))))
		((and (equal (first in) '*) (symbolp (second in)))
			(list 'm 1 'td (solve-pow (rest in))))
		((and
			(equal (first in) '*)
			(listp (second in))
			(equal (first (second in)) 'expt))
				(list'm 1 'td (solve-pow (rest in))))
		((and (equal (first in) '*) (numberp (eval (second in))))
			(list 'm (eval (second in)) 'td (solve-pow (rest (rest in)))))
		((and (equal (first in) '*) (null (second in)))
			'(m 0 0 ()))
		(T (error "Input not accepted"))))

;Given an expression with only variables, returns its intern representation.
;Part of as-monomial.
(defun solve-pow (vars)
	(cond
		((null vars) nil)
		((atom (first vars))
			(cons
				(list 'v 1 (first vars))
				(solve-pow (rest vars))))
		((equal (first (first vars)) 'expt)
			(if
				(= (third (first vars)) 0)
				(solve-pow (rest vars))
					(cons
						(list
							'v
							(third (first vars))
							(second (first vars)))
						(solve-pow (rest vars)))))))

;Given a monomial in its intern representation, return the monomial properly
;sorted and reduced.
(defun sort-m (in)
	(if
		(equal (second in) 0)
		(list 'm 0 0 nil)
		(list
			'm
			(second in)
			(reduce '+ (fourth in) :key 'second)
			(reduce-same-vars
				(sort (copy-list (fourth in)) 'string< :key 'third)))))
	
;Given a varpower list in its inter representation, compacts it unifying
;the same variables. Part of sort-m.
(defun reduce-same-vars (vars)
	(cond
		((null vars) NIL)
		((equal (third (first vars)) (third (second vars)))
			(reduce-same-vars (cons 
				(list
					'v
					(+	(second (first vars)) (second (second vars)))
					(third (first vars)))
				(rest (rest vars)))))
		(T (cons (first vars) (reduce-same-vars (rest vars))))))

;The as-polynomial function returns the data structure (list) that represents 
;the monomial resultant from "Parsing" expression. Unify the same vars in 
;a monomial. The polynomial generated is sorted.
(defun as-polynomial (in)
	(if
		(null in)
		(list 'p NIL)
		(sort-poly (list 'poly (list-monomials in)))))

;Given an expression, return an unsorted list of monomials in their intern
;representation.
;Part of as-polynomial.
(defun list-monomials (in)
	(cond
		((and (listp in) (equal (first in) '+))
			(mapcar 'as-monomial (rest in)))
		((null in) NIL)
		(T
			(list (as-monomial in)))))

;Given a structure Monomial, returns list of variables.
(defun vars-of (in)
	(if 
		(is-monomial in)
		(variables in)
		(vars-of (first (second (check-input in))))))

;The monomials function returns list - sorted - the monomials
;that appear in Poly.
(defun monomials (in)
	(if
		(is-polynomial in)
		(second (sort-poly in))
		(monomials (check-input in))))

;The variables function returns a list of variable symbols that appear in Poly,
;sorted in ascending alphabetical order.
(defun variables (in)
	(sort 
		(copy-list
			(remove-duplicates (mapcar
				(lambda (x) (third x))
				(varpowers in))))
		'string<))

;Given a structure Monomial, return its total degree TotalDegree.
(defun monomial-degree (in)
	(if
		(equal (first in) 'm)
		(third in)
		(monomial-degree (as-monomial in))))

;Given a Monomial structure, returns its coefficient Coefficient.
(defun monomial-coefficient (in)
	(if
		(equal (first in) 'm)
		(second in)
		(monomial-coefficient (as-monomial in))))

;The coefficients function returns a list of coefficients of Poly.
(defun coefficients (in)
	(if
		(is-polynomial in)
		(if
			(null (second in))
			(list 0)
			(mapcar 'monomial-coefficient (second in)))
		(coefficients (check-input in))))

;Given a Monomial structure, returns list of varpowers VP-list.
(defun varpowers (in)
	(cond
		((is-polynomial in)
			(if 
				(null (second in))
				NIL
				(append
				(varpowers (first (second in)))
				(varpowers (list 'poly (rest (second in)))))))
		((is-monomial in)
			(fourth in))
		(T (varpowers (check-input in)))))

;Given a polynomial, return a list of its monomials.
(defun get-monomials (in)
	(if
		(is-polynomial in)
		(second in)
		(check-input in)))

;The maxdegree function returns the maximum degree of the monomials
;that appear in Poly.
(defun maxdegree (in)
	(if 
		(is-polynomial in)
		(if 
			(null (second in))
			0
			(apply 'max (mapcar (lambda (x) (third x)) (second in))))
		(maxdegree (check-input in))))

;The mindegree function returns the minimum degree of the monomials
;that appear in Poly.
(defun mindegree (in)
	(if 
		(is-polynomial in)
			(if
				(null (second in))
				0
				(apply 'min (mapcar (lambda (x) (third x)) (second in))))
		(mindegree (check-input in))))

;Given a polynomial in its intern representation, return the polynomial
;properly sorted and reduced.
(defun sort-poly (in)
	(list
		(first in)
		(remove-zeros
			(reduce-same-monomials
				(sort (copy-list (second in)) 'monomial<)))))

;Given 2 monomials in their intern representation, return T if the first
;monomial is less than the second, NIL otherwise.
(defun monomial< (x y)
	(cond
		((and (null (fourth x)) (null (fourth y))) NIL)
		((< (third x) (third y)) T)
		((= (third x) (third y)) 
			(cond
				((equal
					0
					(string<
						(third (first (fourth x)))
						(third (first (fourth y)))))
					T)
				((string=
					(third (first (fourth x)))
					(third (first (fourth y))))
					(cond
						((<
							(second (first (fourth x)))
							(second (first (fourth y))))
							T)
						((equal
							(second (first (fourth x)))
							(second (first (fourth y))))
							(monomial<
								(list
									(first x)
									(second x)
									(third x)
									(rest (fourth x)))
								(list
									(first y)
									(second y)
									(third y)
									(rest (fourth y)))))
						(T NIL)))
				(T NIL)))
		(T NIL)))

;Given a list of monomials in its inter representation, compacts it 
;unifying the same variables.
(defun reduce-same-monomials (in)
	(cond
		((null in) NIL)
		((null (second in)) in)
		(())
		((equal (varpowers (first in)) (varpowers (second in)))
			(reduce-same-monomials 
				(cons 
					(list
						'm
						(+
							(monomial-coefficient (first in))
							(monomial-coefficient (second in)))
						(monomial-degree (first in))
						(varpowers (first in)))
					(rest (rest in)))))
		(T (cons (first in) (reduce-same-monomials (rest in))))))

;Given a list of monomials in its inter representation, removes the monomials
;with 0 for their coefficients.
(defun remove-zeros (in)
	(cond 
		((null in) NIL)
		((zerop (monomial-coefficient (first in)))
			(remove-zeros (rest in)))
		(T (cons (first in) (remove-zeros (rest in))))))

;Given a symbolic Expression, return its polynomial inter representation.
(defun check-input (in)
	(cond 
		((is-polynomial in) (sort-poly in))
		((is-monomial in)
			(list
				'poly
				(list (sort-m in))))
		((if
			(listp in)
			(equal (first in) '+)
			NIL) 
			(as-polynomial in))
		(T (check-input (as-monomial in)))))

;The polyplus function generates the polynomial sum of Poly1 and Poly2.
(defun polyplus (x y)
	(if
		(and (is-polynomial x) (is-polynomial y))
		(sort-poly
			(list
				'poly
				(append (second x) (second y))))
		(polyplus (check-input x) (check-input y))))

;The polyminus function generates the polynomial difference of Poly1 and Poly2.
(defun polyminus (x y)
	(polyplus
		x
		(polytimes
			y
			(as-polynomial '(+ -1)))))

;The polytimes function returns the polynomial in inter representation
;resultant from the multiplication of Poly1 and Poly2.
(defun polytimes (x y)
	(if
	(and (is-polynomial x) (is-polynomial y))
	(cond
		((null (second y)) (list 'poly NIL))
		((< (length (second x)) (length (second y)))
			(polytimes y x))
		(T
			(polyplus 
				(list
					'poly
					(mapcar
						(lambda (l) (mtimes l (first (second y))))
						(second x))) 
				(polytimes
					x
					(list 'poly (rest (second y)))))))
	(polytimes (check-input x) (check-input y))))

;Given 2 monomials in their intern representation, return the monomial
;result of their product.
(defun mtimes (x y)
	(if 
		(and (is-monomial x) (is-monomial y))
		(sort-m
			(list
				'm
				(* (second x) (second y))
				(+ (third x) (third y))
				(append (fourth x) (fourth y))))
		(mtimes
			(first (second (check-input x)))
			(first (second (check-input y))))))

;The polyval function returns the value of the polynomial Polynomial Value,
;in n-dimensional point from the list represented VariableValues, which 
;contains a value for each variable obtained with the variables function.
(defun polyval (poly vals)
	(if
		(is-polynomial poly)
		(reduce
			'+
			(mapcar
				(lambda (x) (mval x vals (variables poly)))
				(second poly)))
		(polyval (check-input poly) vals)))

;Given a single monomial in its intern representation Monomial, its variables
;Variables and their value VariableValues, mval returns its value Value.
;Part of polyval.
(defun mval (mon vals vars &optional (acc nil))
	(cond
		((and (null (fourth mon)) (null acc))
			(second mon))
		((null vals)
			(error "Not enough values in input"))
		((equal
			(third (first (fourth mon)))
			(first vars))
			(*
				(expt 
					(first vals)
					(second (first (fourth mon))))
				(mval
					(list
						'm
						(second mon)
						'td
						(rest (fourth mon)))
					vals
					vars
					acc)))
		((null (fourth mon))
			(mval
				(list
					'm
					(second mon)
					'td
					acc)
				(rest vals)
				(rest vars)))
		(T 
			(mval
				(list
					'm
					(second mon)
					'td
					(rest (fourth mon)))
				vals
				vars
				(cons (first (fourth mon)) acc)))))

;The pprint-polynomial function returns NIL after printing (the
;"standard output") a representation traditional polynomial term 
;associated with polynomial.
(defun pprint-polynomial (in)
	(let ((ms (second in)))
		(cond
			((and (equal (first in) 'poly) (null ms))
				(format t "0"))
			((and
				(equal (second (first ms)) 'pad)
				(null (fourth (first ms)))
				(null (second ms)))
				nil)
			((and
				(equal (second (first ms)) 'pad)
				(null (fourth (first ms))))
				(if 
					(> (second (second ms)) 0)
					(append
						(format t "+ ")
						(pprint-polynomial
							(list
								'poly
								(rest ms))))
					(append
						(format t "- ")
						(pprint-polynomial
							(list
								'poly
								(cons
									(list
										'm
										(- (second (second ms)))
										'td
										(fourth (second ms)))
									(rest (rest ms))))))))
			((equal (second (first ms)) 'pad)
				(let ((var (first (fourth (first ms)))))
					(append
						(if 
							(equal (second var) 1)
							(format t "~s " (third var))
							(format t "~s^~d " (third var) (second var))))
						(pprint-polynomial
							(list
								'poly
								(cons
									(list 
										'm
										(second (first ms))
										'td
										(rest (fourth (first ms))))
									(rest ms))))))
			((< (second (first ms)) 1)
				(append
					(format t "-")
					(pprint-polynomial
						(list 
							'poly
							(cons
								(list
									'm
									(- (second (first ms)))
									'td
									(fourth (first ms)))
								(rest ms))))))
			((equal (second (first ms)) 1)
				(append
					(if (null (fourth (first ms)))
						(format t "1")
						NIL)
					(pprint-polynomial
						(list 
							'poly
							(cons
								(list
									'm
									'pad
									'td
									(fourth (first ms)))
								(rest ms))))))
			(T
				(append
					(format t "~d " (second (first ms)))
					(pprint-polynomial
						(list 
							'poly
							(cons
								(list
									'm
									'pad
									'td
									(fourth (first ms)))
								(rest ms)))))))))