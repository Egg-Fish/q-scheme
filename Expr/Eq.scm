(define (Q.Expr.Eq? v)
  (and (pair? v)
       (equal? (car v) 'Q.Expr.Eq)))

(define (Q.Expr.Eq?? v)
  (and (list? v)
       (= (length v) 3)
       (equal? (car v) 'Q.Expr.Eq)
       (Q.Expr? (cadr v))
       (Q.Expr? (caddr v))))

(define (Q.Expr.Eq lhs rhs)
  (let ([e (list 'Q.Expr.Eq lhs rhs)])
    (if (not (Q.Expr.Eq?? e))
	(error 'Q.Expr.Eq
	       "Invalid Q.Expr.Eq"
	       e)
	e)))

(define (Q.Expr.Eq:lhs e)
  (unless (Q.Expr.Eq? e)
    (error 'Q.Expr.Eq:lhs
	   "Not a Q.Expr.Eq"
	   e))
  (cadr e))

(define (Q.Expr.Eq:rhs e)
  (unless (Q.Expr.Eq? e)
    (error 'Q.Expr.Eq:rhs
	   "Not a Q.Expr.Eq"
	   e))
  (caddr e))

(define (Q.Expr.Eq->string e)
  (unless (Q.Expr.Eq? e)
    (error 'Q.Expr.Eq->string
	   "Not a Q.Expr.Eq"
	   e))
  (let* ([lhs (Q.Expr.Eq:lhs e)]
	 [lhsStr (Q.Expr->string lhs)]
	 [rhs (Q.Expr.Eq:rhs e)]
	 [rhsStr (Q.Expr->string rhs)])
    (when (or (Q.Expr.Lam? lhs))
      (set! lhsStr (string-append "(" lhsStr ")")))
    (when (or (Q.Expr.Lam? rhs))
      (set! rhsStr (string-append "(" rhsStr ")")))
    (string-append lhsStr " = " rhsStr)))
