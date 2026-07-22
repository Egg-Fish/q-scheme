(define (Q.Expr.Mul? v)
  (and (pair? v)
       (equal? (car v) 'Q.Expr.Mul)))

(define (Q.Expr.Mul?? v)
  (and (list? v)
       (= (length v) 3)
       (equal? (car v) 'Q.Expr.Mul)
       (Q.Expr? (cadr v))
       (Q.Expr? (caddr v))))

(define (Q.Expr.Mul lhs rhs)
  (let ([e (list 'Q.Expr.Mul lhs rhs)])
    (if (not (Q.Expr.Mul?? e))
	(error 'Q.Expr.Mul
	       "Invalid Q.Expr.Mul"
	       e)
	e)))

(define (Q.Expr.Mul:lhs e)
  (unless (Q.Expr.Mul? e)
    (error 'Q.Expr.Mul:lhs
	   "Not a Q.Expr.Mul"
	   e))
  (cadr e))

(define (Q.Expr.Mul:rhs e)
  (unless (Q.Expr.Mul? e)
    (error 'Q.Expr.Mul:rhs
	   "Not a Q.Expr.Mul"
	   e))
  (caddr e))

(define (Q.Expr.Mul->string e)
  (unless (Q.Expr.Mul? e)
    (error 'Q.Expr.Mul->string
	   "Not a Q.Expr.Mul"
	   e))
  (let* ([lhs (Q.Expr.Mul:lhs e)]
	 [lhsStr (Q.Expr->string lhs)]
	 [rhs (Q.Expr.Mul:rhs e)]
	 [rhsStr (Q.Expr->string rhs)])
    (when (or (Q.Expr.Let? lhs)
	      (Q.Expr.Lam? lhs)
	      (Q.Expr.Eq? lhs)
	      (Q.Expr.Add? lhs)
	      (Q.Expr.Sub? lhs))
      (set! lhsStr (string-append "(" lhsStr ")")))
    (when (or (Q.Expr.Let? rhs)
	      (Q.Expr.Lam? rhs)
	      (Q.Expr.Eq? rhs)
	      (Q.Expr.Add? rhs)
	      (Q.Expr.Sub? rhs)
	      (Q.Expr.Mul? rhs))
      (set! rhsStr (string-append "(" rhsStr ")")))
    (string-append lhsStr " * " rhsStr)))
