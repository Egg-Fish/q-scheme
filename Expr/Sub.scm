(define (Q.Expr.Sub? v)
  (and (pair? v)
       (equal? (car v) 'Q.Expr.Sub)))

(define (Q.Expr.Sub?? v)
  (and (list? v)
       (= (length v) 3)
       (equal? (car v) 'Q.Expr.Sub)
       (Q.Expr? (cadr v))
       (Q.Expr? (caddr v))))

(define (Q.Expr.Sub lhs rhs)
  (let ([e (list 'Q.Expr.Sub lhs rhs)])
    (if (not (Q.Expr.Sub?? e))
	(error 'Q.Expr.Sub
	       "Invalid Q.Expr.Sub"
	       e)
	e)))

(define (Q.Expr.Sub:lhs e)
  (unless (Q.Expr.Sub? e)
    (error 'Q.Expr.Sub:lhs
	   "Not a Q.Expr.Sub"
	   e))
  (cadr e))

(define (Q.Expr.Sub:rhs e)
  (unless (Q.Expr.Sub? e)
    (error 'Q.Expr.Sub:rhs
	   "Not a Q.Expr.Sub"
	   e))
  (caddr e))

(define (Q.Expr.Sub->string e)
  (unless (Q.Expr.Sub? e)
    (error 'Q.Expr.Sub->string
	   "Not a Q.Expr.Sub"
	   e))
  (let* ([lhs (Q.Expr.Sub:lhs e)]
	 [lhsStr (Q.Expr->string lhs)]
	 [rhs (Q.Expr.Sub:rhs e)]
	 [rhsStr (Q.Expr->string rhs)])
    (when #f ;; TODO
      (set! lhsStr (string-append "(" lhsStr ")")))
    (when (or (Q.Expr.Add? rhs)
	      (Q.Expr.Sub? rhs))
      (set! rhsStr (string-append "(" rhsStr ")")))
    (string-append lhsStr " - " rhsStr)))
