(define (Q.Expr.Add? v)
  (and (pair? v)
       (equal? (car v) 'Q.Expr.Add)))

(define (Q.Expr.Add?? v)
  (and (list? v)
       (= (length v) 3)
       (equal? (car v) 'Q.Expr.Add)
       (Q.Expr? (cadr v))
       (Q.Expr? (caddr v))))

(define (Q.Expr.Add lhs rhs)
  (let ([e (list 'Q.Expr.Add lhs rhs)])
    (if (not (Q.Expr.Add?? e))
	(error 'Q.Expr.Add
	       "Invalid Q.Expr.Add"
	       e)
	e)))

(define (Q.Expr.Add:lhs e)
  (unless (Q.Expr.Add? e)
    (error 'Q.Expr.Add:lhs
	   "Not a Q.Expr.Add"
	   e))
  (cadr e))

(define (Q.Expr.Add:rhs e)
  (unless (Q.Expr.Add? e)
    (error 'Q.Expr.Add:rhs
	   "Not a Q.Expr.Add"
	   e))
  (caddr e))

(define (Q.Expr.Add->string e)
  (unless (Q.Expr.Add? e)
    (error 'Q.Expr.Add->string
	   "Not a Q.Expr.Add"
	   e))
  (let* ([lhs (Q.Expr.Add:lhs e)]
	 [lhsStr (Q.Expr->string lhs)]
	 [rhs (Q.Expr.Add:rhs e)]
	 [rhsStr (Q.Expr->string rhs)])
    (when #f ;; TODO 
      (set! lhsStr (string-append "(" lhsStr ")")))
    (when (or (Q.Expr.Add? rhs)
	      (Q.Expr.Sub? rhs))
      (set! rhsStr (string-append "(" rhsStr ")")))
    (string-append lhsStr " + " rhsStr)))
