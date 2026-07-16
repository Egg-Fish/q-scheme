(define (Q.Expr.App? v)
  (and (pair? v)
       (equal? (car v) 'Q.Expr.App)))

(define (Q.Expr.App?? v)
  (and (list? v)
       (= (length v) 3)
       (equal? (car v) 'Q.Expr.App)
       (Q.Expr? (cadr v))
       (Q.Expr? (caddr v))))

(define (Q.Expr.App lhs rhs)
  (let ([e (list 'Q.Expr.App lhs rhs)])
    (if (not (Q.Expr.App?? e))
	(error 'Q.Expr.App
	       "Invalid Q.Expr.App"
	       e)
	e)))

(define (Q.Expr.App:lhs e)
  (unless (Q.Expr.App? e)
    (error 'Q.Expr.App:lhs
	   "Not a Q.Expr.App"
	   e))
  (cadr e))

(define (Q.Expr.App:rhs e)
  (unless (Q.Expr.App? e)
    (error 'Q.Expr.App:rhs
	   "Not a Q.Expr.App"
	   e))
  (caddr e))

(define (Q.Expr.App->string e)
  (unless (Q.Expr.App? e)
    (error 'Q.Expr.App->string
	   "Not a Q.Expr.App"
	   e))
  (let* ([lhs (Q.Expr.App:lhs e)]
	 [lhsStr (Q.Expr->string lhs)]
	 [rhs (Q.Expr.App:rhs e)]
	 [rhsStr (Q.Expr->string rhs)])
    (when (or (Q.Expr.Lam? lhs)
	      (Q.Expr.Add? lhs)
	      (Q.Expr.Sub? lhs)
	      (Q.Expr.Mul? lhs))
      (set! lhsStr (string-append "(" lhsStr ")")))
    (when (or (Q.Expr.Lam? rhs)
	      (Q.Expr.Add? rhs)
	      (Q.Expr.Sub? rhs)
	      (Q.Expr.Mul? rhs)
	      (Q.Expr.App? rhs))
      (set! rhsStr (string-append "(" rhsStr ")")))
    (string-append lhsStr " " rhsStr)))
