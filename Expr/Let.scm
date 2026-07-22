(define (Q.Expr.Let? v)
  (and (pair? v)
       (equal? (car v) 'Q.Expr.Let)))

(define (Q.Expr.Let?? v)
  (and (list? v)
       (= (length v) 4)
       (equal? (car v) 'Q.Expr.Let)
       (Q.Expr.Var? (cadr v))
       (Q.Expr? (caddr v))
       (Q.Expr? (cadddr v))))

(define (Q.Expr.Let lhs rhs body)
  (let ([e (list 'Q.Expr.Let lhs rhs body)])
    (if (not (Q.Expr.Let?? e))
	(error 'Q.Expr.Let
	       "Invalid Q.Expr.Let"
	       e)
	e)))

(define (Q.Expr.Let:lhs e)
  (unless (Q.Expr.Let? e)
    (error 'Q.Expr.Let:lhs
	   "Not a Q.Expr.Let"
	   e))
  (cadr e))

(define (Q.Expr.Let:rhs e)
  (unless (Q.Expr.Let? e)
    (error 'Q.Expr.Let:rhs
	   "Not a Q.Expr.Let"
	   e))
  (caddr e))

(define (Q.Expr.Let:body e)
  (unless (Q.Expr.Let? e)
    (error 'Q.Expr.Let:body
	   "Not a Q.Expr.Let"
	   e))
  (cadddr e))

(define (Q.Expr.Let->string e)
  (unless (Q.Expr.Let? e)
    (error 'Q.Expr.Let->string
	   "Not a Q.Expr.Let"
	   e))
  (let* ([lhs (Q.Expr.Let:lhs e)]
	 [lhsStr (Q.Expr->string lhs)]
	 [rhs (Q.Expr.Let:rhs e)]
	 [rhsStr (Q.Expr->string rhs)]
	 [body (Q.Expr.Let:body e)]
	 [bodyStr (Q.Expr->string body)])
    (string-append "let "
		   lhsStr
		   " = "
		   rhsStr
		   " in "
		   bodyStr)))
