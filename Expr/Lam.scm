(define (Q.Expr.Lam? v)
  (and (pair? v)
       (equal? (car v) 'Q.Expr.Lam)))

(define (Q.Expr.Lam?? v)
  (and (list? v)
       (= (length v) 3)
       (equal? (car v) 'Q.Expr.Lam)
       (Q.Expr.Var? (cadr v))
       (Q.Expr? (caddr v))))

(define (Q.Expr.Lam param body)
  (let ([e (list 'Q.Expr.Lam param body)])
    (if (not (Q.Expr.Lam?? e))
	(error 'Q.Expr.Lam
	       "Invalid Q.Expr.Lam"
	       e)
	e)))

(define (Q.Expr.Lam:param e)
  (unless (Q.Expr.Lam? e)
    (error 'Q.Expr.Lam:param
	   "Not a Q.Expr.Lam"
	   e))
  (cadr e))

(define (Q.Expr.Lam:body e)
  (unless (Q.Expr.Lam? e)
    (error 'Q.Expr.Lam:body
	   "Not a Q.Expr.Lam"
	   e))
  (caddr e))

(define (Q.Expr.Lam->string e)
  (unless (Q.Expr.Lam? e)
    (error 'Q.Expr.Lam->string
	   "Not a Q.Expr.Lam"
	   e))
  (let* ([param (Q.Expr.Lam:param e)]
	 [paramStr (Q.Expr->string param)]
	 [body (Q.Expr.Lam:body e)]
	 [bodyStr (Q.Expr->string body)])
    (string-append paramStr " - " bodyStr)))
