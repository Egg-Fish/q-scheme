(define (Q.Expr.Var? v)
  (and (pair? v)
       (equal? (car v) 'Q.Expr.Var)))

(define (Q.Expr.Var?? v)
  (and (list? v)
       (= (length v) 2)
       (equal? (car v) 'Q.Expr.Var)
       (string? (cadr v))))

(define (Q.Expr.Var name)
  (let ([e (list 'Q.Expr.Var name)])
    (if (not (Q.Expr.Var?? e))
	(error 'Q.Expr.Var
	       "Invalid Q.Expr.Var"
	       e)
	e)))

(define (Q.Expr.Var:name e)
  (unless (Q.Expr.Var? e)
    (error 'Q.Expr.Var:name
	   "Not a Q.Expr.Var"
	   e))
  (cadr e))

(define (Q.Expr.Var->string e)
  (unless (Q.Expr.Var? e)
    (error 'Q.Expr.Var->string
	   "Not a Q.Expr.Var"
	   e))
  (Q.Expr.Var:name e))
