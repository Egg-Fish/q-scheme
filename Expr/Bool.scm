(define (Q.Expr.Bool? v)
  (and (pair? v)
       (equal? (car v) 'Q.Expr.Bool)))

(define (Q.Expr.Bool?? v)
  (and (list? v)
       (= (length v) 2)
       (equal? (car v) 'Q.Expr.Bool)
       (boolean? (cadr v))))

(define (Q.Expr.Bool value)
  (let ([e (list 'Q.Expr.Bool value)])
    (if (not (Q.Expr.Bool?? e))
	(error 'Q.Expr.Bool
	       "Invalid Q.Expr.Bool"
	       e)
	e)))

(define (Q.Expr.Bool:value e)
  (unless (Q.Expr.Bool? e)
    (error 'Q.Expr.Bool:value
	   "Not a Q.Expr.Bool"
	   e))
  (cadr e))

(define (Q.Expr.Bool->string e)
  (unless (Q.Expr.Bool? e)
    (error 'Q.Expr.Bool->string
	   "Not a Q.Expr.Bool"
	   e))
  (if (Q.Expr.Bool:value e)
      "true"
      "false"))
