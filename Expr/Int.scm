(define (Q.Expr.Int? v)
  (and (pair? v)
       (equal? (car v) 'Q.Expr.Int)))

(define (Q.Expr.Int?? v)
  (and (list? v)
       (= (length v) 2)
       (equal? (car v) 'Q.Expr.Int)
       (integer? (cadr v))))

(define (Q.Expr.Int value)
  (let ([e (list 'Q.Expr.Int value)])
    (if (not (Q.Expr.Int?? e))
	(error 'Q.Expr.Int
	       "Invalid Q.Expr.Int"
	       e)
	e)))

(define (Q.Expr.Int:value e)
  (unless (Q.Expr.Int? e)
    (error 'Q.Expr.Int:value
	   "Not a Q.Expr.Int"
	   e))
  (cadr e))

(define (Q.Expr.Int->string e)
  (unless (Q.Expr.Int? e)
    (error 'Q.Expr.Int->string
	   "Not a Q.Expr.Int"
	   e))
  (number->string (Q.Expr.Int:value e)))
