(define (Q.Token.Eq? v)
  (equal? v 'Q.Token.Eq))

(define (Q.Token.Eq?? v)
  (equal? v 'Q.Token.Eq))

(define (Q.Token.Eq)
  'Q.Token.Eq)

(define (Q.Token.Eq->string t)
  (unless (Q.Token.Eq? t)
    (error 'Q.Token.Eq->string
	   "Not a Q.Token.Eq"
	   t))
  "=")
