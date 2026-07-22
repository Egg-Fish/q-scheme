(define (Q.Token.In? v)
  (equal? v 'Q.Token.In))

(define (Q.Token.In?? v)
  (equal? v 'Q.Token.In))

(define (Q.Token.In)
  'Q.Token.In)

(define (Q.Token.In->string t)
  (unless (Q.Token.In? t)
    (error 'Q.Token.In->string
	   "Not a Q.Token.In"
	   t))
  "in")
