(define (Q.Token.RP? v)
  (equal? v 'Q.Token.RP))

(define (Q.Token.RP?? v)
  (equal? v 'Q.Token.RP))

(define (Q.Token.RP)
  'Q.Token.RP)

(define (Q.Token.RP->string t)
  (unless (Q.Token.RP? t)
    (error 'Q.Token.RP->string
	   "Not a Q.Token.RP"
	   t))
  ")")
