(define (Q.Token.Plus? v)
  (equal? v 'Q.Token.Plus))

(define (Q.Token.Plus?? v)
  (equal? v 'Q.Token.Plus))

(define (Q.Token.Plus)
  'Q.Token.Plus)

(define (Q.Token.Plus->string t)
  (unless (Q.Token.Plus? t)
    (error 'Q.Token.Plus->string
	   "Not a Q.Token.Plus"
	   t))
  "+")
