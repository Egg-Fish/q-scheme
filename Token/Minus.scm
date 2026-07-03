(define (Q.Token.Minus? v)
  (equal? v 'Q.Token.Minus))

(define (Q.Token.Minus?? v)
  (equal? v 'Q.Token.Minus))

(define (Q.Token.Minus)
  'Q.Token.Minus)

(define (Q.Token.Minus->string t)
  (unless (Q.Token.Minus? t)
    (error 'Q.Token.Minus->string
	   "Not a Q.Token.Minus"
	   t))
  "-")
