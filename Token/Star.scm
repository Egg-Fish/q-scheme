(define (Q.Token.Star? v)
  (equal? v 'Q.Token.Star))

(define (Q.Token.Star?? v)
  (equal? v 'Q.Token.Star))

(define (Q.Token.Star)
  'Q.Token.Star)

(define (Q.Token.Star->string t)
  (unless (Q.Token.Star? t)
    (error 'Q.Token.Star->string
	   "Not a Q.Token.Star"
	   t))
  "*")
