(define (Q.Token.Let? v)
  (equal? v 'Q.Token.Let))

(define (Q.Token.Let?? v)
  (equal? v 'Q.Token.Let))

(define (Q.Token.Let)
  'Q.Token.Let)

(define (Q.Token.Let->string t)
  (unless (Q.Token.Let? t)
    (error 'Q.Token.Let->string
	   "Not a Q.Token.Let"
	   t))
  "let")
