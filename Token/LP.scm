(define (Q.Token.LP? v)
  (equal? v 'Q.Token.LP))

(define (Q.Token.LP?? v)
  (equal? v 'Q.Token.LP))

(define (Q.Token.LP)
  'Q.Token.LP)

(define (Q.Token.LP->string t)
  (unless (Q.Token.LP? t)
    (error 'Q.Token.LP->string
	   "Not a Q.Token.LP"
	   t))
  "(")
