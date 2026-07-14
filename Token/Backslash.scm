(define (Q.Token.Backslash? v)
  (equal? v 'Q.Token.Backslash))

(define (Q.Token.Backslash?? v)
  (equal? v 'Q.Token.Backslash))

(define (Q.Token.Backslash)
  'Q.Token.Backslash)

(define (Q.Token.Backslash->string t)
  (unless (Q.Token.Backslash? t)
    (error 'Q.Token.Backslash->string
	   "Not a Q.Token.Backslash"
	   t))
  "\\")
