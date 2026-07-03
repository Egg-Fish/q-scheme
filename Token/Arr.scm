(define (Q.Token.Arr? v)
  (equal? v 'Q.Token.Arr))

(define (Q.Token.Arr?? v)
  (equal? v 'Q.Token.Arr))

(define (Q.Token.Arr)
  'Q.Token.Arr)

(define (Q.Token.Arr->string t)
  (unless (Q.Token.Arr? t)
    (error 'Q.Token.Arr->string
	   "Not a Q.Token.Arr"
	   t))
  "->")
