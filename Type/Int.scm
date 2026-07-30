(define (Q.Type.Int? v)
  (equal? v 'Q.Type.Int))

(define (Q.Type.Int?? v)
  (equal? v 'Q.Type.Int))

(define (Q.Type.Int)
  'Q.Type.Int)

(define (Q.Type.Int->string t)
  (unless (Q.Type.Int? t)
    (error 'Q.Type.Int->string
	   "Not a Q.Type.Int"
	   t))
  "Int")
