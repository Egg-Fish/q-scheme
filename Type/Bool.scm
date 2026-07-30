(define (Q.Type.Bool? v)
  (equal? v 'Q.Type.Bool))

(define (Q.Type.Bool?? v)
  (equal? v 'Q.Type.Bool))

(define (Q.Type.Bool)
  'Q.Type.Bool)

(define (Q.Type.Bool->string t)
  (unless (Q.Type.Bool? t)
    (error 'Q.Type.Bool->string
	   "Not a Q.Type.Bool"
	   t))
  "Bool")
