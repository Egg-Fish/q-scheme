(define (Q.Type? v)
  (or (Q.Type.Fun? v)
      (Q.Type.Int? v)
      (Q.Type.Bool? v)))

(define (Q.Type?? v)
  (or (Q.Type.Fun?? v)
      (Q.Type.Int?? v)
      (Q.Type.Bool?? v)))

(define (Q.Type->string e)
  (unless (Q.Type? e)
    (error 'Q.Type->string
	   "Not a Q.Type"
	   e))

  (cond
   [(Q.Type.Fun? e)
    (Q.Type.Fun->string e)]
   [(Q.Type.Int? e)
    (Q.Type.Int->string e)]
   [(Q.Type.Bool? e)
    (Q.Type.Bool->string e)]
   [else
    (error 'Q.Type->string
	   "Exhausted"
	   e)]))
