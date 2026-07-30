(define (Q.Type.Fun? v)
  (and (pair? v)
       (equal? (car v) 'Q.Type.Fun)))

(define (Q.Type.Fun?? v)
  (and (list? v)
       (= (length v) 3)
       (equal? (car v) 'Q.Type.Fun)
       (Q.Type? (cadr v))
       (Q.Type? (caddr v))))

(define (Q.Type.Fun lhs rhs)
  (let ([e (list 'Q.Type.Fun lhs rhs)])
    (if (not (Q.Type.Fun?? e))
	(error 'Q.Type.Fun
	       "Invalid Q.Type.Fun"
	       e)
	e)))

(define (Q.Type.Fun:lhs e)
  (unless (Q.Type.Fun? e)
    (error 'Q.Type.Fun:lhs
	   "Not a Q.Type.Fun"
	   e))
  (cadr e))

(define (Q.Type.Fun:rhs e)
  (unless (Q.Type.Fun? e)
    (error 'Q.Type.Fun:rhs
	   "Not a Q.Type.Fun"
	   e))
  (caddr e))

(define (Q.Type.Fun->string e)
  (unless (Q.Type.Fun? e)
    (error 'Q.Type.Fun->string
	   "Not a Q.Type.Fun"
	   e))
  (let* ([lhs (Q.Type.Fun:lhs e)]
	 [lhsStr (Q.Type->string lhs)]
	 [rhs (Q.Type.Fun:rhs e)]
	 [rhsStr (Q.Type->string rhs)])
    (when #f
      (set! lhsStr (string-append "(" lhsStr ")")))
    (when #f
      (set! rhsStr (string-append "(" rhsStr ")")))
    (string-append lhsStr " -> " rhsStr)))
