(define (Q.Token.Int? v)
  (and (pair? v)
       (equal? (car v) `Q.Token.Int)))

(define (Q.Token.Int?? v)
  (and (list? v)
       (= (length v) 2)
       (equal? (car v) 'Q.Token.Int)
       (integer? (cadr v))))

(define (Q.Token.Int value)
  (let ([t (list 'Q.Token.Int value)])
    (if (not (Q.Token.Int?? t))
	(error 'Q.Token.Int
	       "Invalid Q.Token.Int"
	       t)
	t)))

(define (Q.Token.Int:value t)
  (unless (Q.Token.Int? t)
    (error 'Q.Token.Int:value
	   "Not a Q.Token.Int"
	   t))
  (cadr t))

(define (Q.Token.Int->string t)
  (unless (Q.Token.Int? t)
    (error 'Q.Token.Int->string
	   "Not a Q.Token.Int"
	   t))
  (number->string (Q.Token.Int:value t)))
