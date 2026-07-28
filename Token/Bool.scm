(define (Q.Token.Bool? v)
  (and (pair? v)
       (equal? (car v) `Q.Token.Bool)))

(define (Q.Token.Bool?? v)
  (and (list? v)
       (= (length v) 2)
       (equal? (car v) 'Q.Token.Bool)
       (boolean? (cadr v))))

(define (Q.Token.Bool value)
  (let ([t (list 'Q.Token.Bool value)])
    (if (not (Q.Token.Bool?? t))
	(error 'Q.Token.Bool
	       "Invalid Q.Token.Bool"
	       t)
	t)))

(define (Q.Token.Bool:value t)
  (unless (Q.Token.Bool? t)
    (error 'Q.Token.Bool:value
	   "Not a Q.Token.Bool"
	   t))
  (cadr t))

(define (Q.Token.Bool->string t)
  (unless (Q.Token.Bool? t)
    (error 'Q.Token.Bool->string
	   "Not a Q.Token.Bool"
	   t))
  (if (Q.Token.Bool:value t)
      "true"
      "false"))
