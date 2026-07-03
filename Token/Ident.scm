(define (Q.Token.Ident? v)
  (and (pair? v)
       (equal? (car v) 'Q.Token.Ident)))

(define (Q.Token.Ident?? v)
  (and (list? v)
       (= (length v) 2)
       (equal? (car v) 'Q.Token.Ident)
       (string? (cadr v))))

(define (Q.Token.Ident name)
  (let ([t (list 'Q.Token.Ident name)])
    (if (not (Q.Token.Ident?? t))
	(error 'Q.Token.Ident
	       "Invalid Q.Token.Ident"
	       t)
	t)))

(define (Q.Token.Ident:name t)
  (unless (Q.Token.Ident? t)
    (error 'Q.Token.Ident:name
	   "Not a Q.Token.Ident"
	   t))
  (cadr t))


(define (Q.Token.Ident->string t)
  (unless (Q.Token.Ident? t)
    (error 'Q.Token.Ident->string
	   "Not a Q.Token.Ident"
	   t))
  (Q.Token.Ident:name t))
