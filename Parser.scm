


(define (Q.Parser.parse tokens)
  (Q.Parser.parseExpr tokens
		      (lambda (expr ts)
			(if (null? ts)
			    expr
			    (error 'Q.Parser.parse "Unconsumed tokens" ts)))
		      (lambda (ts)
			(error 'Q.Parser.parse "Could not parse" ts))
		      (lambda (msg ts resume)
			(display "ERROR: ")
			(display msg)
			(newline)
			(resume ts))))



;; kSucc : Q.Expr -> [Q.Token] -> _
;; kFail : [Q.Token] -> _
;; kErr : string -> [Q.Token] -> ([Q.Token] -> _) -> _


(define (Q.Parser.parseExpr tokens kSucc kFail kErr)
  ((Q.Parser.combine Q.Parser.parseAdd
		     Q.Parser.parseApp
		     Q.Parser.parseVar
		     Q.Parser.parseInt)
   tokens
   kSucc
   kFail
   kErr))


(define (Q.Parser.parseAdd tokens kSucc kFail kErr)
  (define parseOperand (Q.Parser.combine Q.Parser.parseApp
					 Q.Parser.parseVar
					 Q.Parser.parseInt))

  (define parseOperator (Q.curry Q.Parser.parseToken Q.Token.Plus?))

  (parseOperand tokens
		(lambda (lhs ts)
		  (let loop ([expr lhs]
			     [ts ts])
		    (if (null? ts)
			(if (Q.Expr.Add? expr)
			    (kSucc expr ts)
			    (kFail tokens))
			(parseOperator ts
				       (lambda (_ ts)
					 (parseOperand ts
						       (lambda (e ts)
							 (loop (Q.Expr.Add expr e)
							       ts))
						       (lambda (ts)
							 (if (Q.Token.Plus? (car ts))
							     (kErr "Consecutive +'s, skipping"
								   ts
								   (lambda _
								     (loop expr
									   ts)))
							     (kErr "Could not parse rhs"
								   ts
								   (lambda _ #f))))
						       kErr))
				       (lambda _
					 (if (Q.Expr.Add? expr)
					     (kSucc expr ts)
					     (kFail tokens)))
				       kErr))))
		kFail
		kErr))


(define (Q.Parser.parseApp tokens kSucc kFail kErr)
  (define parseFunc (Q.Parser.combine Q.Parser.parseVar))

  (define parseArg (Q.Parser.combine Q.Parser.parseVar
				     Q.Parser.parseInt))

  (define (parseArgs tokens kSucc kFail kErr)
    (parseArg tokens
	      (lambda (a ts)
		((Q.Parser.star parseArg)
		 ts
		 (lambda (as ts)
		   (kSucc (cons a as)
			  ts))
		 kFail
		 kErr))
	      kFail
	      kErr))

  (parseFunc tokens
	     (lambda (f ts)
	       (parseArgs ts
			  (lambda (as ts)
			    (kSucc (Q.fold Q.Expr.App f as)
				   ts))
			  kFail
			  (lambda _
			    (kFail tokens))))
	     kFail
	     kErr))



(define (Q.Parser.parseVar tokens kSucc kFail kErr)
  (if (null? tokens)
      (kErr "Unexpected EOF"
	    tokens
	    (lambda (tokens) #f))
      (if (Q.Token.Ident? (car tokens))
	  (kSucc (Q.Expr.Var (Q.Token.Ident:name (car tokens)))
		 (cdr tokens))
	  (kFail tokens))))

(define (Q.Parser.parseInt tokens kSucc kFail kErr)
  (if (null? tokens)
      (kErr "Unexpected EOF"
	    tokens
	    (lambda (tokens) #f))
      (if (Q.Token.Int? (car tokens))
	  (kSucc (Q.Expr.Int (Q.Token.Int:value (car tokens)))
		 (cdr tokens))
	  (kFail tokens))))

(define (Q.Parser.combine p . ps)
  (if (null? ps)
      p
      (lambda (tokens kSucc kFail kErr)
	(p tokens
	   kSucc
	   (lambda _
	     ((apply Q.Parser.combine ps)
	      tokens
	      kSucc
	      kFail
	      kErr))
	   kErr))))


;; kSucc : [Q.Expr] -> [Q.Token] -> _
;; kFail : [Q.Token] -> _
;; kErr : [string] -> [Q.Token] -> ([Q.Token] -> _) -> _

(define (Q.Parser.star p)
  (lambda (tokens kSucc kFail kErr)
    (let loop ([es (list)]
	       [tokens tokens])
      (let ([return (lambda (tokens)
		      (kSucc (reverse es)
			     tokens))])

	(if (null? tokens)
	    (return tokens)
	    (p tokens
	       (lambda (e tokens)
		 (loop (cons e es)
		       tokens))
	       (lambda (tokens)
		 (return tokens))
	       kErr))))))




;; kSucc : Q.Token -> [Q.Token] -> _
;; kFail : [Q.Token] -> _
;; kErr : [string] -> [Q.Token] -> ([Q.Token] -> _) -> _

(define (Q.Parser.parseToken token? tokens kSucc kFail kErr)
  (if (null? tokens)
      (kErr "Unexpected EOF"
	    tokens
	    (lambda (tokens) #f))
      (if (token? (car tokens))
	  (kSucc (car tokens)
		 (cdr tokens))
	  (kFail tokens))))
