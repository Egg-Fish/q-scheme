
(define (Q.Parser.parse tokens)
  (Q.Parser.parseExpr tokens
		      (lambda (expr tokens)
			expr)
		      (lambda (msg tokens resume)
			(display msg)
			(newline)
			(resume))))


(define (Q.Parser.parseExpr tokens kSuccess kError)
  ((Q.Parser.choice Q.Parser.parseLam
		    Q.Parser.parseAddSub) tokens kSuccess kError))


(define (Q.Parser.parseToken token tokens kSuccess kError)
  (if (null? tokens)
      (kError (list "Unexpected EOF")
	      tokens
	      (lambda () #f))
      (if (not (equal? token (car tokens)))
	  (kError (list (string-append "Expected "
				       (Q.Token->string token)
				       ", got "
				       (Q.Token->string (car tokens))))
		  tokens
		  (lambda () #f))
	  (kSuccess (car tokens)
		    (cdr tokens)))))


(define (Q.Parser.parseLam tokens kSuccess kError)
  (let ([kError (lambda (msg tokens resume)
		  (kError (list "While parsing Q.Expr.Lam"
				msg)
			  tokens
			  resume))])
    (Q.Parser.parseToken (Q.Token.Backslash)
			 tokens
			 (lambda (_ tokens)
			   (Q.Parser.parseVar tokens
					      (lambda (param tokens)
						(Q.Parser.parseToken (Q.Token.Arr)
								     tokens
								     (lambda (_ tokens)
								       (Q.Parser.parseExpr tokens
											   (lambda (body tokens)
											     (kSuccess (Q.Expr.Lam param body)
												       tokens))
											   kError))
								     kError))
					      kError))
			 kError)))






(define (Q.Parser.parseBinop tokens
			     parseOperand
			     parseOperator
			     kSuccess
			     kError)
  (parseOperand tokens
		(lambda (lhs tokens)
		  (let loop ([expr lhs]
			     [tokens tokens])
		    (parseOperator tokens
				   (lambda (op t tokens)
				     (parseOperand tokens
						   (lambda (rhs tokens)
						     (loop (op expr rhs)
							   tokens))
						   (lambda (msg tokens resume)
						     (kError (list (string-append "While parsing RHS of "
										  (Q.Expr->string expr)
										  " "
										  (Q.Token->string t))
								   msg)
							     tokens
							     (lambda ()
							       (loop expr
								     tokens))))))
				   (lambda _
				     (kSuccess expr
					       tokens)))))
		(lambda (msg tokens resume)
		  (kError (list "While parsing LHS"
				msg)
			  tokens
			  resume))))


(define (Q.Parser.parseAddSub tokens kSuccess kError)
  (define parseOperand Q.Parser.parseMulDiv)
  (define (parseOperator tokens kSuccess kError)
    (if (or (null? tokens)
	    (not (or (Q.Token.Plus? (car tokens))
		     (Q.Token.Minus? (car tokens)))))
	(kError (list "")
		tokens
		(lambda () #f))
	(kSuccess (cond
		   [(Q.Token.Plus? (car tokens))
		    Q.Expr.Add]
		   [(Q.Token.Minus? (car tokens))
		    Q.Expr.Sub]
		   [else
		    #f])
		  (car tokens)
		  (cdr tokens))))

  (Q.Parser.parseBinop tokens parseOperand parseOperator kSuccess kError))



(define (Q.Parser.parseMulDiv tokens kSuccess kError)
  (define parseOperand (Q.Parser.choice Q.Parser.parseApp
					Q.Parser.parseInt))
  (define (parseOperator tokens kSuccess kError)
    (if (or (null? tokens)
	    (not (or (Q.Token.Star? (car tokens)))))
	(kError (list "")
		tokens
		(lambda () #f))
	(kSuccess (cond
		   [(Q.Token.Star? (car tokens))
		    Q.Expr.Mul]
		   [else
		    #f])
		  (car tokens)
		  (cdr tokens))))

  (Q.Parser.parseBinop tokens parseOperand parseOperator kSuccess kError))


(define (Q.Parser.parseApp tokens kSuccess kError)
  (define parseArg (Q.Parser.choice Q.Parser.parseVar
				    Q.Parser.parseInt
				    Q.Parser.parseParens))

  (parseArg tokens
	    (lambda (f tokens)
	      (let loop ([expr f]
			 [tokens tokens])
		(parseArg tokens
			  (lambda (a tokens)
			    (loop (Q.Expr.App expr a)
				  tokens))
			  (lambda _
			    (kSuccess expr
				      tokens)))))
	    (lambda (msg tokens resume)
	      (kError (list "While parsing application function"
			    msg)
		      tokens
		      resume))))


(define (Q.Parser.parseVar tokens kSuccess kError)
  (if (null? tokens)
      (kError (list "While parsing Q.Expr.Var"
		    (list "Unexpected EOF"))
	      tokens
	      (lambda () (Q.Expr.Var "<EOF>")))
      (if (Q.Token.Ident? (car tokens))
	  (kSuccess (Q.Expr.Var (Q.Token.Ident:name (car tokens)))
		    (cdr tokens))
	  (kError (list "While parsing Q.Expr.Var"
			(list (string-append "Expected identifier, got " (Q.Token->string (car tokens)))))
		  tokens
		  (lambda () (Q.Expr.Var "<unknown var>"))))))


(define (Q.Parser.parseInt tokens kSuccess kError)
  (if (null? tokens)
      (kError (list "While parsing Q.Expr.Int"
		    (list "Unexpected EOF"))
	      tokens
	      (lambda () (Q.Expr.Var "<EOF>")))
      (if (Q.Token.Int? (car tokens))
	  (kSuccess (Q.Expr.Int (Q.Token.Int:value (car tokens)))
		    (cdr tokens))
	  (kError (list "While parsing Q.Expr.Int"
			(list (string-append "Expected integer, got " (Q.Token->string (car tokens)))))
		  tokens
		  (lambda () (Q.Expr.Var "<unknown int>"))))))


(define (Q.Parser.parseParens tokens kSuccess kError)
  (if (null? tokens)
      (kError (list "While parsing (Q.Expr)"
		    (list "Unexpected EOF"))
	      tokens
	      (lambda () (Q.Expr.Var "<EOF>")))
      (if (not (Q.Token.LP? (car tokens)))
	  (kError (list "While parsing (Q.Expr)"
			(list (string-append "Expected (, got " (Q.Token->string (car tokens)))))
		  tokens
		  (lambda ()
		    (Q.Expr.parseExpr (cdr tokens)
				      kSuccess
				      kError)))
	  (Q.Parser.parseExpr (cdr tokens)
			      (lambda (expr tokens)
				(cond
				 [(null? tokens)
				  (kError (list "While parsing (Q.Expr)"
						(list "Unexpected EOF"))
					  tokens
					  (lambda () (Q.Expr.Var "<EOF>")))]
				 
				 [(not (Q.Token.RP? (car tokens)))
				  (kError (list "While parsing (Q.Expr)"
						(list (string-append "Expected ), got " (Q.Token->string (car tokens)))))
					  tokens
					  (lambda ()
					    (kSuccess expr
						      tokens)))]

				 [else
				  (kSuccess expr
					    (cdr tokens))]))
			      (lambda (msg tokens resume)
				(kError (list "While parsing (Q.Expr)"
					      msg)
					tokens
					resume))))))




(define (Q.Parser.choice p . ps)
  (lambda (tokens kSuccess kErrors)
    (p tokens
       kSuccess
       (lambda (msg tokens resume)
	 (if (null? ps)
	     (kErrors (list msg)
		      tokens
		      resume)
	     ((apply Q.Parser.choice ps)
	      tokens
	      kSuccess
	      (lambda (msgs tokens _)
		(kErrors (cons msg msgs)
			 tokens
			 resume))))))))
