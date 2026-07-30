(define (Q.Typechecker.typecheck expr)
  (define (indent level)
    (if (<= level 0)
	#f
	(display " ")))
  
  (define (displayMsg msg level)
    (if (string? (car msg))
	(begin
	  (indent level)
	  (display (car msg))
	  (newline)
	  (if (null? (cdr msg))
	      #f
	      (displayMsg (cadr msg) level)))
	(map (lambda (m)
	       (displayMsg m (+ level 1))
	       (newline))
	     msg)))
    
  (Q.Typechecker.infer (list)
		       expr
		       (lambda (_ type)
			 type)
		       (lambda (msg resume)
			 (displayMsg msg 0)
			 (newline)
			 (resume))))


(define (Q.Typechecker.infer env expr kSuccess kError)
  (cond
   [(Q.Expr.Let? expr)
    (let ([lhs (Q.Expr.Let:lhs expr)]
	  [rhs (Q.Expr.Let:rhs expr)]
	  [body (Q.Expr.Let:body expr)]
	  [kError (lambda (msg resume)
		    (kError (list (string-append "While inferring type of let-expression "
						 (Q.Expr->string expr))
				  msg)
			    resume))])
      (Q.Typechecker.infer
       env
       rhs
       (lambda (env type)
	 (let ([kError (lambda (msg resume)
			 (kError (list (string-append "With variable "
						      (Q.Expr->string lhs)
						      " : "
						      (Q.Type->string type))
				       msg)
				 resume))])
	   (Q.Typechecker.infer
	    (cons (cons lhs type) env)
	    body
	    kSuccess
	    (lambda (msg resume)
	      (kError (list (string-append "While inferring type of body "
					   (Q.Expr->string body))
			    msg)
		      resume)))))
       (lambda (msg resume)
	 (kError (list (string-append "While inferring type of RHS "
				      (Q.Expr->string rhs))
		       msg)
		 resume))))]

   [(Q.Expr.Eq? expr)
    (let ([lhs (Q.Expr.Eq:lhs expr)]
	  [rhs (Q.Expr.Eq:rhs expr)])
      (Q.Typechecker.infer
       env
       lhs
       (lambda (env type)
	 (let ([kError (lambda (msg resume)
			 (kError (list (string-append "With LHS "
						      (Q.Expr->string lhs)
						      " : "
						      (Q.Type->string type))
				       msg)
				 resume))])
	   (Q.Typechecker.check
	    env
	    rhs
	    type
	    (lambda (env _)
	      (kSuccess env type))
	    (lambda (msg resume)
	      (kError (list (string-append "While checking RHS "
					   (Q.Expr->string rhs))
			    msg)
		      (lambda () (kSuccess env type)))))))
       kError))]
			    
   
   [(Q.Expr.Add? expr)
    (Q.Typechecker.check
     env
     (Q.Expr.Add:lhs expr)
     (Q.Type.Int)
     (lambda (env _)
       (Q.Typechecker.check
	env
	(Q.Expr.Add:rhs expr)
	(Q.Type.Int)
	(lambda (env _)
	  (kSuccess env (Q.Type.Int)))
	(lambda (msg resume)
	  (kError (list "While checking RHS"
			msg)
		  (lambda () (kSuccess env (Q.Type.Int)))))))
     (lambda (msg1 _)
       (Q.Typechecker.check
	env
	(Q.Expr.Add:rhs expr)
	(Q.Type.Int)
	(lambda _
	  (kError (list "While checking left operand"
			msg1)
		  (lambda () (kSuccess env (Q.Type.Int)))))
	(lambda (msg2 _)
	  (kError (list (list "While checking LHS"
			      msg1)
			(list "While checking RHS"
			      msg2))
		  (lambda () (kSuccess env (Q.Type.Int))))))))]

   
   [(Q.Expr.Var? expr)
    (let ([type (assoc expr env)]
	  [kError (lambda (msg resume)
		    (kError (list (string-append "While inferring type of "
						 (Q.Expr->string expr))
				  msg)
			    resume))])
      (if type
	  (kSuccess env (cdr type))
	  (kError (list (string-append "Variable "
				       (Q.Expr->string expr)
				       " is not in scope"))
		  (lambda () #f))))]

   
   [(Q.Expr.Int? expr)
    (kSuccess env (Q.Type.Int))]

   
   [(Q.Expr.Bool? expr)
    (kSuccess env (Q.Type.Bool))]
   

   [else
    (kError (list "Could not infer type of expression")
	    (lambda () #f))]))


(define (Q.Typechecker.check env expr expected kSuccess kError)
  (cond
   [(Q.Expr.Var? expr)
    (let ([type (assoc expr env)]
	  [kError (lambda (msg resume)
		    (kError (list (string-append "While checking variable "
						 (Q.Expr->string expr)
						 " : "
						 (Q.Type->string expected))
				  msg)
			    resume))])
      (if type
	  (let ([actual (cdr type)])
	    (if (equal? expected actual)
		(kSuccess env (cdr type))
		(kError (list (string-append "Expected type "
					     (Q.Type->string expected)
					     ", got "
					     (Q.Type->string actual)))
			(lambda () #f))))
	  (kError (list (string-append "Variable "
				       (Q.Expr->string expr)
				       " is not in scope"))
		  (lambda () #f))))]

   
   [else
    (Q.Typechecker.infer env
			 expr
			 (lambda (env actual)
			   (if (equal? expected actual)
			       (kSuccess env actual)
			       (kError (list (string-append "Expected type "
							    (Q.Type->string expected)
							    ", got "
							    (Q.Type->string actual)))
				       (lambda () #f))))
			 kError)]))
    
