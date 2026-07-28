(define Q.Normalizer.freshVar
  (let ([state 0])
    (lambda ()
      (set! state (+ state 1))
      (Q.Expr.Var (string-append "t"
				 (number->string state))))))

(define (Q.Normalizer.normalize expr)
  (Q.Normalizer.normalizeExpr expr (lambda (k t) (k t))))

(define (Q.Normalizer.normalizeExpr expr k)
  (cond
   [(Q.Expr.Let? expr)
    (Q.Normalizer.normalizeExpr
     (Q.Expr.Let:rhs expr)
     (lambda (k1 t1)
       (Q.Normalizer.normalizeExpr
	(Q.Expr.Let:body expr)
	(lambda (k2 t2)
	  (k (lambda (e) (k1 (Q.Expr.Let (Q.Expr.Let:lhs expr)
					 t1
					 (k2 e))))
	     t2)))))]

   [(Q.Expr.Lam? expr)
    (let ([t (Q.Normalizer.freshVar)])
      (Q.Normalizer.normalizeExpr
       (Q.Expr.Lam:body expr)
       (lambda (k1 t1)
	 (k (lambda (e) (Q.Expr.Let t (Q.Expr.Lam (Q.Expr.Lam:param expr) (k1 t1)) e))
	    t))))]

   [(Q.Expr.Eq? expr)
    (Q.Normalizer.normalizeExpr
     (Q.Expr.Eq:lhs expr)
     (lambda (k1 t1)
       (Q.Normalizer.normalizeExpr
	(Q.Expr.Eq:rhs expr)
	(lambda (k2 t2)
	  (let ([t (Q.Normalizer.freshVar)])
	    (k (lambda (e) (k1 (k2 (Q.Expr.Let t (Q.Expr.Eq t1 t2) e))))
	       t))))))]

   [(Q.Expr.Add? expr)
    (Q.Normalizer.normalizeExpr
     (Q.Expr.Add:lhs expr)
     (lambda (k1 t1)
       (Q.Normalizer.normalizeExpr
	(Q.Expr.Add:rhs expr)
	(lambda (k2 t2)
	  (let ([t (Q.Normalizer.freshVar)])
	    (k (lambda (e) (k1 (k2 (Q.Expr.Let t (Q.Expr.Add t1 t2) e))))
	       t))))))]

   [(Q.Expr.Sub? expr)
    (Q.Normalizer.normalizeExpr
     (Q.Expr.Sub:lhs expr)
     (lambda (k1 t1)
       (Q.Normalizer.normalizeExpr
	(Q.Expr.Sub:rhs expr)
	(lambda (k2 t2)
	  (let ([t (Q.Normalizer.freshVar)])
	    (k (lambda (e) (k1 (k2 (Q.Expr.Let t (Q.Expr.Sub t1 t2) e))))
	       t))))))]

   [(Q.Expr.Mul? expr)
    (Q.Normalizer.normalizeExpr
     (Q.Expr.Mul:lhs expr)
     (lambda (k1 t1)
       (Q.Normalizer.normalizeExpr
	(Q.Expr.Mul:rhs expr)
	(lambda (k2 t2)
	  (let ([t (Q.Normalizer.freshVar)])
	    (k (lambda (e) (k1 (k2 (Q.Expr.Let t (Q.Expr.Mul t1 t2) e))))
	       t))))))]

   [(Q.Expr.App? expr)
    (Q.Normalizer.normalizeExpr
     (Q.Expr.App:lhs expr)
     (lambda (k1 t1)
       (Q.Normalizer.normalizeExpr
	(Q.Expr.App:rhs expr)
	(lambda (k2 t2)
	  (let ([t (Q.Normalizer.freshVar)])
	    (k (lambda (e) (k1 (k2 (Q.Expr.Let t (Q.Expr.App t1 t2) e))))
	       t))))))]

   [(Q.Expr.Var? expr)
    (k (lambda (e) e)
       expr)]

   [(Q.Expr.Int? expr)
    (let ([t (Q.Normalizer.freshVar)])
      (k (lambda (e) (Q.Expr.Let t expr e))
	 t))]

   [(Q.Expr.Bool? expr)
    (let ([t (Q.Normalizer.freshVar)])
      (k (lambda (e) (Q.Expr.Let t expr e))
	 t))]

   [else
    (error 'Q.Normalizer.normalizeExpr
	   "Exhausted"
	   expr)]))
