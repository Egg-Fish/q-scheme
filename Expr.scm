(define (Q.Expr? v)
  (or (Q.Expr.Let? v)
      (Q.Expr.Lam? v)
      (Q.Expr.Eq? v)
      (Q.Expr.Add? v)
      (Q.Expr.Sub? v)
      (Q.Expr.Mul? v)
      (Q.Expr.App? v)
      (Q.Expr.Var? v)
      (Q.Expr.Int? v)
      (Q.Expr.Bool? v)))

(define (Q.Expr?? v)
  (or (Q.Expr.Let?? v)
      (Q.Expr.Lam?? v)
      (Q.Expr.Eq?? v)
      (Q.Expr.Add?? v)
      (Q.Expr.Sub?? v)
      (Q.Expr.Mul?? v)
      (Q.Expr.App?? v)
      (Q.Expr.Var?? v)
      (Q.Expr.Int?? v)
      (Q.Expr.Bool?? v)))

(define (Q.Expr->string e)
  (unless (Q.Expr? e)
    (error 'Q.Expr->string
	   "Not a Q.Expr"
	   e))

  (cond
   [(Q.Expr.Let? e)
    (Q.Expr.Let->string e)]
   [(Q.Expr.Lam? e)
    (Q.Expr.Lam->string e)]
   [(Q.Expr.Eq? e)
    (Q.Expr.Eq->string e)]
   [(Q.Expr.Add? e)
    (Q.Expr.Add->string e)]
   [(Q.Expr.Sub? e)
    (Q.Expr.Sub->string e)]
   [(Q.Expr.Mul? e)
    (Q.Expr.Mul->string e)]
   [(Q.Expr.App? e)
    (Q.Expr.App->string e)]
   [(Q.Expr.Var? e)
    (Q.Expr.Var->string e)]
   [(Q.Expr.Int? e)
    (Q.Expr.Int->string e)]
   [(Q.Expr.Bool? e)
    (Q.Expr.Bool->string e)]
   [else
    (error 'Q.Expr->string
	   "Exhausted"
	   e)]))
