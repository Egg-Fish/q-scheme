(define (Q.Expr? v)
  (or (Q.Expr.Add? v)
      (Q.Expr.Sub? v)
      (Q.Expr.App? v)
      (Q.Expr.Var? v)
      (Q.Expr.Int? v)))

(define (Q.Expr?? v)
  (or (Q.Expr.Add?? v)
      (Q.Expr.Sub?? v)
      (Q.Expr.App?? v)
      (Q.Expr.Var?? v)
      (Q.Expr.Int?? v)))

(define (Q.Expr->string e)
  (unless (Q.Expr? e)
    (error 'Q.Expr->string
	   "Not a Q.Expr"
	   e))

  (cond
   [(Q.Expr.Add? e)
    (Q.Expr.Add->string e)]
   [(Q.Expr.Sub? e)
    (Q.Expr.Sub->string e)]
   [(Q.Expr.App? e)
    (Q.Expr.App->string e)]
   [(Q.Expr.Var? e)
    (Q.Expr.Var->string e)]
   [(Q.Expr.Int? e)
    (Q.Expr.Int->string e)]
   [else
    (error 'Q.Expr->string
	   "Exhausted"
	   e)]))
