(define (Q.curry f . xs)
  (lambda ys
    (apply f (append xs ys))))

(define (Q.fold f acc xs)
  (if (null? xs)
      acc
      (Q.fold f
	      (f acc (car xs))
	      (cdr xs))))

(define (Q.reduce f xs)
  (Q.fold f (car xs) (cdr xs)))

(load "Token.scm")
(load "Token/Arr.scm")
(load "Token/Plus.scm")
(load "Token/Minus.scm")
(load "Token/Star.scm")
(load "Token/LP.scm")
(load "Token/RP.scm")
(load "Token/Ident.scm")
(load "Token/Int.scm")

(load "Lexer.scm")

(load "Expr.scm")
(load "Expr/Add.scm")
(load "Expr/Sub.scm")
(load "Expr/Mul.scm")
(load "Expr/App.scm")
(load "Expr/Var.scm")
(load "Expr/Int.scm")

(load "Parser.scm")
