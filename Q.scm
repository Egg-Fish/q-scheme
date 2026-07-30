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
(load "Token/Backslash.scm")
(load "Token/Arr.scm")
(load "Token/Let.scm")
(load "Token/In.scm")
(load "Token/Eq.scm")
(load "Token/Plus.scm")
(load "Token/Minus.scm")
(load "Token/Star.scm")
(load "Token/LP.scm")
(load "Token/RP.scm")
(load "Token/Ident.scm")
(load "Token/Int.scm")
(load "Token/Bool.scm")

(load "Lexer.scm")

(load "Expr.scm")
(load "Expr/Let.scm")
(load "Expr/Lam.scm")
(load "Expr/Eq.scm")
(load "Expr/Add.scm")
(load "Expr/Sub.scm")
(load "Expr/Mul.scm")
(load "Expr/App.scm")
(load "Expr/Var.scm")
(load "Expr/Int.scm")
(load "Expr/Bool.scm")

(load "Parser.scm")

(load "Type.scm")
(load "Type/Fun.scm")
(load "Type/Int.scm")
(load "Type/Bool.scm")

(load "Typechecker.scm")

(load "Normalizer.scm")


(define (Q.compile filename)
  (let ([tokens (Q.Lexer.lexFile filename)])
    (let ([expr (Q.Parser.parse tokens)])
      (display (string-append "Expr: " (Q.Expr->string expr)))
      (newline)
      (let ([type (Q.Typechecker.typecheck expr)])
	(display (string-append "Type: " (Q.Type->string type)))
	(newline)
	(let ([norm (Q.Normalizer.normalize expr)])
	  (display (string-append "Normalized Expr: " (Q.Expr->string norm)))
	  (newline)
	  norm)))))

(define (Q.lexAndParse filename)
  (Q.Parser.parse (Q.Lexer.lexFile filename)))

(define (Q.typecheck filename)
  (Q.Typechecker.typecheck (Q.lexAndParse filename)))
