(define (Q.Lexer.lexFile filename)
  (call-with-input-file filename
    Q.Lexer.lexPort))

(define (Q.Lexer.lexPort port)
  (let loop ([ts (list)])
    (let ([t (Q.Lexer.lexToken port)])
      (if (not t)
	  (reverse ts)
	  (loop (cons t ts))))))

(define (Q.Lexer.lexToken port)
  (let ([c (read-char port)])
    (cond
     [(eof-object? c)
      #f]

     [(char-whitespace? c)
      (Q.Lexer.lexToken port)]

     [(char=? c #\\)
      (Q.Token.Backslash)]

     [(char=? c #\=)
      (Q.Token.Eq)]

     [(char=? c #\+)
      (Q.Token.Plus)]

     [(char=? c #\-)
      (let ([d (peek-char port)])
	(if (char=? d #\>)
	    (begin
	      (read-char port)
	      (Q.Token.Arr))
	    (Q.Token.Minus)))]

     [(char=? c #\*)
      (Q.Token.Star)]

     [(char=? c #\()
      (Q.Token.LP)]

     [(char=? c #\))
      (Q.Token.RP)]

     [(char-alphabetic? c)
      (Q.Lexer.lexIdent c port)]

     [(char-numeric? c)
      (Q.Lexer.lexInt c port)]

     [else
      (error 'Q.Lexer.lexToken
	     "Unknown character"
	     c)])))

(define (Q.Lexer.lexIdent c port)
  (let* ([cs (Q.Lexer.readWhile char-alphabetic? port)]
	 [cs (cons c cs)]
	 [s (list->string cs)]
	 [name s])
    (cond
     [(string=? name "let")
      (Q.Token.Let)]
     
     [(string=? name "in")
      (Q.Token.In)]

     [else
      (Q.Token.Ident name)])))

(define (Q.Lexer.lexInt c port)
  (let* ([cs (Q.Lexer.readWhile char-numeric? port)]
	 [cs (cons c cs)]
	 [s (list->string cs)]
	 [value (string->number s)])
    (if value
	(Q.Token.Int value)
	(error 'Q.Lexer.lexInt
	       "Could not parse as integer"
	       s))))

(define (Q.Lexer.readWhile predicate port)
  (let loop ([cs (list)])
    (let ([c (peek-char port)])
      (if (and (not (eof-object? c))
	       (predicate c))
	  (loop (cons (read-char port) cs))
	  (reverse cs)))))
