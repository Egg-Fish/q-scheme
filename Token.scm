(define (Q.Token? v)
  (or (Q.Token.Backslash? v)
      (Q.Token.Arr? v)
      (Q.Token.Eq? v)
      (Q.Token.Plus? v)
      (Q.Token.Minus? v)
      (Q.Token.Star? v)
      (Q.Token.LP? v)
      (Q.Token.RP? v)
      (Q.Token.Ident? v)
      (Q.Token.Int? v)))

(define (Q.Token?? v)
  (or (Q.Token.Backslash?? v)
      (Q.Token.Arr?? v)
      (Q.Token.Eq?? v)
      (Q.Token.Plus?? v)
      (Q.Token.Minus?? v)
      (Q.Token.Star?? v)
      (Q.Token.LP?? v)
      (Q.Token.RP?? v)
      (Q.Token.Ident?? v)
      (Q.Token.Int?? v)))

(define (Q.Token->string t)
  (unless (Q.Token? t)
    (error 'Q.Token->string
	   "Not a Q.Token"
	   t))
  (cond
   [(Q.Token.Backslash? t)
    (Q.Token.Backslash->string t)]
   [(Q.Token.Arr? t)
    (Q.Token.Arr->string t)]
   [(Q.Token.Eq? t)
    (Q.Token.Eq->string t)]
   [(Q.Token.Plus? t)
    (Q.Token.Plus->string t)]
   [(Q.Token.Minus? t)
    (Q.Token.Minus->string t)]
   [(Q.Token.Star? t)
    (Q.Token.Star->string t)]
   [(Q.Token.LP? t)
    (Q.Token.LP->string t)]
   [(Q.Token.RP? t)
    (Q.Token.RP->string t)]
   [(Q.Token.Ident? t)
    (Q.Token.Ident->string t)]
   [(Q.Token.Int? t)
    (Q.Token.Int->string t)]
   [else
    (error 'Q.Token->string
	   "Exhausted"
	   t)]))
