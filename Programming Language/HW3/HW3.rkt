#lang plai

(require (for-syntax racket/base) racket/match racket/list racket/string
         (only-in mzlib/string read-from-string-all))

;; build a regexp that matches restricted character expressions, can use only
;; {}s for lists, and limited strings that use '...' (normal racket escapes
;; like \n, and '' for a single ')
(define good-char "(?:[ \t\r\na-zA-Z0-9_{}!?*/<=>:+-]|[.][.][.])")

;; this would make it awkward for students to use \" for strings
;; (define good-string "\"[^\"\\]*(?:\\\\.[^\"\\]*)*\"")
(define good-string "[^\"\\']*(?:''[^\"\\']*)*")
(define expr-re
  (regexp (string-append "^"
                         good-char"*"
                         "(?:'"good-string"'"good-char"*)*"
                         "$")))
(define string-re
  (regexp (string-append "'("good-string")'")))

(define (string->sexpr str)
  (unless (string? str)
    (error 'string->sexpr "expects argument of type <string>"))
    (unless (regexp-match expr-re str)
      (error 'string->sexpr "syntax error (bad contents)"))
    (let ([sexprs (read-from-string-all
                 (regexp-replace*
                  "''" (regexp-replace* string-re str "\"\\1\"") "'"))])
    (if (= 1 (length sexprs))
      (car sexprs)
      (error 'string->sexpr "bad syntax (multiple expressions)"))))

(test/exn (string->sexpr 1) "expects argument of type <string>")
(test/exn (string->sexpr ".") "syntax error (bad contents)")
(test/exn (string->sexpr "{} {}") "bad syntax (multiple expressions)")

; PWAE abstract syntax trees
(define-type PWAE
  [nums  (nums (listof number?))]
  [add  (left PWAE?) (right PWAE?)]
  [sub  (left PWAE?) (right PWAE?)]
  [with (name symbol?) (init PWAE?) (body PWAE?)]
  [id   (name symbol?)]
  [pooh (opd (listof PWAE?)) (opt symbol?)])

; parse-sexpr: sexpr -> PWAE
; to convert s-expressions into PWAEs
;(parse-sexpr '(with (y 2) (with (x 1) (+ (- (+ (pooh x x +) (pooh y x -)) y) (1 3 5))))) should produce 
;(with 'y (nums '(2)) (with 'x (nums '(1)) (add (sub (add (pooh (list (id 'x) (id 'x)) '+) (pooh (list (id 'y) (id 'x)) '-)) (nums '(1))) (nums '(1 3 5)))))
;(parse-sexpr '(with (y 2) (1 2 3)))
;(parse-sexpr '(with (y 2) (1 2 3))) (with 'y (nums '(2)) (nums '(1 2 3))) should produce (with 'y (nums '(2)) (nums '(1 2 3)))
;(parse-sexpr '(5 6 +))should produce "parse: bad syntax: (5 6 +)"
;(parse-sexpr '(pooh (5 6 +)) should porduce "parse: bad syntax: (pooh (5 6 +))"
;(parse-sexpr '(pooh 5 6 /)) "parse: bad syntax: (pooh (5 6 /))"
;(parse-sexpr '(with (x 5) (+ (x 3 5) (7 1 x)))) "parse: bad syntax: (x 3 5)"
(define (parse-sexpr sexp)
  (match sexp
    [(? number?) (nums (list sexp))]
    [(? (listof number?)) (nums sexp)]
    [(list '+ l r) (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (with x (parse-sexpr i) (parse-sexpr b))]
    [(? symbol?) (id sexp)]
    [(list 'pooh a ... '+) (pooh (map parse-sexpr a) '+)]
    [(list 'pooh a ... '-) (pooh (map parse-sexpr a) '-)]
    [else (error 'parse "bad syntax: ~a" sexp)]))

(test (parse-sexpr '(with (y 2) (with (x 1) (+ (- (+ (pooh x x +) (pooh y x -)) 1) (1 3 5))))) 
 (with 'y (nums '(2)) (with 'x (nums '(1)) (add (sub (add (pooh (list (id 'x) (id 'x)) '+) (pooh (list (id 'y) (id 'x)) '-)) (nums '(1))) (nums '(1 3 5))))))
(test (parse-sexpr '(with (y 2) (1 2 3))) (with 'y (nums '(2)) (nums '(1 2 3))))
(test/exn (parse-sexpr '(5 6 +)) "parse: bad syntax: (5 6 +)")
(test/exn (parse-sexpr '(pooh (5 6 +))) "parse: bad syntax: (pooh (5 6 +))")
(test/exn (parse-sexpr '(pooh 5 6 /)) "parse: bad syntax: (pooh 5 6 /)")
(test/exn (parse-sexpr '(with (x 5) (+ (x 3 5) (7 1 x)))) "parse: bad syntax: (x 3 5)")

;parse: str -> PWAE
;parses a string containing a PWAE expression to a PWAE AST
;(parse "{with {y 2} {with {x 1} {+ {- {+ {pooh x x +} {pooh y x -}} 1} {1 3 5}}}}") should producce
;(with 'y (nums '(2)) (with 'x (nums '(1)) (add (sub (add (pooh (list (id 'x) (id 'x)) '+) (pooh (list (id 'y) (id 'x)) '-)) (nums '(1))) (nums '(1 3 5)))))
;(parse-sexpr "{with {y 2} {1 2 3}}") should produce (with 'y (nums '(2)) (nums '(1 2 3)))
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(test (parse "{with {y 2} {with {x 1} {+ {- {+ {pooh x x +} {pooh y x -}} 1} {1 3 5}}}}") 
 (with 'y (nums '(2)) (with 'x (nums '(1)) (add (sub (add (pooh (list (id 'x) (id 'x)) '+) (pooh (list (id 'y) (id 'x)) '-)) (nums '(1))) (nums '(1 3 5))))))
(test (parse "{with {y 2} {1 2 3}}") (with 'y (nums '(2)) (nums '(1 2 3))))

;subst: PWAE symbol num -> PWAE
;substitutes the second argument with the third argument in the first argument, 
;as per the rules of substitution; the resulting expression contains no free instances 
;of the second argument
;(subst (parse "{with {x 1} {+ {- {+ {pooh x x +} {pooh y x -}} 1} {1 3 5}}}") 'y '(1)) should producce
;(with 'x (nums '(1)) (add (sub (add (pooh (list (id 'x) (id 'x)) '+) 
;(pooh (list (nums '(2)) (id 'x)) '-))(nums '(1)))(nums '(1 3 5))))
(define (subst expr from to)
  (type-case PWAE expr
    [nums (n)   expr]
    [add (l r) (add (subst l from to) (subst r from to))]
    [sub (l r) (sub (subst l from to) (subst r from to))]
    [id (name) (if (symbol=? name from) (nums to) expr)]
    [with (bound-id named-expr bound-body)
          (with bound-id
                (subst named-expr from to)
                (if (symbol=? bound-id from)
                    bound-body
                    (subst bound-body from to)))]
    [pooh (opd opt) (pooh (map (lambda (x) (subst x from to)) opd) opt)]))

(test (subst (parse "{with {x 1} {+ {- {+ {pooh x x +} {pooh y x -}} 1} {1 3 5}}}") 'y '(2))
      (with 'x (nums '(1)) (add (sub (add (pooh (list (id 'x) (id 'x)) '+) 
                                         (pooh (list (nums '(2)) (id 'x)) '-))
                                    (nums '(1)))(nums '(1 3 5)))))
      
;bin-op : (listof number) (listof number) -> (listof number)
;applies a binary numeric function on all combinations of numbers from
;the two input lists or numbers, and return the list of all of the results
;(bin-op + '(1 2 3) '(2 3)) should produce '(3 4 4 5 5 6)
;(bin-op - '(1 2 3) '(2 3)) should produce '(-1 -2 0 -1 1 0)
;(bin-op - '() '(2 3)) should produce '()
;(bin-op - 1 '(1)) should produce "parse: bad syntax: 1"
;(bin-op - '(1) 1) should produce "parse: bad syntax: 1"
(define (bin-op op ls rs)
  (define (helper l rs)
    ;; f : number -> number
    (define (f x) (op l x))
    (map f rs))
  (cond  ((not (list? ls)) (error 'parse "bad syntax: ~a" ls))
         ((not (list? rs)) (error 'parse "bad syntax: ~a" rs))
         ((null? ls) null)
         (else(append (helper (first ls) rs) (bin-op op (rest ls) rs)))))

(test (bin-op + '(1 2 3) '(2 3)) '(3 4 4 5 5 6))
(test (bin-op - '(1 2 3) '(2 3)) '(-1 -2 0 -1 1 0))
(test (bin-op - '() '(2 3)) '())
(test/exn (bin-op - 1 '(1)) "parse: bad syntax: 1")
(test/exn (bin-op - '(1) 1) "parse: bad syntax: 1")

;eval: PWAE -> listof number
;evaluates PWAE expressions by reducing them to list of numbers
;(eval (parse "{with {y 2} {with {x 1} {+ {- {+ {pooh x x +} {pooh y x -}} 1} {1 3 5}}}}")) should produce '(3 5 7)
(define (eval expr)
  (type-case PWAE expr
    [nums (n) n]
    [add (l r) (bin-op + (eval l) (eval r))]
    [sub (l r) (bin-op - (eval l) (eval r))]
    [with (bound-id named-expr bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval named-expr)))]
    [id (name)(error 'eval "free identifier: ~s" name)]
    [pooh (opd opt) (cond ((= (length opd) 2) (if (symbol=? '+ opt) 
                                                  (bin-op + (eval (first opd)) (eval (second opd)))
                                                  (bin-op - (eval (first opd)) (eval (second opd)))))
                          (else (if (symbol=? '+ opt) 
                                          (bin-op + (eval (pooh (drop-right opd 1) opt)) (eval (last opd)))
                                          (bin-op - (eval (pooh (drop-right opd 1) opt)) (eval (last opd))))))]))

(test (eval (parse "{with {y 2} {with {x 1} {+ {- {+ {pooh x x +} {pooh y x -}} 1} {1 3 5}}}}")) '(3 5 7))

;run : string -> listof number
;evaluate a PWAE program contained in a string
(define (run str)
  (eval (parse str)))

;my test
(test (run "1") '(1))
(test (run "{1}")'(1))
(test/exn (run "x") "eval: free identifier: x")
(test (run "{+ 1 2}") '(3))
(test (run "{- 2 1}") '(1))
(test (run "{pooh 1 2 +}") '(3))
(test (run "{pooh 2 1 -}") '(1))
(test (run "{pooh 1 2 3 4 +}") '(10))
(test (run "{pooh 1 2 3 4 -}") '(-8))
(test (run "{+ {1 2 3} 1}") '(2 3 4))
(test (run "{- 1 {1 2 3}}") '(0 -1 -2))
(test (run "{+ {1} {1 2 3}}") '(2 3 4))
(test (run "{- {1 2 3} {1}}") '(0 1 2))
(test (run "{+ {1 2 3} {1 2 3}}") '(2 3 4 3 4 5 4 5 6))
(test (run "{+ {1 2} {1 2 3 4}}") '(2 3 4 5 3 4 5 6))
(test (run "{- {1 2 3} {1 2 3}}") '(0 -1 -2 1 0 -1 2 1 0))
(test (run "{- {1 2 3 4} {1 2}}") '(0 -1 1 0 2 1 3 2))
(test (run "{- {+ 1 {1 2 3}} {1 4}}") '(1 -2 2 -1 3 0))
(test/exn (run "{- {1 2 3 4} {1 2} {1 2}}") "parse: bad syntax: (- (1 2 3 4) (1 2) (1 2))")
(test/exn (run "{+ {1 2 3 4} {1 2} {1 2}}") "parse: bad syntax: (+ (1 2 3 4) (1 2) (1 2))")
(test (run "{pooh {1 2} {1 2} +}") '(2 3 3 4))
(test (run "{pooh {1} {1 2} {1 2 3} {1 2 3 4} +}") '(4 5 6 7 5 6 7 8 6 7 8 9 5 6 7 8 6 7 8 9 7 8 9 10))
(test (run "{pooh {1 2} {1 2} -}") '(0 -1 1 0))
(test (run "{pooh {1} {1 2} {1 2 3} {1 2 3 4} -}") '(-2 -3 -4 -5 -3 -4 -5 -6 -4 -5 -6 -7 -3 -4 -5 -6 -4 -5 -6 -7 -5 -6 -7 -8))
(test (run "{pooh {1 2} {pooh 1 {pooh {1 2} 1 +} 3 -} {1 2} +}") '(-2 -1 -3 -2 -1 0 -2 -1))
(test (run "{pooh {1 2} {+ {2 3} {pooh {1 2} 1 -}} {pooh 1 2 3 +} +}") '(9 10 10 11 10 11 11 12))
(test (run "{with {x 1} {+ 1 2}}") '(3))
(test (run "{with {x 1} {+ 1 x}}") '(2))
(test (run "{with {x 1} {- 1 x}}") '(0))
(test (run "{with {x 1} {pooh 1 x +}}") '(2))
(test (run "{with {x {+ 1 2}} {pooh 1 x +}}") '(4))
(test (run "{with {x {pooh 1 2 +}} {pooh 1 x +}}") '(4))
(test (run "{with {x 1} {+ x {1 2 3}}}") '(2 3 4))
(test (run "{with {x {+ {1 2} {1 2}}} {+ x 1}}") '(3 4 4 5))
(test (run "{with {x {pooh {1 2} {1 2} -}} {+ x 1}}") '(1 0 2 1))
(test (run "{with {x {pooh {1 2} {1 2} +}} {pooh {1 2} x -}}") '(-1 -2 -2 -3 0 -1 -1 -2))
(test (run "{with {x {pooh {1 2} {1 2} +}} {+ x {pooh {1 2} x -}}}") '(1 0 0 -1 2 1 1 0 2 1 1 0 3 2 2 1 2 1 1 0 3 2 2 1 3 2 2 1 4 3 3 2))
(test (run "{with {x {+ {pooh {1 2} {1 2} +} {1 2}}} {pooh {1 2} x -}}") '(-2 -3 -3 -4 -3 -4 -4 -5 -1 -2 -2 -3 -2 -3 -3 -4))
(test (run "{with {x {+ {pooh {1 2} {1 2} +} {1 2}}} {- {pooh {1 2} x -} {1 2}}}") '(-3 -4 -4 -5 -4 -5 -5 -6 -4 -5 -5 -6 -5 -6 -6 -7 -2 -3 -3 -4 -3 -4 -4 -5 -3 -4 -4 -5 -4 -5 -5 -6))
(test (run "{with {x {with {y {pooh {+ 1 2} 1 +}} {pooh y 1 +}}} {+ {1 2} {pooh 1 2 +}}}") '(4 5))
(test (run "{with {x {pooh 1 {pooh 1 2 +} +}} {with {y {pooh {+ x 2} {1 2} -}} {+ {pooh y 1 +} x}}}") '(10 9))
(test (run "{pooh {with {x {pooh {1 2} 1 +}} {+ x {pooh x {1 2} -}}} {pooh {with {x 1} {pooh x x -}} 0 +} -}") '(3 2 4 3 4 3 5 4))
(test (run "{pooh {+ 1 {with {x 1} {with {x 2} {pooh x {+ x x} x +}}}} {pooh 1 {1 2} +} -}") '(7 6))
(test (run "{+ {with {x {pooh {1 2} 1 +}} {+ x {pooh x {1 2} -}}} {pooh {with {x 1} {pooh x x -}} 0 +}}") '(3 2 4 3 4 3 5 4))
(test (run "{- {+ 1 {with {x 1} {with {x 2} {pooh x {+ x x} x +}}}} {pooh 1 {1 2} +}}") '(7 6))
(test (run "{pooh {+ 1 {with {x 1} {with {x 2} {pooh x {+ x x} x +}}}} {pooh 1 {1 2} +} {+ 1 {pooh {1 2} 1 +}}-}") '(4 3 3 2))
(test (run "{pooh {+ 1 {with {x 1} {with {x 2} {pooh x {+ x x} x +}}}} {pooh 1 {1 2} +} {with {x 5} {pooh 1 2 3 4 x +}} -}") '(-8 -9))
(test/exn (run "{with {x {+ x 1}} {+ x 1}}") "eval: free identifier: x")
(test (run "{with {x {+ {1} 2}} {+ {pooh {1 2} {1 2 3} +} x}}") '(5 6 7 6 7 8))
(test (run "{with {x {+ {pooh {1 2} {1 5} 1 +} {1 2}}} {with {y 1} {+ {pooh x {1 3} {2 3} -} y}}}") '(2 1 0 -1 3 2 1 0 6 5 4 3 7 6 5 4 3 2 1 0 4 3 2 1 7 6 5 4 8 7 6 5))


;given tests
(test (run "{+ {2 1} {3 4}}") '(5 6 4 5))
(test (run "{+ {- {+ 1 3} 2} {10 -10}}") '(12 -8))
(test (run "{+ 3 7}") '(10))
(test (run "{- 10 {3 5}}") '(7 5))
(test (run "{with {x {+ 5 5}} {+ x x}}") '(20))
(test (run "5") '(5))
(test (run "{+ 5 5}") '(10))
(test (run "{with {x {+ 5 5}} {+ x x}}") '(20))
(test (run "{with {x 5} {+ x x}}") '(10))
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") '(14))
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") '(4))
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") '(15))
(test (run "{with {x 5} {+ x {with {x 3} x}}}") '(8))
(test (run "{with {x 5} {+ x {with {y 3} x}}}") '(10))
(test (run "{with {x 5} {with {y x} y}}") '(5))
(test (run "{with {x 5} {with {x x} x}}") '(5))
(test/exn (run "{with {x 1} y}") "free identifier")
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{pooh 1 2 -}") '(-1))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{with {x {with {x 20} {pooh 1 x +}}} {with {y 10} {pooh x y -}}}") '(11))
(test (run "{with {x {pooh 1 2 3 4 5 +}} x}") '(15))
(test (run "{pooh {with {x {pooh {1 2} {3 4} 1 +}} x} 2 3 -}") '(0 1 1 2))
(test (run "{pooh 1 2 3 4 5 +}") '(15))
(test (run "{pooh {1 2 3} {4 5} -}") '(-3 -4 -2 -3 -1 -2))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{pooh 1 2 3 4 +}") '(10))
(test (run "{pooh {3 4} {-4 0} 5 +}") '(4 8 5 9))
(test (run "{pooh 1 2 3 4 -}") '(-8))
(test (run "{pooh {4 1} 1 {5 6 7} -}") '(-2 -3 -4 -5 -6 -7))
(test (run "{+ {pooh 1 {4 9} -3 {2 0 1} +} {- {pooh {3 4} {2} -} 4}}") '(1 2 -1 0 0 1 6 7 4 5 5 6))
(test (run "{pooh 1 {pooh 1 2 -} 3 +}") '(3))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{pooh {2 1} {3 4} +}") '(5 6 4 5))
(test (run "{with {x {1 2}} {pooh x {+ {1 2} 1} -}}") '(-1 -2 0 -1))
(test (run "{with {x {1 2}} {pooh x {pooh {1 2} 1 +} -}}") '(-1 -2 0 -1))
(test (run "{+ {+ {pooh 1 2 -} 5} {pooh 3 4 -}}") '(3))
(test (run "{pooh {+ {pooh 1 2 -} 5} {- 3 4} +}") '(3))
(test (run "{pooh {pooh {pooh 1 2 -} 5 +} {pooh 3 4 -} +}") '(3))
(test (run "{+ {+ {- 1 2} 5} {- 3 4}}") '(3))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{with {x {with {y {1 -2}} {pooh 1 y 2 -}}} {+ x x}}") '(-4 -1 -1 2))
(test (run "{pooh {0 1} {2 3} {4 5} 6 +}") '(12 13 13 14 13 14 14 15))
(test (run "{with {x {pooh 8 7 -}} {with {x 10} {+ x 3}}}") '(13))
(test (run "{pooh {pooh 1 2 {2 3} {1 2} -} {2 1 3 2} {1 2} +}") '(-1 0 -2 -1 0 1 -1 0 -2 -1 -3 -2 -1 0 -2 -1 -2 -1 -3 -2 -1 0 -2 -1 -3 -2 -4 -3 -2 -1 -3 -2))
(test (run "{with {x {pooh {1 2} {2 3} 1 +}} {pooh x 1 2 +}}") '(7 8 8 9))
(test (run "{with {x {pooh 3 4 -}} {pooh {+ {pooh 1 2 -} 5} x +}}") '(3))
(test (run "{with {x {with {x 20} {pooh 1 x +}}} {with {y 10} {pooh x y -}}}") '(11))
(test (run "{with {x {pooh 1 2 3 4 5 +}} x}") '(15))
(test (run "{pooh {with {x {pooh {1 2} {3 4} 1 +}} x} 2 3 -}") '(0 1 1 2))
(test (run "{pooh {1 2 3} {4 5} -}") '(-3 -4 -2 -3 -1 -2))

;additional tests for complete coverage
(test (run "{with {x 2} {- {+ x x} x}}") '(2))
(test/exn (run "{with x = 2 {+ x 3}}") "bad syntax")
(test/exn (run "{bleh}") "bad syntax")