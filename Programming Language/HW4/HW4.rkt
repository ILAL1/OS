#lang plai

(require (for-syntax racket/base) racket/match racket/list racket/string
         (only-in mzlib/string read-from-string-all))

;; build a regexp that matches restricted character expressions, can use only
;; {}s for lists, and limited strings that use '...' (normal racket escapes
;; like \n, and '' for a single ')
(define good-char "(?:[ \t\r\na-zA-Z0-9_{}!?*/<=>:+-]|[.][.][.]|[@$%&~^])")

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

;FunDef abstract syntax trees
(define-type FunDef
  (fundef (fun-name symbol?) (arg-name (listof symbol?)) (body FnWAE?)))

;FnWAE-Value abstract syntax trees
(define-type FnWAE-Value
  (numV (n number?))
  (recV (r rec?)))

;RecPair abstract syntax trees
(define-type RecPair
  (recPair (name symbol?)(expr FnWAE?)))

;FnWAE abstract syntax trees
(define-type FnWAE
  (num (n number?))
  (add (lhs FnWAE?) (rhs FnWAE?))
  (sub (lhs FnWAE?) (rhs FnWAE?))
  (with (name symbol?) (named-expr FnWAE?) (body FnWAE?))
  (id (name symbol?))
  (rec (arg (listof RecPair?)))
  (get (r FnWAE?) (name symbol?))
  (app (ftn symbol?) (arg (listof FnWAE?))))

;uniq?: list-of-symbol -> bool
;to check list-of-volume has duplicates or not 
;If it is, then return false 
;If it's not, return true
(define (uniq? x) 
  (cond ((not (list? x)) (error 'uniq? "bad syntax: ~a" x))
        ((not (listof symbol?)) (error 'uniq? "bad syntax: ~a" x))
        ((empty? x) #t)
        (else (cond ((andmap (lambda (list) (not (symbol=? (first x) list))) (rest x)) (uniq? (rest x)))
                    (else #f)))))

;parse: sexp -> FnWAE
;to convert s-expressions into FnWAEs
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list x i) b) (with x (parse i) (parse b))]
    [(? symbol?) (id sexp)] 
    [(list 'rec a ...) (cond ((not (andmap (lambda (e) (list? e)) a)) (error 'parse "bad syntax: ~a" sexp))
                             ((not (andmap (lambda (e) (= (length e) 2)) a)) (error 'parse "bad syntax: ~a" sexp))
                             (else (unless (uniq? (map (lambda (e) (first e)) a)) (error "duplicate fields"))
                                   (rec (map (lambda (e) (recPair (first e) (parse (second e)))) a))))]
    [(list 'get r x) (get (parse r) x)]
    [(list '+ a ...) (error 'parse "bad syntax: ~a" sexp)]
    [(list '- a ...) (error 'parse "bad syntax: ~a" sexp)]
    [(list 'with a ...) (error 'parse "bad syntax: ~a" sexp)]
    [(list 'get a ...) (error 'parse "bad syntax: ~a" sexp)]
    [(list f x ...) (app f (map parse x))]
    [else (error 'parse "bad syntax: ~a" sexp)]))

;parse-defn: sexp -> FunDef
;to convert s-expressions into FunDefs
(define (parse-defn sexp)
  (match sexp
    [(list 'deffun (list f x ...) body)
     (unless (uniq? x)
       (error 'parse-defn "bad syntax"))
     (fundef f x (parse body))]))

;parse-str: str -> FnWAE
;parses a string containing a FnWAE expression to a FnWAE AST
(define (parse-str str)
  (parse (string->sexpr str)))

;parse-defn-str: str -> FunDef
;parses a string containing a FunDef expression to a FunDef AST
(define (parse-defn-str str)
  (parse-defn (string->sexpr str)))

;helper-with: symbol list-of-symbol list-of-number -> number
;to get rid of first argument from second argumet and corresponding number from 
;third argument resulting expression 
(define (helper-with x xs vals)
  (cond ((symbol=? x (first xs)) (rest vals))
        (else (append (list (first vals)) (helper-with x xs (rest vals))))))

;helper-id: symbol list-of-symbol list-of-number -> number
;to get corresponding number value in third argument to symbol in second argument 
;which is same with first argument
(define (helper-id s xs vals)
  (cond ((symbol=? s (first xs)) (first vals))
        (else (helper-id s (rest xs) (rest vals)))))

;FnWAE-Value: FnWAE-Value -> FnWAE
;to convert FnWAE-Value to FnWAE
(define (FnWAE-Value->FnWAE fnwae-value)
  (cond ((numV? fnwae-value) (num (numV-n fnwae-value)))
        ((recV? fnwae-value) (recV-r fnwae-value))))

;subst: FnWAE list-of-symbol list-of-FnWAE-Value -> FnWAE
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst fnwae xs vals)
  (type-case FnWAE fnwae
    (num (n) fnwae)
    (add (l r) (add (subst l xs vals) (subst r xs vals)))
    (sub (l r) (sub (subst l xs vals) (subst r xs vals)))
    (with (y i b) (with y
                        (subst i xs vals)
                        (if (member y xs) (subst b (remove y xs) (helper-with y xs vals))
                            (subst b xs vals))))
    (id (s) (if (not (member s xs)) fnwae
                (FnWAE-Value->FnWAE (helper-id s xs vals))))
    (rec (a) (rec (map (lambda (e) (recPair (recPair-name e) (subst (recPair-expr e) xs vals))) a)))
    (get (r x) (get (subst r xs vals) x))
    (app (f a) (app f (map (lambda (e) (subst e xs vals)) a)))))

;lookup-fundef : symbol list-of-FunDef -> FunDef
;to get FunDef in second argument which is same with first argument
(define (lookup-fundef name fundefs)
  (cond
    ((empty? fundefs) (error 'lookup-fundef "unknown function"))
    (else (if (symbol=? name (fundef-fun-name (first fundefs))) (first fundefs)
              (lookup-fundef name (rest fundefs))))))

;num-op: (number number -> number) -> ((FnWAE FnWAE) -> FnWAE)
;take number opertator and make it into FnWAE operator
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))
;num+:FnWAE FnWAE -> FnWAE
;add operator for FnWAE
(define num+ (num-op +))
;num+:FnWAE FnWAE -> FnWAE
;sub operator for FnWAE
(define num- (num-op -))

;interp : FnWAE list-of-FunDef -> FnWAE-Value
;evaluates FnWAE expressions by reducing them to FnWAE-Value
(define (interp fnwae fundefs)
  (type-case FnWAE fnwae
    (num (n) (numV n))
    (add (l r) (num+ (interp l fundefs) (interp r fundefs)))
    (sub (l r) (num- (interp l fundefs) (interp r fundefs)))
    (with (x i b) (interp (subst b (list x) (list (interp i fundefs))) fundefs))
    (rec (a) (recV (rec (map (lambda (e) (recPair (recPair-name e) (FnWAE-Value->FnWAE (interp (recPair-expr e) fundefs)))) a))))
    (get (r x) (local
                 ((define recv (interp r fundefs)))
                 (cond ((empty? (rec-arg (recV-r recv))) (error "no such field"))
                       ((symbol=? x (recPair-name (first (rec-arg (recV-r recv))))) (interp (recPair-expr (first (rec-arg (recV-r recv)))) fundefs))
                       (else (interp (get (rec (rest (rec-arg (recV-r recv)))) x) fundefs)))))
    (id (s) (error 'interp "free variables"))
    (app (f a) (local
                 ((define a-fundef (lookup-fundef f fundefs)))
                 (cond ((= (length a) (length (fundef-arg-name a-fundef))) (interp (subst (fundef-body a-fundef)
                                                                                          (fundef-arg-name a-fundef)
                                                                                          (map (lambda (e) (interp e fundefs)) a)) fundefs))
                       (else (error "wrong arity")))))))

;interp-expr : FnWAE-Value -> number or symbol
;evaluates FnWAE-Value expressions by reducing them to number or symbol
(define (interp-expr fnwae-value)
  (type-case FnWAE-Value fnwae-value
    (numV (n) n)
    (recV (r) 'record)))


;run : string list -> number or symbol
;evaluate a FnWAE program contained in a string and function define list
(define (run str fundefs)
  (interp-expr (interp (parse-str str) fundefs)))

;my tests
(test (run "{f 1 2}" (list (parse-defn '{deffun {f x y} {+ x y}}))) 3)
(test (run "{+ {f} {f}}" (list (parse-defn '{deffun {f} 5}))) 10)
(test/exn (run "{f 1}" (list (parse-defn '{deffun {f x y} {+ x y}})))
          "wrong arity")
(test (run "{good 1 2}" (list (parse-defn '{deffun {good x y} {+ x y}}))) 3)
(test (run "{* 1 2}" (list (parse-defn '{deffun {* x y} {+ x y}}))) 3)
(test/exn (run "f" (list (parse-defn '{deffun {* x y} {+ x y}}))) "interp: free variables")
(test (run "{^ 1 2}" (list (parse-defn '{deffun {^ x y} {+ x y}}))) 3)
(test (run "{= 1 2}" (list (parse-defn '{deffun {= x y} {+ x y}}))) 3)
(test (run "{@ 1 2}" (list (parse-defn '{deffun {@ x y} {+ x y}}))) 3)
(test (run "{$ 1 2}" (list (parse-defn '{deffun {$ x y} {+ x y}}))) 3)
(test (run "{% 1 2}" (list (parse-defn '{deffun {% x y} {+ x y}}))) 3)
(test (run "{~ 1 2}" (list (parse-defn '{deffun {~ x y} {+ x y}}))) 3)
(test (run "{~12 1 2}" (list (parse-defn '{deffun {~12 x y} {+ x y}}))) 3)
(test (run "{~12b 1 2}" (list (parse-defn '{deffun {~12b x y} {+ x y}}))) 3)
(test (run "{- 1 2}" (list (parse-defn '{deffun {- x y} {+ x y}}))) -1)
(test (run "{+ 1 2}" (list (parse-defn '{deffun {+ x y} {- x y}}))) 3)
(test/exn (run "{with 1 2}" (list (parse-defn '{deffun {with x y} {+ x y}}))) "parse: bad syntax: (with 1 2)")
(test/exn (run "{rec 1 2}" (list (parse-defn '{deffun {rec x y} {+ x y}}))) "parse: bad syntax: (rec 1 2)")
(test/exn (run "{rec {x 1} {y 1 2}}" empty) "parse: bad syntax: (rec (x 1) (y 1 2))")
(test/exn (run "{with x}" (list (parse-defn '{deffun {with x} 1}))) "parse: bad syntax: (with x)")
(test/exn (run "{+ 1 2 3}" (list (parse-defn '{deffun {+ x y z} {+ {+ x y} z}}))) "parse: bad syntax: (+ 1 2 3)")
(test/exn (run "{- 1 2 3}" (list (parse-defn '{deffun {- x y z} {- {- x y} z}}))) "parse: bad syntax: (- 1 2 3)")
(test/exn (run "{ab 1 2}" (list (parse-defn '{deffun {abc x y} {- x y}}))) "lookup-fundef: unknown function")
(test (run "{rec {a 1} {b 2}}" empty) 'record)
(test (run "{rec {a 1} {b {+ 1 1}}}" empty) 'record)
(test/exn (run "{rec {a 1} {b x}}" empty) "interp: free variables")
(test (run "{rec {a {with {x 1} {+ 1 x}}} {b {+ 1 1}}}" empty) 'record)
(test (run "{rec {a {with {x 1} {+ 1 x}}} {b {+ 1 {with {x 1} {+ 1 x}}}}}" empty) 'record)
(test (run "{rec {a {rec {b 1}}}}" empty) 'record)
(test (run "{rec {a {get {rec {b 1}} b}}}" empty) 'record)
(test/exn (run "{rec {b 1} {b 2}}" empty) "duplicate fields")
(test/exn (run "{get {rec {b 1} {b 2}} b}" empty) "duplicate fields")
(test/exn (run "{get {rec {a 1} {b 2}} c}" empty) "no such field")
(test (run "{get {get {rec {a {rec {b 1}}}} a} b}" empty) 1)
(test (run "{get {rec {a 1} {b {get {rec {c 2}} c}}} b}" empty) 2)
(test (run "{get {rec {a 1} {b {get {rec {c {with {x {get {rec {d 2}} d}} {+ 1 x}}}} c}}} b}" empty) 3)
(test (run "{get {rec {a 1} {b {get {rec {a {with {a {get {rec {a 2}} a}} {+ 1 a}}}} a}}} b}" empty) 3)
(test (run "{+ {get {rec {a 1} {b 2}} a} {with {x {get {rec {a {+ 1 1}}} a}} {- x x}}}" empty) 1)
(test (run "{f 1}" (list (parse-defn '{deffun {f x} x}))) 1)
(test/exn (run "{f x}" (list (parse-defn '{deffun {f x} x}))) "interp: free variables")
(test (run "{f 2}" (list (parse-defn '{deffun {f x} 1}))) 1)
(test (run "{f}" (list (parse-defn '{deffun {f} 1}))) 1)
(test (run "{g {f}}" (list (parse-defn '{deffun {f} 1})(parse-defn '{deffun {g x} 2}))) 2)
(test/exn (run "{g {f}}" (list (parse-defn '{deffun {f} 1})(parse-defn '{deffun {g x} {rec 1}}))) "parse: bad syntax: (rec 1)")
(test/exn (run "{g {f}}" (list (parse-defn '{deffun {f} 1})(parse-defn '{deffun {h x} 2}))) "lookup-fundef: unknown function")
(test (run "{h {f} {g 1}}" (list (parse-defn '{deffun {f} 1})(parse-defn '{deffun {g x} 2})(parse-defn '{deffun {h x y} 3}))) 3)
(test (run "{h {f} {g 1}}" (list (parse-defn '{deffun {f} 1})(parse-defn '{deffun {g x} 2})(parse-defn '{deffun {h x y} {f}}))) 1)
(test (run "{h {f} {g 1}}" (list (parse-defn '{deffun {f} {g 1}})(parse-defn '{deffun {g x} 2})(parse-defn '{deffun {h x y} {f}}))) 2)
(test (run "{with {h {h {f} {g 1}}} {h h h}}" (list (parse-defn '{deffun {f} {g 1}})(parse-defn '{deffun {g x} 2})(parse-defn '{deffun {h x y} {f}}))) 2)
(test/exn (run "{+ {h {f} {g 1}} {h 1}}" (list (parse-defn '{deffun {f} 1})(parse-defn '{deffun {g x} 2})(parse-defn '{deffun {h x y} 3}))) "wrong arity")
(test/exn (run "{+ {h {f} {g 1}} {h 1}}" (list (parse-defn '{deffun {f} 1})(parse-defn '{deffun {g x} 2})(parse-defn '{deffun {h x y} 3}))) "wrong arity")
(test (run "{+ {h {f} {g 1}} {h 1 2}}" (list (parse-defn '{deffun {f} 1})(parse-defn '{deffun {g x} 2})(parse-defn '{deffun {h x y} 3}))) 6)
(test (run "{+ {g {h {f} {g 1}}} {h 1 2}}" (list (parse-defn '{deffun {f} 1})(parse-defn '{deffun {g x} {with {x {get{rec {a 1} {b 2}} a}} {get {rec {x x}} x}}})(parse-defn '{deffun {h x y} 3}))) 4)
(test (run "{+ {g {h {with {h {get {rec {h 1}} h}} {h h h}} {g 1}}} {h 1 2}}" (list (parse-defn '{deffun {f} 1})(parse-defn '{deffun {g x} {with {x {get{rec {a 1} {b 2}} a}} {get {rec {x x}} x}}})(parse-defn '{deffun {h x y} 3}))) 4)
(test (run "{get {r 1} a}" (list (parse-defn '{deffun {r y} {rec {a y}}}))) 1)
(test (run "{get {get {rec {a {rec {b {f 1}}}}} a} b}" (list (parse-defn '{deffun {f a} {rec {a a}}}))) 'record)
(test (run "{get {get {get {rec {a {rec {b {f 1}}}}} a} b} a}" (list (parse-defn '{deffun {f a} {rec {a a}}}))) 1)
(test (run "{get {rec {a {f 1 1 2}} {b {get {rec {c 2}} c}}} a}" (list (parse-defn '{deffun {f a b c} {- {+ a b} c}}))) 0)
(test (run "{get {rec {a 1} {b {get {rec {c {with {x {get {rec {d 2}} d}} {+ 1 x}}}} c}}} b}" empty) 3)
(test (run "{get {rec {a 1} {b {get {rec {a {with {a {get {rec {a 2}} a}} {+ 1 a}}}} a}}} b}" empty) 3)
(test (run "{+ {get {rec {a 1} {b 2}} a} {with {x {get {rec {a {+ 1 1}}} a}} {- x x}}}" empty) 1)
(test (run "{g {g {r 1 {r 2 3}}}}" (list (parse-defn '{deffun {r x y} {rec {a x} {b y}}}) (parse-defn '{deffun {g x} {get x b}}))) 3)
(test (run "{get {f 1 2 3 4 5} a}" (list (parse-defn '{deffun {f l m n o p} {rec {a m} {b l} {c l} {d l} {e l}}}))) 2)
(test (run "{get {f 1 2 3 4 5} a}" (list (parse-defn '{deffun {f l m n o p} {rec {a l} {b m} {c n} {d o} {e p}}}))) 1)
(test (run "{get {f 1 2 3 4 5} a}" (list (parse-defn '{deffun {f a b c d e} {rec {a a} {b b} {c c} {d d} {e e}}}))) 1)
(test (run "{get {rec {a 1} {b {g {r {with {a {g {r 2}}} {+ 1 a}}}}}} b}" (list (parse-defn '{deffun {g x} {get x a}}) (parse-defn '{deffun {r x} {rec {a x}}}))) 3)
(test (run "{+ {get {rec {a 1} {b 2}} a} {with {x {get {r 1} a}} {- x x}}}" (list (parse-defn '{deffun {r x} {rec {a x}}}))) 1)
(test (run "{+ {get {r-2 1 2} a} {with {x {get {r-1 1} a}} {- x x}}}" (list (parse-defn '{deffun {r-1 x} {rec {a x}}}) (parse-defn '{deffun {r-2 x y} {rec {a x} {b y}}}))) 1)

;given tests
(test (run "{rec {a 10} {b {+ 1 2}}}" empty)
      'record)
(test (run "{get {rec {a 10} {b {+ 1 2}}} b}" empty)
      3)
(test/exn (run "{get {rec {b 10} {b {+ 1 2}}} b}" empty)
          "duplicate fields")
(test/exn (run "{get {rec {a 10}} b}" empty)
          "no such field")
(test (run "{g {rec {a 0} {c 12} {b 7}}}"
           (list (parse-defn '{deffun {g r} {get r c}})))
      12)
(test (run "{get {rec {r {rec {z 0}}}} r}" empty)
      'record)
(test (run "{get {get {rec {r {rec {z 0}}}} r} z}" empty)
      0)
(test/exn (run "{rec {z {get {rec {z 0}} y}}}" empty)
         "no such field")
(test (run "{with {x {f 2 5}} {g x}}" (list (parse-defn '{deffun {f a b} {+ a b}}) (parse-defn '{deffun {g x} {- x 5}}))) 2)
(test (run "{f 1 2}" (list (parse-defn '{deffun {f x y} {+ x y}}))) 3)
(test (run "{+ {f} {f}}" (list (parse-defn '{deffun {f} 5}))) 10)
(test (run "{h 1 4 5 6}" (list (parse-defn '{deffun {h x y z w} {+ x w}}) (parse-defn '{deffun {g x y z w} {+ y z}}))) 7)
(test (run "{with {x 10} {- {+ x {f}} {g 4}}}" (list (parse-defn '{deffun {f} 4}) (parse-defn '{deffun {g x} {+ x x}}))) 6)
(test (run "{rec {a 10} {b {+ 1 2}}}" empty) 'record)
(test (run "{get {rec {a 10} {b {+ 1 2}}} b}" empty) 3)
(test (run "{g {rec {a 0} {c 12} {b 7}}}" (list (parse-defn '{deffun {g r} {get r c}}))) 12)
(test (run "{get {rec {r {rec {z 0}}}} r}" empty) 'record)
(test (run "{get {get {rec {r {rec {z 0}}}} r} z}" empty) 0)
(test (run "{with {x 3} {with {y 5} {get {rec {a x} {b y}} a}}}" empty) 3)
(test (run "{with {x {f {rec {a 10} {b 5}} 2}} {g x}}" (list (parse-defn '{deffun {f a b} {+ {get a a} b}}) (parse-defn '{deffun {g x} {+ 5 x}}))) 17)
(test (run "{get {f 1 2 3 4 5} c}" (list (parse-defn '{deffun {f a b c d e} {rec {a a} {b b} {c c} {d d} {e e}}}))) 3)
(test (run "{get {f 1 2 3} b}" (list (parse-defn '{deffun {f a b c} {rec {a a} {b b} {c c}}}))) 2)
(test (run "{get {f 1 2 3} y}" (list (parse-defn '{deffun {f a b c} {rec {x a} {y b} {z c} {d 2} {e 3}}}))) 2)
(test (run "{get {f 1 2 3} d}" (list (parse-defn '{deffun {f a b c} {rec {x a} {y b} {z c} {d 2} {e 3}}}))) 2)
(test (run "{f {get {get {rec {a {rec {a 10} {b {- 5 2}}}} {b {get {rec {x 50}} x}}} a} b}}" (list (parse-defn '{deffun {f x} {+ 5 x}}))) 8)
(test (run "{get {rec {a 10} {b {+ 1 2}}} b}" empty) 3)
(test (run "{g {rec {a 0} {c 12} {b 7}}}" (list (parse-defn '{deffun {g r} {get r c}}))) 12)
(test (run "{get {rec {r {rec {z 0}}}} r}" empty) 'record)
(test (run "{get {get {rec {r {rec {z 0}}}} r} z}" empty) 0)
(test (run "{rec {a 10}}" empty) `record)
(test (run "{get {rec {a 10}} a}" empty) 10)
(test (run "{get {rec {a {+ 1 2}}} a}" empty) 3)
(test (run "{rec }" empty) `record)
(test (run "{get {rec {a {rec {b 10}}}} a}" empty) `record)
(test (run "{get {get {rec {a {rec {a 10}}}} a} a}" empty) 10)
(test (run "{get {get {rec {a {rec {a 10} {b 20}}}} a} a}" empty) 10)
(test (run "{get {get {rec {a {rec {a 10} {b 20}}}} a} b}" empty) 20)
(test (run "{+ {get {rec {a 10}} a} {get {rec {a 20}} a}}" empty) 30)
(test (run "{+ {get {rec {a 10}} a} {get {rec {a 20}} a}}" empty) 30)
(test (run "{rec {a 10}}" empty) `record)
(test (run "{rec {a {- 2 1}}}" empty) `record)
(test (run "{get {rec {a 10}} a}" empty) 10)
(test (run "{get {rec {a {- 2 1}}} a}" empty) 1)
(test (run "{get {rec {a {rec {b 10}}}} a}" empty) `record)
(test (run "{get {get {rec {a {rec {a 10}}}} a} a}" empty) 10)
(test (run "{get {get {rec {a {rec {a 10} {b 20}}}} a} a}" empty) 10)
(test (run "{get {get {rec {a {rec {a 10} {b 20}}}} a} b}" empty) 20)
(test (run "{rec {a 10} {b {+ 1 2}}}" empty) 'record)
(test (run "{get {rec {a 10} {b {+ 1 2}}} b}" empty) 3)
(test (run "{get {rec {r {rec {z 0}}}} r}" empty) 'record)
(test (run "{get {get {rec {r {rec {z 0}}}} r} z}" empty) 0)
(test (run "{rec {a 10} {b {+ 1 2}}}" empty) 'record)
(test (run "{get {rec {a 10} {b {+ 1 2}}} b}" empty) 3)
(test (run "{g {rec {a 0} {c 12} {b 7}}}" (list (parse-defn '{deffun {g r} {get r c}}))) 12)
(test (run "{get {rec {r {rec {z 0}}}} r}" empty) 'record)
(test (run "{get {get {rec {r {rec {z 0}}}} r} z}" empty) 0)
(test (run "{with {y {rec {x 1} {y 2} {z 3}}} {get y y}}" empty) 2)
(test (run "{with {y {rec {x 1} {y 2} {z 3}}} {get y z}}" empty) 3)
(test (run "{rec {a 10} {b {+ 1 2}}}" empty) 'record)
(test (run "{get {rec {a 10} {b {+ 1 2}}} b}" empty) 3)
(test (run "{g {rec {a 0} {c 12} {b 7}}}" (list (parse-defn '{deffun {g r} {get r c}}))) 12)
(test (run "{get {rec {r {rec {z 0}}}} r}" empty) 'record)
(test (run "{get {get {rec {r {rec {z 0}}}} r} z}" empty) 0)