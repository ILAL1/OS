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

(define-type KCFAE
  [num (n number?)]
  [add (lhs KCFAE?)
       (rhs KCFAE?)]
  [sub (lhs KCFAE?)
       (rhs KCFAE?)]
  [id (name symbol?)]
  [fun (params (listof symbol?))
       (body KCFAE?)]
  [app (fun-expr KCFAE?)
       (arg-exprs (listof KCFAE?))]
  [if0 (test KCFAE?)
       (then KCFAE?)
       (else KCFAE?)]
  [withcc (name symbol?)
          (body KCFAE?)]
  [try-catch (try KCFAE?)
             (catch KCFAE?)]
  [throw])

(define-type KCFAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body KCFAE?)
            (sc DefrdSub?)]
  [contV (proc procedure?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value KCFAE-Value?)
        (rest DefrdSub?)])

;; ----------------------------------------

;; parse : S-expr -> KCFAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'fun a ...) (fun (first a) (parse (second a)))]
    [(list 'if0 a ...) (if0 (parse (first a))
                           (parse (second a))
                           (parse (third a)))]
    [(list 'withcc a ...) (withcc (first a) (parse (second a)))]
    [(list 'try t 'catch c) (try-catch (parse t) (parse c))]
    [(list 'throw) (throw)]
    [(list a b ...) (app (parse a) (map parse b))]
    [else (error 'parse "bad syntax ~a" sexp)]))
   

(test (parse 3) (num 3))
(test (parse 'x) (id 'x))
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{- 1 2}) (sub (num 1) (num 2)))
(test (parse '{fun {x} x}) (fun '(x) (id 'x)))
(test (parse '{1 2}) (app (num 1) (list (num 2))))
(test (parse '{if0 0 1 2}) (if0 (num 0) (num 1) (num 2)))
(test (parse '{withcc x 2}) (withcc 'x (num 2)))

;; ----------------------------------------

;; interp : KCFAE DefrdSub (KCFAE-Value -> alpha) -> alpha
(define (interp a-fae ds k)
  (type-case KCFAE a-fae
    [num (n) (k (numV n))]
    [add (l r) (interp l ds
                       (lambda (v1)
                         (interp r ds
                                 (lambda (v2)
                                   (k (num+ v1 v2))))))]
    [sub (l r) (interp l ds
                       (lambda (v1)
                         (interp r ds
                                 (lambda (v2)
                                   (k (num- v1 v2))))))]
    [id (name) (k (lookup name ds))]
    [fun (params body-expr)
         (k (closureV params body-expr ds))]
    [app (fun-expr arg-exprs)
         (interp fun-expr ds
                 (lambda (fun-val)
                   (if (= (length arg-exprs) (type-case KCFAE-Value fun-val
                                               [closureV (params body ds) (length params)]
                                               [contV (k) 1]
                                               [else (error 'interp "not a function")]))
                       (interp-multiple arg-exprs ds
                                        (lambda (arg-vals)
                                          (type-case KCFAE-Value fun-val
                                            [closureV (params body fun-ds)
                                                      (interp body
                                                              (if (not (find 'catcher ds)) (ds-helper params arg-vals fun-ds)
                                                                  (aSub 'catcher (lookup 'catcher ds) (ds-helper params arg-vals fun-ds)))
                                                              k)]
                                            [contV (k)
                                                   (k (first arg-vals))]
                                            [else (error 'interp "not a function")])))
                       (error 'interp "wrong input number"))))]
    [if0 (test-expr then-expr else-expr)
         (interp test-expr ds
                 (lambda (v)
                   (if (numzero? v)
                       (interp then-expr ds k)
                       (interp else-expr ds k))))]
    [withcc (id body-expr)
            (interp body-expr 
                    (aSub id
                          (contV k)
                          ds)
                    k)]
    [try-catch (try catch) (interp try (aSub 'catcher (contV (lambda () (interp catch ds k))) ds) k)]
    [throw () (interp (id 'catcher) ds (lambda (catcher) ((contV-proc catcher) )))]))

;;interp-multiple : (listof KCFAE) DefrdSub (KCFAE-Value -> alpha) alpha
(define (interp-multiple arg-expr ds k)
  (if (empty? arg-expr) (k empty)
      (interp (first arg-expr) ds
              (lambda (arg-val)
                (interp-multiple (rest arg-expr) ds 
                                 (lambda (arg-vals) (k (append (list arg-val) arg-vals))))))))

;;ds-helper : (listof symbol) (listof KCFAE-Value) DefrdSub -> DefrdSub
(define (ds-helper params arg-vals ds)
  (if (empty? params) ds
      (ds-helper (rest params) (rest arg-vals) (aSub (first params) (first arg-vals) ds))))

;; num-op : (number number -> number) -> (KCFAE-Value KCFAE-Value -> KCFAE-Value)
(define (num-op op op-name)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op + '+))
(define num- (num-op - '-))

;; numzero? : KCFAE-Value -> boolean
(define (numzero? x)
  (zero? (numV-n x)))

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free variable")]
    [aSub (sub-name num rest-sc)
          (if (symbol=? sub-name name)
              num
              (lookup name rest-sc))]))

;(test/exn (lookup 'x (mtSub)) "free variable")
;(test (lookup 'x (aSub 'x (numV 9) (mtSub))) (numV 9))
;(test (lookup 'x (aSub 'y (numV 10) (aSub 'x (numV 9) (mtSub)))) (numV 9))

(define (find name ds)
  (type-case DefrdSub ds
    [mtSub () #f]
    [aSub (sub-name num rest-sc)
          (if (symbol=? sub-name name)
              #t
              (find name rest-sc))]))

;; interp-expr : KCFAE -> KCFAE-Value
(define (interp-expr a-fae)
  (type-case KCFAE-Value (interp a-fae (mtSub) (lambda (x) x))
    [numV (n) n]
    [closureV (params body ds) 'function]
    [contV (k) 'function]))

;; run : string -> KCFAE-Value
(define (run string)
  (interp-expr (parse (string->sexpr string))))

(test (run "{+ 1 1}") 2)
(test (run "{{fun {x y} {- y x}} 10 12}") 2)
(test (run "{fun {} 12}") 'function)
(test (run "{fun {x} {fun {} x}}") 'function)
(test (run "{{{fun {x} {fun {} x}} 13}}") 13)
(test (run "{withcc esc {{fun {x y} x} 1 {esc 3}}}") 3)

(test/exn (run "{{fun {x} x} 1 2}") "interp: wrong input number")
(test/exn (run "{withcc esc {esc 2 3 4}}") "interp: wrong input number")

(test (run "{try 7 catch 8}") 7)
(test (run "{try {throw} catch 8}") 8)
(test (run "{try {+ 1 {throw}} catch 8}") 8)
(test (run "{{fun {f} {try {f 3} catch 8}}
             {fun {x} {throw}}}") 8)       
(test (run "{try {try {throw} catch 8} catch 9}") 8)       
(test (run "{try {try {throw} catch {throw}} catch 9}") 9)       
(test (run "{try {try 7 catch {throw}} catch 9}") 7)       
(test (run "{{withcc esc {try {{withcc k {esc k}} 0} catch {fun {x} 8}}}
             {fun {x} {throw}}}") 8)	
(test (run "{- 0 {withcc k {+ {k 10} 17}}}") -10)
(test (run "{- 0 {withcc k {+ 1 {withcc c {k {+ {c 10} 17}}}}}}") -11)
(test (run "{+ 5 {withcc k {+ 1000 {k 5}}}}") 10)
(test (run "{- 0 {withcc k {+ 15 {k 3}}}}") -3)
(test (run "{withcc a {- 0 {withcc b {b 15}}}}") -15)
(test (run "{{{fun {x y} {fun {c} {+ {+ x y} c}}} 1 2} 3}") 6)
(test (run "{if0 {withcc esc {+ 3 {esc 1}}} 10 {- 0 10}}") -10)
(test (run "{if0 {withcc esc {+ 3 {esc 0}}} 10 {- 0 10}}") 10)
(test (run "{- 0 {withcc esc {{fun {f} {f 3}} esc}}}") -3)
(test (run "{{fun {x y} {- y x}} 10 12}") 2)
(test (run "{fun {x} {fun {} x}}") 'function)
(test (run "{{{fun {x} {fun {} x}} 13}}") 13)
(test (run "{withcc esc {{fun {x y} x} 1 {esc 3}}}") 3)
(test (run "{+ 3 {withcc k {+ 5 {k 9}}}}") 12)
(test (run "{{withcc k {fun {x y} {+ x y}}} 4 5}") 9)
(test (run "{+ {withcc k {k 5}} 4}" ) 9)
(test (run "{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} 1 {+ y {g g {- y 1}}}}} 10}") 55) ; recursive function
(test (run "{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {done 100} {+ y {g g {- y 1}}}}} 10}}") 100) ; exit from recursive function using continuation
(test (run "{withcc k {- 0 {k 100}}}" ) 100)
(test (run "{withcc k {k {- 0 100}}}" ) -100)
(test (run "{withcc k {k {+ 100 11}}}" ) 111)
(test (run "{{fun {a b c} {- {+ {withcc k {+ {k 100} a}} b} c}} 100 200 300}" ) 0)
(test (run "{withcc esc {{fun {x y} x} 1 {esc 3}}}") 3)
(test (run "{{withcc esc {{fun {x y} {fun {z} {+ z y}}} 1 {withcc k {esc k}}}} 10}") 20)
(test (run "{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {+ y {g g {- y 1}}}}} 10} catch 110}") 110) ; exit from recursive function using try-catch
(test (run "{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch y}}} 10}") 54) ; test for multiple recursive try-catch
(test (run "{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {done y}}}} 10}}") 2)
(test (run "{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {throw}}}} 10} catch 20110464}") 20110464) ; recursive try-catch throwing (1)
(test (run "{try {{fun {x y z} {a b c}} 1 2 {throw}} catch 0}") 0)
(test (run "{{fun {f} {try {f 3} catch 8}} {fun {x} {throw}}}") 8)
(test (run "{try {- 0 {withcc k {+ 3 {k {throw}}}}} catch 89}") 89)
(test (run "{try {+ 3 {withcc k {+ 1000 {k {throw}}}}} catch 11}") 11)
(test (run "{{fun {x y z} {try {+ 1 {+ x {throw}}} catch {+ y z}}} 1 2 3}") 5)
(test (run "{+ {try {- 10 {throw}} catch 3} 10}") 13)
(test (run "{try {if0 0 {throw} {+ 1 2}} catch {if0 10 1 {try {throw} catch 54}}}") 54)
(test (run "{try {withcc a {+ 1 {withcc b {throw}}}} catch 10}") 10)
(test (run "{try {- 0 {throw}} catch 5}") 5)
(test (run "{try {if0 {throw} 3 4} catch 5}") 5)
(test (run "{try {{fun {x y} {try x catch y}} {throw} 0} catch -1}") -1)
(test (run "{try {try {throw} catch {throw}} catch 9}") 9)
(test (run "{{withcc esc {try {{withcc k {esc k}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}") 8)
(test (run "{{withcc esc {try {{withcc k {try {esc k} catch {fun {x} {fun {y} 9}}}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}") 8)
(test (run "{withcc foo {{fun {x y} {y x}} {+ 2 3} {withcc bar {+ 1 {bar foo}}}}}") 5)
(test (run "{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {zzz 10} {throw}}} catch 42}") 10)
(test (run "{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {throw} {zzz 10}}} catch 42}") 42)
(test (run "{try {withcc zzz {{fun {x y z w} {+ {w {+ x y}} {+ {throw} z}}} 1 2 3 zzz}} catch 42}") 3)
(test (run "{withcc esc {try {+ {throw} {esc 3}} catch 4}}") 4)
(test (run "{withcc esc {{fun {x y} {+ {+ x 3} y}} {withcc k {try {k {esc {throw}}} catch {k 5}}} 7}}") 15)
(test (run "{try {withcc x {+ {x 1} {throw}}} catch 0}") 1)
(test (run "{+ 12 {withcc k {+ 1 {k {{fun {} 7}}}}}}") 19)

(test (run "{{fun {x y} {+ x y}} {try {throw} catch 1} {try {throw} catch 8} }") 9)
(test (run "{{fun {x y} {+ x y}} {try {throw} catch {try {throw} catch 7}} {try {throw} catch 8} }") 15)
(test (run "{{fun {x y} {+ x y}} {try {try {throw} catch {throw}} catch {try {throw} catch 7}} {try {throw} catch 8} }") 15)
(test (run "{{fun {x} {try {try {throw} catch {x}} catch 7}} {fun {} {throw}}}") 7)
(test (run "{{fun {x} {try {try {x} catch {x}} catch 7}} {fun {} {throw}}}") 7)
(test (run "{{fun {x y} {try {try {x} catch {y}} catch 10}} {fun {} {throw}} {fun {} {try {throw} catch 7}}}") 7)
(test (run "{{fun {x y} {try {try {x} catch {y}} catch 7}} {fun {} {throw}} {fun {} {try {throw} catch {throw}}}}") 7)
(test (run "{{fun {x y} {try {try {y} catch {x}} catch 7}} {fun {} {throw}} {fun {} {try {throw} catch {throw}}}}") 7)
(test (run "{{fun {x y} {try {try {y} catch {x}} catch 10}} {fun {} {throw}} {fun {} {try {throw} catch 7}}}") 7)
(test (run "{+ {try {throw} catch 7} {try {+ 10 {+ 10 {+ 10 {throw}}}} catch 8}}") 15)
(test (run "{+ {try {throw} catch 7} {try {+ 10 {+ 10 {+ 10 {throw}}}} catch 8}}") 15)
(test (run "{{fun {x} {+ {try {throw} catch 7} {try {+ 10 {+ {throw} {+ 10 x}}} catch 8}}} {fun {} {try {throw} catch 3}}}") 15)
(test (run "{{fun {x} {+ {try {throw} catch 7} {try {+ 10 {+ 10 {+ 10 {x {throw}}}}} catch 8}}} {fun {x} {try {+ {1 x}} catch 3}}}") 15)
(test (run "{withcc x {+ 1 {x 2}}}") 2)
(test (run "{withcc x {+ 1 {try {throw} catch {x 2}}}}") 2)
(test (run "{+ 1 {try {throw} catch {withcc x {+ 1 2}}}}") 4)
(test (run "{+ {+ 1 {try {throw} catch {withcc x {+ 1 {x 1}}}}} 1}") 3)
(test (run "{{fun {x} {+ {+ 1 {try x catch {withcc x {+ 1 {x 1}}}}} 1}} {try {throw} catch 1}}") 3)