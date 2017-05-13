#lang plai

;;problem1
(define-type WAE
  (num (n number?))
  (add (lhs WAE?) (rhs WAE?))
  (sub (lhs WAE?) (rhs WAE?))
  (with (name symbol?)
        (named-expr WAE?)
        (body WAE?))
  (id (name symbol?)))

;;gen_list_with_dupl: list, ... -> list
;;to generate list with multiple number of list by using append regardless of duplicates. 
;;(gen_list_with_dupl '() '(())) should produce '(())
;;(gen_list_with_dupl 1) should produce "wrong type: operand is not a list"
;;(gen_list_with_dupl '() 11) should produce "wrong type: operand is not a list"
;;(gen_list_with_dupl '() 'x) should produce "wrong type: operand is not a list"
;;(gen_list_with_dupl '() '(1) '(2)) should produce '(1 2)
(define-syntax gen_list_with_dupl 
  (syntax-rules ()
    ((_ a ...)
     (cond ((findf (lambda (arg) (not (false? arg))) (map (lambda (arg) (not (list? arg))) (list a ...)))
            (error "wrong type: operand is not a list"))
           (else (append a ...))))))

(test (gen_list_with_dupl '() '(())) '(()))
(test/exn (gen_list_with_dupl 1) "wrong type: operand is not a list")
(test/exn (gen_list_with_dupl '() 11) "wrong type: operand is not a list")
(test/exn (gen_list_with_dupl '() 'x) "wrong type: operand is not a list")
(test (gen_list_with_dupl '() '(1) '(2)) '(1 2))
  
;;list_reorder_and_remove_dupl: list -> list
;;to generate list without dulicate element and reorder the element
;;(list_reoder_and_remove_dupl '(x y e a t y x)) should producce '(a e t x y)
;;(list_reoder_and_remove_dupl 'x) should producce "wrong type: operand is not a list"
;;(list_reoder_and_remove_dupl 11) should producce "wrong type: operand is not a list"

(define (list_reorder_and_remove_dupl l)
  (define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))
  (cond ((not (list? l)) (error "wrong type: operand is not a list"))
        (else (remove-duplicates (sort l symbol<?)))))

(test (list_reorder_and_remove_dupl '(x y e a t y x)) '(a e t x y))
(test/exn (list_reorder_and_remove_dupl 'x) "wrong type: operand is not a list")
(test/exn (list_reorder_and_remove_dupl 11) "wrong type: operand is not a list")

;;free-ids: WAE -> list
;;to get free identifiers in WAE with no duplicates and in order.
;;(free-ids (id 'x) should produce '(x)
;;(free-ids (add (id 'x) (id 'y))) should produce '(x y)
;;(free-ids (sub (id 'x) (id 'y))) should produce '(x y)
;;(free-ids (with 'x (num 3) (num 3))) should produce '()
;;(free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) should produce '()
;;(free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x))))) should produce '(a)
;;(free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x))))) should produce '(a b)
;;(free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b)))))) should produce '(a b)
;;(free-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) should produce '(a b y)
;;(free-ids (with 'x (id 't) (sub (id 'x) (with 'y (id 'y) (add (id 'x) (sub (id 'b) (id 'a))))))) should produce '(a b t y)
;;(free-ids (with 'x (with 'y (num 3) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y)))) should produce '(x y)
;;(free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'a) (id 'a)))) should produce '(a b c y)
;;(free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) should produce '(b c d y)
;;(free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) should produce '(b c d y z)

(define (free-ids wae)
  (type-case WAE wae
    (num (n) '())
    (add (l r) (list_reorder_and_remove_dupl(gen_list_with_dupl (free-ids l) (free-ids r))))
    (sub (l r) (list_reorder_and_remove_dupl (gen_list_with_dupl (free-ids l) (free-ids r))))
    (with (n ne b) (list_reorder_and_remove_dupl (gen_list_with_dupl (remove n (free-ids b)) (free-ids ne))))
    (id (n) (list n))))

(test (free-ids (id 'x)) '(x))
(test (free-ids (add (id 'x) (id 'y))) '(x y))
(test (free-ids (sub (id 'x) (id 'y))) '(x y))
(test (free-ids (with 'x (num 3) (num 3))) '())
(test (free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '())
(test (free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x))))) '(a))
(test (free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b)))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b y))
(test (free-ids (with 'x (id 't) (sub (id 'x) (with 'y (id 'y) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b t y))
(test (free-ids (with 'x (with 'y (num 3) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y)))) '(x y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'a) (id 'a)))) '(a b c y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(b c d y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(b c d y z))

;;problem2
;;binding-ids: WAE -> list
;;to get binding identifiers in WAE with no duplicates and in order.
;;(binding-ids (id 'x)) should produce '()
;;(binding-ids (add (num 3) (sub (id 'x) (id 'y)))) should produce '()
;;(binding-ids (with 'y (num 3) (with 'x (id 'x) (id 'y)))) should produce '(x y)
;;(binding-ids (with 'y (num 3) (with 'y (id 'x) (add (id 'x) (id 'y))))) should produce '(y)
;;(binding-ids (with 'y (num 3) (with 'y (with 'x (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y))))) should produce '(x y)
;;(binding-ids (with 'z (num 3) (with 'w (with 'z (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (with 'w (id 'y) (add (num 7) (id 'w)))))) should produce '(w z)

(define (binding-ids wae)
  (type-case WAE wae
    (num (n) '())
    (add (l r) (list_reorder_and_remove_dupl(gen_list_with_dupl (binding-ids l) (binding-ids r))))
    (sub (l r) (list_reorder_and_remove_dupl (gen_list_with_dupl (binding-ids l) (binding-ids r))))
    (with (n ne b) (list_reorder_and_remove_dupl (gen_list_with_dupl (list n) (binding-ids b) (binding-ids ne))))
    (id (n) '())))

(test (binding-ids (id 'x)) '())
(test (binding-ids (add (num 3) (sub (id 'x) (id 'y)))) '())
(test (binding-ids (with 'y (num 3) (with 'x (id 'x) (id 'y)))) '(x y))
(test (binding-ids (with 'y (num 3) (with 'y (id 'x) (add (id 'x) (id 'y))))) '(y))
(test (binding-ids (with 'y (num 3) (with 'y (with 'x (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y))))) '(x y))
(test (binding-ids (with 'z (num 3) (with 'w (with 'z (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (with 'w (id 'y) (add (num 7) (id 'w)))))) '(w z))

;;problem3
;;ids: WAE -> list
;;to get every ids in WAE without binding identifier
;;(ids (with 'x (num 3) (add (id 'y) (num 3)))) should produce '(y)
;;(ids (with 'x (num 3) (add (id 'x) (sub (id 'x) (id 'y))))) should produce '(x y)
;;(ids (with 'x (num 3) (add (id 'x) (with 'y (num 7) (sub (id 'x) (id 'y)))))) should produce '(x y)
;;(ids (with 'x (num 3) (with 'y (id 'x) (sub (num 3) (id 'y))))) should produce '(x y)
;;(ids (with 'x (num 3) (add (id 'y) (with 'y (id 'x) (sub (num 3) (num 7)))))) should produce '(x y)
;;(ids (with 'x (id 'x) (add (id 'y) (with 'y (id 'y) (sub (num 3) (with 'z (num 7) (sub (id 'z) (id 'x)))))))) should produce '(x y z)
;;(ids (with 'x (with 'y (num 3) (add (id 'x) (id 'y))) (add (id 'y) (with 'y (id 'y) (sub (num 3) (num 7)))))) should produce '(x y)
;;(ids (with 'x (id 'a) (with 'y (id 'b) (with 'z (id 'c) (add (id 'd) (sub (id 'x) (add (id 'y) (id 'z)))))))) should produce '(a b c d x y z)
;;(ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) should produce '(a b c d x y)
;;(ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) should produce '(b c d x y z)

(define (ids wae)
  (type-case WAE wae
    (num (n) '())
    (add (l r) (list_reorder_and_remove_dupl(gen_list_with_dupl (ids l) (ids r))))
    (sub (l r) (list_reorder_and_remove_dupl (gen_list_with_dupl (ids l) (ids r))))
    (with (n ne b) (list_reorder_and_remove_dupl (gen_list_with_dupl (ids ne) (ids b))))
    (id (n) (list n))))

(test (ids (with 'x (num 3) (add (id 'y) (num 3)))) '(y))
(test (ids (with 'x (num 3) (add (id 'x) (sub (id 'x) (id 'y))))) '(x y))
(test (ids (with 'x (num 3) (add (id 'x) (with 'y (num 7) (sub (id 'x) (id 'y)))))) '(x y))
(test (ids (with 'x (num 3) (with 'y (id 'x) (sub (num 3) (id 'y))))) '(x y))
(test (ids (with 'x (num 3) (add (id 'y) (with 'y (id 'x) (sub (num 3) (num 7)))))) '(x y))
(test (ids (with 'x (id 'x) (add (id 'y) (with 'y (id 'y) (sub (num 3) (with 'z (num 7) (sub (id 'z) (id 'x)))))))) '(x y z))
(test (ids (with 'x (with 'y (num 3) (add (id 'x) (id 'y))) (add (id 'y) (with 'y (id 'y) (sub (num 3) (num 7)))))) '(x y))
(test (ids (with 'x (id 'a) (with 'y (id 'b) (with 'z (id 'c) (add (id 'd) (sub (id 'x) (add (id 'y) (id 'z)))))))) '(a b c d x y z))
(test (ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(a b c d x y))
(test (ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(b c d x y z))

;;find: symbol, list -> list
;;to find a symbol in the list and if it is, return list of the symbol, else return empty list
;;(find 1 '(x y)) should produce "wrong type: operand is not a symbol"
;;(find 1 '(1 x y)) should produce "wrong type: operand is not a symbol"
;;(find 'x '(x x y)) should produce '(x)
;;(find 'x '(y y)) should produce '()

(define (find a b)
  (cond ((not (symbol? a)) (error "wrong type: operand is not a symbol"))
        ((not (list? b)) (error "wrong type: operand is not a list"))
        ((findf (lambda (arg) (eq? arg a)) b) (cons a '()))
        (else '())))

(test/exn (find 1 '(x y)) "wrong type: operand is not a symbol")
(test/exn (find 1 '(1 x y)) "wrong type: operand is not a symbol")
(test/exn (find 'x 1) "wrong type: operand is not a list")
(test (find 'x '(x x y)) '(x))
(test (find 'x '(y y)) '())

;;bound-ids: WAE -> list
;;to get bound identifiers in WAE with no duplicates and in order.
;;(bound-ids (id 'x)) should produce '()
;;(bound-ids (add (id 'x) (num 3)) should produce '()
;;(bound-ids (with 'x (num 3) (add (id 'y) (num 3)))) should produce '()
;;(bound-ids (with 'x (num 3) (add (id 'x) (sub (id 'x) (id 'y))))) should produce '(x)
;;(bound-ids (with 'x (num 3) (add (id 'x) (with 'y (num 7) (sub (id 'x) (id 'y)))))) should produce '(x y)
;;(bound-ids (with 'x (num 3) (with 'y (id 'x) (sub (num 3) (id 'y))))) should produce '(x y)
;;(bound-ids (with 'x (num 3) (add (id 'y) (with 'y (id 'x) (sub (num 3) (num 7)))))) should produce '(x)
;;(bound-ids (with 'x (id 'x) (add (id 'y) (with 'y (id 'y) (sub (num 3) (with 'z (num 7) (sub (id 'z) (id 'x)))))))) should produce '(x z)
;;(bound-ids (with 'x (with 'y (num 3) (add (id 'x) (id 'y))) (add (id 'y) (with 'y (id 'y) (sub (num 3) (num 7)))))) should produce '(y)
;;(bound-ids (with 'x (id 'a) (with 'y (id 'b) (with 'z (id 'c) (add (id 'd) (sub (id 'x) (add (id 'y) (id 'z)))))))) should produce '(x y z)
;;(bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) should produce '(a x)
;;(bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) should produce '(x)

(define (bound-ids wae)
  (type-case WAE wae
    (num (n) '())
    (add (l r) (list_reorder_and_remove_dupl(gen_list_with_dupl (bound-ids l) (bound-ids r))))
    (sub (l r) (list_reorder_and_remove_dupl (gen_list_with_dupl (bound-ids l) (bound-ids r))))
    (with (n ne b) (list_reorder_and_remove_dupl (gen_list_with_dupl (find n (ids b)) (bound-ids ne) (bound-ids b))))                                                                             
    (id (n) '())))

(test (bound-ids (id 'x)) '())
(test (bound-ids (add (id 'x) (num 3))) '())
(test (bound-ids (with 'x (num 3) (add (id 'y) (num 3)))) '())
(test (bound-ids (with 'x (num 3) (add (id 'x) (sub (id 'x) (id 'y))))) '(x))
(test (bound-ids (with 'x (num 3) (add (id 'x) (with 'y (num 7) (sub (id 'x) (id 'y)))))) '(x y))
(test (bound-ids (with 'x (num 3) (with 'y (id 'x) (sub (num 3) (id 'y))))) '(x y))
(test (bound-ids (with 'x (num 3) (add (id 'y) (with 'y (id 'x) (sub (num 3) (num 7)))))) '(x))
(test (bound-ids (with 'x (id 'x) (add (id 'y) (with 'y (id 'y) (sub (num 3) (with 'z (num 7) (sub (id 'z) (id 'x)))))))) '(x z))
(test (bound-ids (with 'x (with 'y (num 3) (add (id 'x) (id 'y))) (add (id 'y) (with 'y (id 'y) (sub (num 3) (num 7)))))) '(y))
(test (bound-ids (with 'x (id 'a) (with 'y (id 'b) (with 'z (id 'c) (add (id 'd) (sub (id 'x) (add (id 'y) (id 'z)))))))) '(x y z))
(test (bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(a x))
(test (bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(x))