#lang plai

;;problem1
;;aud->won: number -> number
;;to exchage Australian dollar to Korean won
;;(aud->won 1) should produce 800
;;(aud->won 2) should produce 1600
;;(aud->won 1.2) should produce 960
;;(aud->won 0.5) should produce 400
;;(aud->won 0) should produce 0
;;(aud->won -1.2) should produce error message "wrong-numerical-type:not positive number"

(define (aud->won n) 
  (cond 
    ((>= n 0) (* 800 n))
    (else (error "wrong-numerical-type:not positive number"))))
  
(test (aud->won 1) 800)
(test (aud->won 5) 4000)
(test (aud->won 1.2) 960)
(test (aud->won 0.5) 400)
(test (aud->won 0) 0)
(test/exn (aud->won -1.2) "wrong-numerical-type:not positive number")

;;problem2
;;volume-cuboid: number, number, number -> number
;;to get the volume of cuboid
;;(volume-cuboid 1 2 3) should produce 6
;;(volume-cuboid 2 2 2) should produce 8
;;(volume-cuboid 39 23 44) should produce 39468
;;(volume-cuboid -2 2 4) should produce error message "wrong-numerical-type: not positive integer"
;;(volume-cuboid 2 0 4) should produce error message "wrong-numerical-type: not positive integer"
;;(volume-cuboid 2 1.4 4) should produce eerror message "wrong-numerical-type: not positive integer"

(define (volume-cuboid a b c) 
  (cond 
    ((and (and (and (integer? a) (> a 0)) (and (integer? b) (> b 0))) (and (integer? c) (> c 0))) (* (* a b) c))
    (else (error "wrong-numerical-type: not positive integer"))))

(test (volume-cuboid 1 2 3) 6)
(test (volume-cuboid 2 2 2) 8)
(test (volume-cuboid 39 23 44) 39468)
(test/exn (volume-cuboid -2 2 4) "wrong-numerical-type: not positive integer")
(test/exn (volume-cuboid 2 0 4) "wrong-numerical-type: not positive integer")
(test/exn (volume-cuboid 2 1.4 4) "wrong-numerical-type: not positive integer")

;;problem3
;;is-odd?: number -> boolean
;;to determine whether number is odd or not
;;(is-odd? 2) should produce false
;;(is-odd? 3) should produce true
;;(is-odd? -2) should produce flase
;;(is-odd? -1) should produce true
;;(is-odd? 1.2) should produce error message "wrong-numerical-type: not integer"
;;(is-odd? -1.2) should produce error message "wrong-numerical-type: not integer"

(define (is-odd? n)
  (cond 
    ((integer? n) (not (= 0 (remainder n 2))))
    (else (error "wrong-numerical-type: not integer"))))

(test (is-odd? 2) false)
(test (is-odd? 3) true)
(test (is-odd? -2) false)
(test (is-odd? -1) true)
(test/exn (is-odd? 1.2) "wrong-numerical-type: not integer")
(test/exn (is-odd? -1.2) "wrong-numerical-type: not integer")

;;problem4
;;gcd: number, number -> number
;;to get the greatest common divisor of two integer numbers
;;(gcd 1 1) should produce 1
;;(gcd 4 2) should produce 2
;;(gcd 2 4) should produce 2
;;(gcd 11 1) should produce 1
;;(gcd 9 12) should produce 3
;;(gcd 12 9) should produce 3
;;(gcd 11 13) should produce 1
;;(gcd 4 -2) should produce 2
;;(gcd -4 -2) should produce 2
;;(gcd -4 2) should produce 2
;;(gcd 4 0) should produce 4
;;(gcd 0 0) should produce 0
;;(gcd 0 2) should produce 0
;;(gcd 1.1 1.1) should produce error message "wrong-numerical-type: not integer"

(define (gcd a b) 
  (define m (abs a))
  (define n (abs b))
  (cond ((and (integer? m) (integer? n)) (cond 
                                           ((or (= 0 m) (= 0 n)) (cond 
                                                                   ((>= m n) m)
                                                                   (else n)))
                                           ((>= m n) (cond 
                                                       ((= (remainder m n) 0) n)
                                                       (else (gcd n (remainder m n)))))
                                           (else (cond 
                                                   ((= (remainder n m) 0) m)
                                                   (else (gcd m (remainder n m)))))))
        
        (else (error "wrong-numerical-type: not integer"))))

(test (gcd 1 1) 1)
(test (gcd 4 2) 2)
(test (gcd 2 4) 2)
(test (gcd 11 1) 1)
(test (gcd 9 12) 3)
(test (gcd 12 9) 3)
(test (gcd 11 13) 1)
(test (gcd 4 -2) 2)
(test (gcd -4 -2) 2)
(test (gcd -4 2) 2)
(test (gcd 4 0) 4)
(test (gcd 0 0) 0)
(test (gcd 0 2) 2)
(test/exn (gcd 1.1 1.1) "wrong-numerical-type: not integer")

;;problem5
;;lcm: number, number -> number
;;to get the least common multiple of two integer numbers
;;(lcm 3 5) should produce 15
;;(lcm 3 1) should produce 3
;;(lcm 3 -5) should produce 15
;;(lcm 3 0) should produce 0
;;(lcm 0 0) should produce 0
;;(lcm 1.5 1) should produce error message "wrong-numerical-type: not integer"

(define (lcm a b) 
   (cond ((and (integer? a) (integer? b)) (cond 
                                           ((and (= 0 a) (= 0 b)) 0)
                                           (else (abs (/ (* a b) (gcd a b))))))
         (else (error "wrong-numerical-type: not integer"))))
                                       

(test (lcm 3 5) 15)
(test (lcm 3 1) 3)
(test (lcm 3 -5) 15)
(test (lcm 3 0) 0)
(test (lcm 0 0) 0)
(test/exn (lcm 1.5 1) "wrong-numerical-type: not integer")

;;problem6
;;not-negative?: number -> boolean
;;to determine number is positive or not
;;(not-negative? 1) should produce true
;;(not-negative? 1.2) should produce true
;;(not-negative? 0) should produce true
;;(not-negative? -1) should produce false

(define (not-negative? n) (>= n 0))

(test (not-negative? 1) #t)
(test (not-negative? 1.2) #t)
(test (not-negative? 0) #t)
(test (not-negative? -1) #f)

;;not-negative-integer?: number -> boolean
;;to determine number is not negative and ingteger or not
;;(not-negative? 1) should produce true
;;(not-negative? 1.2) should produce false
;;(not-negative? 0) should produce true
;;(not-negative? -1) should produce false

(define (not-negative-integer? n) (and (not-negative? n) (integer? n)))

(test (not-negative-integer? 1) #t)
(test (not-negative-integer? 1.2) #f)
(test (not-negative-integer? 0) #t)
(test (not-negative-integer? -1) #f)

(define-type COURSE
  (CS320 (quiz not-negative-integer?)
         (homework not-negative-integer?))
  (CS311 (homework not-negative-integer?))
  (CS330 (projects not-negative-integer?)
         (homework not-negative-integer?)))

(define cs1 (CS320 0 3))
(define cs2 (CS311 3))
(define cs3 (CS330 1 2))
(define cs4 (CS330 2 2))

;;problem7
;;have-homework: COURSE -> number
;;to get the number of homework in the course
;;(have-homework cs1) should produce 3
;;(have-homework cs2) should produce 3
;;(have-homework cs3) should produce 2
;;(have-homework cs4) should produce 2

(define (have-homework course) 
  (cond ((CS320? course) (CS320-homework course))
        ((CS311? course) (CS311-homework course))
        ((CS330? course) (CS330-homework course))))

(test (have-homework cs1) 3)
(test (have-homework cs2) 3)
(test (have-homework cs3) 2)
(test (have-homework cs4) 2)

;;problem8
;;have-projects: COURSE -> boolean
;;to determine the course has more than 2 projects or not
;;(have-projects cs1) should produce false
;;(have-projects cs2) should produce false
;;(have-projects cs3) should produce false
;;(have-projects cs4) should produce true

(define (have-projects course)
  (cond ((CS330? course) (>= (CS330-projects course) 2))
        (else false)))

(test (have-projects cs1) #f)
(test (have-projects cs2) #f)
(test (have-projects cs3) #f)
(test (have-projects cs4) #t)

;;problem9
;;name-pets: list -> list
;;to replace name of pets in lists from dog to smart, from cat to happy, and from pig to pinky
;;(name-pets l1) should produce '(happy smart pinky)
;;(name-pets l2) should produce '(happy smart pinky fish turtle)
;;(name-pets l3) should produce '(happy smart fish pinky)
;;(name-pets l4) should produce '(happy)
;;(name-pets l5) should produce '(happy smart)
;;(name-pets l6) should produce '(smart pinky happy)
;;(name-pets l7) should produce '(fish)
;;(name-pets l8) should produce error message "wrong type: operand is not a list"
;;(name-pets l9) should produce error message "wrong type: element in list is not a symbol"
;;(name-pets l10) should produce error message "wrong type: element in list is not a symbol"
;;(name-pets l11) should produce '()
;;(name-pets l12) should produce '(smart happy smart)

(define (name-pets l)
  (cond 
    ((not (list? l)) (error "wrong type: operand is not a list"))
    ((empty? l) (list ))
    ((not (symbol? (first l))) (error "wrong type: element in list is not a symbol"))
    ((symbol=? 'dog (first l)) (append (list 'happy) (name-pets (rest l))))
    ((symbol=? 'cat (first l)) (append (list 'smart) (name-pets (rest l))))
    ((symbol=? 'pig (first l)) (append (list 'pinky) (name-pets (rest l))))
    (else (append (list (first l)) (name-pets (rest l))))))

(define l1 (list 'dog 'cat 'pig))
(define l2 (list 'dog 'cat 'pig 'fish 'turtle))
(define l3 (list 'dog 'cat 'fish 'pig))
(define l4 (list 'dog))
(define l5 (list 'dog 'cat))
(define l6 (list 'cat 'pig 'dog))
(define l7 (list 'fish))
(define l8 1)
(define l9 (list 'fish 1))
(define l10 (list 'fish '()))
(define l11 (list ))
(define l12 (list 'cat 'dog 'cat))

(test (name-pets l1) (list 'happy 'smart 'pinky))
(test (name-pets l2) (list 'happy 'smart 'pinky 'fish 'turtle))
(test (name-pets l3) (list 'happy 'smart 'fish 'pinky))
(test (name-pets l4) (list 'happy))
(test (name-pets l5) (list 'happy 'smart))
(test (name-pets l6) (list 'smart 'pinky 'happy))
(test (name-pets l7) (list 'fish))
(test/exn (name-pets l8) "wrong type: operand is not a list")
(test/exn (name-pets l9) "wrong type: element in list is not a symbol")
(test/exn (name-pets l10) "wrong type: element in list is not a symbol")
(test (name-pets l11) (list ))
(test (name-pets l12) (list 'smart 'happy 'smart))

;;problem10
;;give-name: list -> list
;;to replace all the name of pets in lists from old to new
;;(give-name 'bear 'pooh lst1) should produce '(pig cat pooh))
;;(give-name 'cat 'kitty lst2) should produce '(dog kitty fish turtle)
;;(give-name 'cat 'kitty lst3) should produce '(dog kitty fish kitty)
;;(give-name 'cat 'kitty lst4) should produce '(dog kitty fish kitty turtle kitty)
;;(give-name 'cat 'kitty lst5) should produce error message "wrong type: operand is not a list"
;;(give-name 'cat 'kitty lst6) should produce error message "wrong type: element in list is not a symbol"
;;(give-name 'cat 'kitty lst7) should produce error message "wrong type: element in list is not a symbol"
;;(give-name 'cat 'kitty lst8) should produce '()

(define (give-name old new l)
  (cond 
    ((not (list? l)) (error "wrong type: operand is not a list"))
    ((empty? l) (list ))
    ((not (symbol? (first l))) (error "wrong type: element in list is not a symbol"))
    ((symbol=? old (first l)) (append (list new) (give-name old new (rest l))))
    (else (append (list (first l)) (give-name old new (rest l))))))

(define lst1 (cons 'pig (cons 'cat (cons 'bear empty))))
(define lst2 (list 'dog 'cat 'fish 'turtle))
(define lst3 (list 'dog 'cat 'fish 'cat))
(define lst4 (list 'dog 'cat 'fish 'cat 'turtle 'cat))
(define lst5 1)
(define lst6 (list 'dog 1 'fish 'cat 'turtle 'cat))
(define lst7 (list 'cat '()))
(define lst8 (list ))

(test (give-name 'bear 'pooh lst1) (list 'pig 'cat 'pooh))
(test (give-name 'cat 'kitty lst2) (list 'dog 'kitty 'fish 'turtle))
(test (give-name 'cat 'kitty lst3) (list 'dog 'kitty 'fish 'kitty))
(test (give-name 'cat 'kitty lst4) (list 'dog 'kitty 'fish 'kitty 'turtle 'kitty))
(test/exn (give-name 'cat 'kitty lst5) "wrong type: operand is not a list")
(test/exn (give-name 'cat 'kitty lst6) "wrong type: element in list is not a symbol")
(test/exn (give-name 'cat 'kitty lst7) "wrong type: element in list is not a symbol")
(test (give-name 'cat 'kitty lst8) (list ))

;;reference
;;https://docs.racket-lang.org/plai/plai-scheme.html
;;https://docs.racket-lang.org/reference/