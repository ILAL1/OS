#lang plai-typed

(define-type TMFAE
  [num (n : number)]
  [bool (b : boolean)]
  [add (lhs : TMFAE)
       (rhs : TMFAE)]
  [sub (lhs : TMFAE)
       (rhs : TMFAE)]
  [eq (lhs : TMFAE)
      (rhs : TMFAE)]
  [id (name : symbol)]
  [ifthenelse (if-expr : TMFAE)
              (then-expr : TMFAE)
              (else-expr : TMFAE)]
  [fun (params : (listof symbol))
       (paramtys : (listof TE))
       (body : TMFAE)]
  [app (fun-expr : TMFAE)
       (arg-exprs : (listof TMFAE))]
  [with (names : (listof symbol))
        (nametys : (listof TE))
        (inits : (listof TMFAE))
        (body : TMFAE)]
  ;[try1 (try-expr : TMFAE)
  ;      (catch-expr : TMFAE)]
  ;[throw]
  [pair (fst : TMFAE)
        (snd : TMFAE)]
  [fst (pair : TMFAE)]
  [snd (pair : TMFAE)])

(define-type TE
  [numTE]
  [boolTE]
  [arrowTE (param : (listof TE))
           (result : TE)]
  [crossTE (fst : TE)
           (snd : TE)]
  ;;[anyTE]
  )

(define-type TMFAE-Value
  [numV (n : number)]
  [closureV (param : (listof symbol))
            (body : TMFAE)
            (ds : DefrdSub)]
  [boolV (b : boolean)]
  [pairV (fst : TMFAE-Value)
         (snd : TMFAE-Value)])

(define-type DefrdSub
  [mtSub]
  [aSub (name : symbol)
        (value : TMFAE-Value)
        (rest : DefrdSub)])

(define-type Type
  [numT]
  [boolT]
  [arrowT (param : (listof Type))
          (result : Type)]
  [crossT (fst : Type)
          (snd : Type)]
  [anyT])

(define-type TypeEnv
  [mtEnv]
  [aBind (name : symbol)
         (type : Type)
         (rest : TypeEnv)])

;; ----------------------------------------

;; interp : TMFAE DefrdSub -> TMFAE-Value
(define (interp tmfae ds)
  (type-case TMFAE tmfae
    [num (n) (numV n)]
    [bool (b) (boolV b)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [eq (l r) (boolV (= (numV-n (interp l ds)) (numV-n (interp r ds))))]
    [ifthenelse (i t e) 
                (local [(define if-val (boolV-b (interp i ds)))]
                  (if (equal? if-val true) (interp t ds)
                      (interp e ds)))]
    [id (name) (lookup name ds)]
    [fun (params param-tes body-expr)
         (closureV params body-expr ds)]
    [app (fun-expr arg-exprs)
         (local [(define fun-val (interp fun-expr ds))
                 (define arg-vals (interp-multiple arg-exprs ds))]
           (interp (closureV-body fun-val)
                   (aSub-multiple (closureV-param fun-val) 
                                arg-vals
                                (closureV-ds fun-val))))]
    [with (names nametys inits body)
          (interp body (aSub-multiple names (interp-multiple inits ds) ds))]
    [pair (fst snd) (pairV (interp fst ds) (interp snd ds))]
    [fst (pair) (pairV-fst (interp pair ds))]
    [snd (pair) (pairV-snd (interp pair ds))]))

(define (interp-multiple arg-exprs ds)
  (if (empty? arg-exprs) empty
      (append (list (interp (first arg-exprs) ds)) (interp-multiple (rest arg-exprs) ds))))

(define (aSub-multiple params arg-vals ds)
  (if (empty? arg-vals) ds
      (aSub-multiple (rest params) (rest arg-vals) (aSub (first params) (first arg-vals) ds))))

;; num-op : (number number -> number) -> (TMFAE-Value TMFAE-Value -> TMFAE-Value)
(define (num-op op x y)
  (numV (op (numV-n x) (numV-n y))))

(define (num+ x y) (num-op + x y))
(define (num- x y) (num-op - x y))

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free variable")]
    [aSub (sub-name num rest-ds)
          (if (symbol=? sub-name name)
              num
              (lookup name rest-ds))]))

;; ----------------------------------------

(define (get-type name-to-find env)
  (type-case TypeEnv env
    [mtEnv () (error 'get-type "free variable, so no type")]
    [aBind (name ty rest)
           (if (symbol=? name-to-find name)
               ty
               (get-type name-to-find rest))]))

;; ----------------------------------------

(define (parse-type te)
  (type-case TE te
    [numTE () (numT)]
    [boolTE () (boolT)]
    [arrowTE (a b) (arrowT (map parse-type a)
                           (parse-type b))]
    [crossTE (fst snd) (crossT (parse-type fst)
                               (parse-type snd))]))



(define (type-error TMFAE msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string TMFAE)
                      (string-append " not "
                                     msg)))))

(define (type-error2 TMFAE1 TMFAE2 msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string TMFAE1)
                      (string-append 
                       (to-string TMFAE2)
                       (string-append " not "
                                     msg))))))

(define typecheck : (TMFAE TypeEnv -> Type)
  (lambda (tmfae env)
    (type-case TMFAE tmfae
      [num (n) (numT)]
      [bool (b) (boolT)]
      [add (l r) (type-case Type (typecheck l env)
                   [numT ()
                         (type-case Type (typecheck r env)
                           [numT () (numT)]
                           [else (type-error r "num")])]
                   [else (type-error l "num")])]
      [sub (l r) (type-case Type (typecheck l env)
                   [numT ()
                         (type-case Type (typecheck r env)
                           [numT () (numT)]
                           [else (type-error r "num")])]
                   [else (type-error l "num")])]
      [eq (l r) (type-case Type (typecheck l env)
                  [numT () 
                        (type-case Type (typecheck r env)
                          [numT () (boolT)]
                          [else (type-error r "num")])]
                  [else (type-error l "num")])]
      [ifthenelse (i t e) (type-case Type (typecheck i env)
                            [boolT ()
                                   (local [(define t-type (typecheck t env))
                                           (define e-type (typecheck e env))]
                                     (if (equal? t-type e-type)
                                         t-type
                                         (type-error2 e t "same type")))]
                            [else (type-error i "bool")])]
      [id (name) (get-type name env)]
      [fun (names tes body)
           (local [(define param-types (parse-type-multiple tes))]
             (arrowT param-types
                     (typecheck body (aBind-multiple names
                                            param-types
                                            env))))]
      [app (fn args)
           (type-case Type (typecheck fn env)
             [arrowT (param-types result-type)
                     (if (equal? param-types
                                 (typecheck-multiple args env))
                         result-type
                         (type-error args
                                     (to-string param-types)))]
             [else (type-error fn "function")])]
      [with (names nametys inits body)
            (typecheck body (aBind-multiple names (parse-type-multiple nametys) env))]
      [pair (fst snd)
            (crossT (typecheck fst env)
                      (typecheck snd env))]
      [fst (pair)
           (type-case Type (typecheck pair env)
             [crossT (fst snd)
                     fst]
             (else (type-error pair "pair")))]
      [snd (pair)
           (type-case Type (typecheck pair env)
             [crossT (fst snd)
                     snd]
             (else (type-error pair "pair")))])))

(define (parse-type-multiple tes)
  (if (empty? tes) empty
      (append (list (parse-type (first tes))) (parse-type-multiple (rest tes)))))

(define (aBind-multiple names param-types env)
  (if (empty? names) env
      (aBind-multiple (rest names) (rest param-types) (aBind (first names) (first param-types) env))))

(define (typecheck-multiple args env)
  (if (empty? args) empty
      (append (list (typecheck (first args) env)) (typecheck-multiple (rest args) env))))

;; ----------------------------------------

(test (interp (num 10)
              (mtSub))
      (numV 10))
(test (interp (add (num 10) (num 17))
              (mtSub))
      (numV 27))
(test (interp (sub (num 10) (num 7))
              (mtSub))
      (numV 3))
(test (interp (app (fun (list 'x) (list (numTE)) (add (id 'x) (num 12)))
                   (list (add (num 1) (num 17))))
              (mtSub))
      (numV 30))
(test (interp (id 'x)
              (aSub 'x (numV 10) (mtSub)))
      (numV 10))

(test (interp (app (fun (list 'x) (list (numTE))
                        (app (fun (list 'f) (list (arrowTE (list (numTE)) (numTE)))
                                  (add (app (id 'f) (list (num 1)))
                                       (app (fun (list 'x) (list (numTE))
                                                 (app (id 'f)
                                                      (list (num 2))))
                                            (list (num 3)))))
                             (list (fun (list 'y) (list (numTE))
                                  (add (id 'x) (id 'y))))))
                   (list (num 0)))
              (mtSub))
      (numV 3))

(test/exn (interp (id 'x) (mtSub))
          "free variable")

(test (typecheck (num 10) (mtEnv))
      (numT))

(test (typecheck (add (num 10) (num 17)) (mtEnv))
      (numT))
(test (typecheck (sub (num 10) (num 7)) (mtEnv))
      (numT))

(test (typecheck (fun (list 'x) (list (numTE)) (add (id 'x) (num 12))) (mtEnv))
      (arrowT (list (numT)) (numT)))

(test (typecheck (fun (list 'x) (list (numTE)) (fun (list 'y) (list (boolTE)) (id 'x))) (mtEnv))
      (arrowT (list (numT)) (arrowT (list (boolT))  (numT))))

(test (typecheck (app (fun (list 'x) (list (numTE)) (add (id 'x) (num 12)))
                      (list (add (num 1) (num 17))))
                 (mtEnv))
      (numT))

(test (typecheck (app (fun (list 'x) (list (numTE))
                           (app (fun (list 'f) (list (arrowTE (list (numTE)) (numTE)))
                                     (add (app (id 'f) (list (num 1)))
                                          (app (fun (list 'x) (list (numTE)) (app (id 'f) (list (num 2))))
                                               (list (num 3)))))
                                (list (fun (list 'y) (list (numTE))
                                     (add (id 'x)
                                          (id' y))))))
                      (list (num 0)))
                 (mtEnv))
      (numT))

(test/exn (typecheck (app (num 1) (list (num 2))) (mtEnv))
          "no type")

(test/exn (typecheck (add (fun (list 'x) (list (numTE)) (num 12))
                          (num 2))
                     (mtEnv))
          "no type")

(test (interp (eq (num 13)
                  (ifthenelse (eq (num 1) (add (num -1) (num 2)))
                              (num 12)
                              (num 13)))
              (mtSub))
      (boolV false))

(test (typecheck (eq (num 13)
                     (ifthenelse (eq (num 1) (add (num -1) (num 2)))
                                 (num 12)
                                 (num 13)))
                 (mtEnv))
      (boolT))
(test/exn (typecheck (add (num 1)
                          (ifthenelse (bool true)
                                      (bool true)
                                      (bool false)))
                     (mtEnv))
          "no type")

(test (interp (fst (pair (num 10) (num 8))) (mtSub)) (numV 10))
(test (interp (snd (pair (num 10) (num 8))) (mtSub)) (numV 8))
(test (typecheck (pair (num 10) (num 8)) (mtEnv)) (crossT (numT) (numT)))
(test (typecheck (add (num 1) (snd (pair (num 10) (num 8)))) (mtEnv)) (numT))
(test (typecheck (fun (list 'x) (list (crossTE (numTE) (boolTE)))
                      (ifthenelse (snd (id 'x)) (num 0) (fst (id 'x))))
                 (mtEnv))
      (arrowT (list (crossT (numT) (boolT))) (numT)))
(test/exn (typecheck (fst (num 10)) (mtEnv)) "no type")
(test/exn (typecheck (add (num 1) (fst (pair (bool false) (num 8)))) (mtEnv)) "no type")
(test/exn (typecheck (fun (list 'x) (list (crossTE (numTE) (boolTE)))
                          (ifthenelse (fst (id 'x)) (num 0) (fst (id 'x))))
                     (mtEnv))
          "no type")