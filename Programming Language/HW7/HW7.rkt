#lang plai-typed

(define-type EXPR
  [num (n : number)]
  [bool (b : boolean)]
  [add (lhs : EXPR) (rhs : EXPR)]
  [sub (lhs : EXPR) (rhs : EXPR)]
  [equ (lhs : EXPR) (rhs : EXPR)]
  [id (name : symbol)]
  [fun (param : (listof symbol)) (paramty : (listof TE)) (body : EXPR)]
  [app (fun-expr : EXPR) (arg-expr : (listof EXPR))]
  [ifthenelse (test-expr : EXPR) (then-expr : EXPR) (else-expr : EXPR)]
  [rec (name : symbol) (ty : TE) (named-expr : EXPR) (body : EXPR)]
  [with-type (name : symbol)
             (var1-name : symbol) (var1-ty : TE)
             (var2-name : symbol) (var2-ty : TE)
             (body-expr : EXPR)]
  [cases (name : symbol)
         (dispatch-expr : EXPR)
         (var1-name : symbol) (bind1-name : symbol) (rhs1-expr : EXPR)
         (var2-name : symbol) (bind2-name : symbol) (rhs2-expr : EXPR)])

(define-type TE
  [numTE]
  [boolTE]
  [arrowTE (params : (listof TE)) (result : TE)]
  [idTE (name : symbol)]
)

(define-type EXPR-Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closureV (param : (listof symbol)) (body : EXPR) (ds : DefrdSub)]
  [variantV (right? : boolean) (val : EXPR-Value)]
  [constructorV (right? : boolean)])

(define-type DefrdSub
  [mtSub]
  [aSub (name : symbol)
        (value : EXPR-Value)
        (rest : DefrdSub)]
  [aRecSub (sub-name : symbol)
           (val-box : (boxof  EXPR-Value))
           (rest-ds : DefrdSub)])

(define-type Type
  [numT]
  [boolT]
  [arrowT (params : (listof Type)) (result : Type)]
  [idT (name : symbol)]
)


(define-type TypeEnv
  [mtEnv]
  [aBind (name : symbol)
         (type : Type)
         (rest : TypeEnv)]
  [tBind (name : symbol)
         (var1-name : symbol) (var1-type : Type)
         (var2-name : symbol) (var2-type : Type)
         (rest : TypeEnv)])

;; ----------------------------------------

;; eval : EXPR DefrdSub (EXPR-Value -> alpha) -> alpha
(define (eval expr ds k)
  (type-case EXPR expr
    [num (n) (k (numV n))]
    [bool (b) (k (boolV b))]
    [add (l r) (eval l ds 
                       (lambda (v1) 
                         (eval r ds
                                 (lambda (v2)
                                   (k (num+ v1 v2))))))]
    [sub (l r) (eval l ds 
                       (lambda (v1) 
                         (eval r ds
                                 (lambda (v2)
                                   (k (num- v1 v2))))))]
    [equ (l r) (eval l ds
                      (lambda (v1)
                        (eval r ds
                                (lambda (v2)
                                  (k (boolV (= (numV-n v1) (numV-n v2))))))))]
    [ifthenelse (i t e)
                (eval i ds
                        (lambda (v1)
                          (if (equal? (boolV-b v1) true)
                              (eval t ds k)
                              (eval e ds k))))]
    [id (name) (k (lookup name ds))]
    [fun (params param-tes body-expr)
         (k (closureV params body-expr ds))]
    [app (fun-expr arg-exprs)
         (eval fun-expr ds
                 (lambda (fun-val)
                   (if (equal? (length arg-exprs) (type-case EXPR-Value fun-val
                                               [closureV (params body ds) (length params)]
                                               [constructorV (right?) 1]
                                               [else (error 'eval "not a function")]))
                       (eval-multiple arg-exprs ds
                                        (lambda (arg-vals)
                                          (type-case EXPR-Value fun-val
                                            [closureV (params body fun-ds)
                                                      (eval body
                                                              (aSub-multiple params arg-vals fun-ds)
                                                              k)]
                                            [constructorV (right?)
                                                          (k (variantV right? (first arg-vals)))]
                                            [else (error 'eval "not a function")])))
                       (error 'eval "wrong input number"))))]
    [rec (bound-id type named-expr body-expr)
         (local [(define value-holder (box (numV 42)))
                 (define new-ds
                         (aRecSub bound-id value-holder ds))]
           (eval named-expr new-ds 
                   (lambda (named-val)
                     (begin
                       (set-box! value-holder named-val)
                       (eval body-expr new-ds k)))))]
    [with-type (type-name var1-name var1-te
                          var2-name var2-te
                          body-expr)
               (eval body-expr
                       (aSub var1-name
                             (constructorV false)
                             (aSub var2-name
                                   (constructorV true)
                                   ds))
                       k)]
    [cases (ty dispatch-expr
               var1-name var1-id var1-rhs
               var2-name var2-id var2-rhs)
       (eval dispatch-expr ds
               (lambda (dispatch-val)
                 (type-case EXPR-Value dispatch-val
                   [variantV (right? val)
                             (if (not right?)
                                 (eval var1-rhs (aSub var1-id val ds) k)
                                 (eval var2-rhs (aSub var2-id val ds) k))]
                   [else (error 'eval "not a variant result")])))]

    ))


(define (length l)
  (if (empty? l) 0
      (+ 1 (length (rest l)))))

;;eval-multiple : (listof EXPR) DefrdSub (EXPR-Value -> alpha) alpha
(define (eval-multiple arg-exprs ds k)
  (if (empty? arg-exprs) (k empty)
      (eval (first arg-exprs) ds
              (lambda (arg-val)
                (eval-multiple (rest arg-exprs) ds
                                 (lambda (arg-vals) (k (append (list arg-val) arg-vals))))))))

;;aSub-multiple : (listof symbol) (listof EXPR-Value) DefrdSub (EXPR-Value -> alpha) alpha
(define (aSub-multiple params arg-vals ds)
  (if (empty? arg-vals) ds
      (aSub-multiple (rest params) (rest arg-vals) (aSub (first params) (first arg-vals) ds))))

;; num-op : (number number -> number) -> (EXPR-Value EXPR-Value -> EXPR-Value)
(define (num-op op x y)
  (numV (op (numV-n x) (numV-n y))))

(define (num+ x y) (num-op + x y))
(define (num- x y) (num-op - x y))

;; lookup : symbol -> EXPR-Value
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free variable")]
    [aSub (sub-name num rest-ds)
          (if (symbol=? sub-name name)
              num
              (lookup name rest-ds))]
    [aRecSub (sub-name val-box rest-ds)
             (if (symbol=? sub-name name)
                 (unbox val-box)
                 (lookup name rest-ds))]))

;; ----------------------------------------

;; get-type : symbol -> Type
(define (get-type name-to-find env)
  (type-case TypeEnv env
    [mtEnv () (error 'get-type "free variable, so no type")]
    [aBind (name ty rest)
           (if (symbol=? name-to-find name)
               ty
               (get-type name-to-find rest))]
    [tBind (name var1-name var1-type var2-name var2-type rest)
           (get-type name-to-find rest)]))

;; ----------------------------------------

;; parse-type : TE -> Type
(define (parse-type te)
  (type-case TE te
    [numTE () (numT)]
    [boolTE () (boolT)]
    [arrowTE (a b) (arrowT (map parse-type a)
                           (parse-type b))]
    [idTE (name) (idT name)]))

(define (type-error EXPR msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string EXPR)
                      (string-append " not "
                                     msg)))))

(define (type-error2 EXPR1 EXPR2 msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string EXPR1)
                      (string-append 
                       (to-string EXPR2)
                       (string-append " not "
                                     msg))))))

;;typecheck: EXPR TypeEnv -> Type
(define typecheck : (EXPR TypeEnv -> Type)
  (lambda (expr env)
    (type-case EXPR expr
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
      [equ (l r) (type-case Type (typecheck l env)
                  [numT () 
                        (type-case Type (typecheck r env)
                          [numT () (boolT)]
                          [else (type-error r "num")])]
                  [else (type-error l "num")])]
      [ifthenelse (i t e) (type-case Type (typecheck i env)
                            [boolT ()
                                   (local [(define t-type (typecheck t env))
                                           (define e-type (typecheck e env))]
                                     (cond ((equal? t-type e-type) t-type)
                                           (else (type-error2 e t "same type"))))]
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
                     (if (equal? param-types (typecheck-multiple args env))
                         result-type
                         (type-error args
                                     (to-string param-types)))]
             [else (type-error fn "function")])]
      [rec (name ty rhs-expr body-expr)
           (local [(define rhs-ty (parse-type ty))
                   (define new-env (aBind name rhs-ty env))]
             (if (equal? rhs-ty (typecheck rhs-expr new-env))
                 (typecheck body-expr new-env)
                 (type-error rhs-expr (to-string rhs-ty))))]
      [with-type (type-name var1-name var1-te var2-name var2-te body-expr)
                 (local [(define var1-ty (parse-type var1-te))
                         (define var2-ty (parse-type var2-te))
                         (define new-env (tBind type-name
                                                var1-name var1-ty
                                                var2-name var2-ty env))]
                   (begin
                     (validtype var1-ty new-env)
                     (validtype var2-ty new-env)
                     (typecheck body-expr
                                (aBind var1-name
                                       (arrowT (list var1-ty)
                                               (idT type-name))
                                       (aBind var2-name
                                              (arrowT (list var2-ty)
                                                      (idT type-name))
                                              new-env)))))]
      [cases (type-name dispatch-expr var1-name var1-id var1-rhs
                        var2-name var2-id var2-rhs)
        (local [(define bind (find-type-id type-name env))]
          (if (and (equal? var1-name (tBind-var1-name bind))
                   (equal? var2-name (tBind-var2-name bind)))
              (type-case Type (typecheck dispatch-expr env)
                [idT (name)
                     (if (equal? name type-name)
                         (local [(define rhs1-ty
                                   (typecheck var1-rhs
                                              (aBind var1-id (tBind-var1-type bind) env)))
                                 (define rhs2-ty
                                   (typecheck var2-rhs
                                              (aBind var2-id (tBind-var2-type bind) env)))]
                           (if (equal? rhs1-ty rhs2-ty)
                               rhs1-ty
                               (type-error var2-rhs (to-string rhs1-ty))))
                         (type-error dispatch-expr (to-string type-name)))]
                [else (type-error dispatch-expr (to-string type-name))])
              (type-error expr "matching variant names")))])))

;;parse-type-multiple: (listof TE) Type
(define (parse-type-multiple tes)
  (if (empty? tes) empty
      (append (list (parse-type (first tes))) (parse-type-multiple (rest tes)))))

;;aBind-multiple: (listof symbol) (listof Type) TypeEnv -> TypeEnv
(define (aBind-multiple names param-types env)
  (if (empty? names) env
      (aBind-multiple (rest names) (rest param-types) (aBind (first names) (first param-types) env))))

;;typecheck-multiple: (listof EXPR) TypeEnv -> (listof Type)
(define (typecheck-multiple args env)
  (if (empty? args) empty
      (append (list (typecheck (first args) env)) (typecheck-multiple (rest args) env))))

(define (validtype ty env)
  (type-case Type ty
    [numT () (mtEnv)]
    [boolT () (mtEnv)]
    [arrowT (a b) (begin (validtype-multiple a env)
                         (validtype b env))]
    [idT (id) (find-type-id id env)]))

(define (validtype-multiple ty env)
  (if (= (length ty) 1) (validtype (first ty) env)
      (begin (validtype (first ty) env)
             (validtype-multiple (rest ty) env))))

(define (find-type-id name-to-find env)
  (type-case TypeEnv env
    [mtEnv () (error 'get-type "free type name, so no type")]
    [aBind (name ty rest)
           (find-type-id name-to-find rest)]
    [tBind (name var1-name var1-ty var2-name var2-ty rest)
           (if (symbol=? name-to-find name)
               env
               (find-type-id name-to-find rest))]))

(define (interp expr ds)
  (eval expr ds (lambda (x) x)))

(test (typecheck (rec 'fib (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
                   (fun (list 'n 'a 'b) (list (numTE) (numTE) (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (id 'a)
                             (ifthenelse (equ (num 0) (sub (id 'n) (num 1)))
                                  (id 'b)
                                  (app (id 'fib) (list (sub (id 'n) (num 1)) (id 'b) (add (id 'a) (id 'b)))))))
                   (app (id 'fib) (list (num 7) (num 0) (num 1))))
                 (mtEnv)) (numT))
(test (interp (rec 'sum (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 0)
                             (add (id 'n) (app (id 'sum) (list (sub (id 'n) (num 1)))))))
                   (app (id 'sum) (list (num 5))))
                 (mtSub))
      (numV 15))
(test (interp (rec 'sum (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 0)
                             (add (id 'n) (app (id 'sum) (list (sub (id 'n) (num 1)))))))
                   (app (id 'sum) (list (num 10))))
                 (mtSub))
      (numV 55))



(test (interp (rec 'fib (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 1)
                             (ifthenelse (equ (num 0) (sub (id 'n) (num 1)))
                                  (num 1)
                                  (add (app (id 'fib) (list (sub (id 'n) (num 1))))
                                       (app (id 'fib) (list (sub (id 'n) (num 2))))))))
                   (app (id 'fib) (list (num 5))))
                 (mtSub)) (numV 8))
(test (interp (rec 'fib (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 1)
                             (ifthenelse (equ (num 0) (sub (id 'n) (num 1)))
                                  (num 1)
                                  (add (app (id 'fib) (list (sub (id 'n) (num 1))))
                                       (app (id 'fib) (list (sub (id 'n) (num 2))))))))
                   (app (id 'fib) (list (num 6))))
                 (mtSub)) (numV 13))
(test (typecheck (rec 'sum (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 0)
                             (add (id 'n) (app (id 'sum) (list (sub (id 'n) (num 1)))))))
                   (app (id 'sum) (list (num 10))))
                 (mtEnv))
      (numT))

(test (typecheck (rec 'fib (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 1)
                             (ifthenelse (equ (num 0) (sub (id 'n) (num 1)))
                                  (num 1)
                                  (add (app (id 'fib) (list (sub (id 'n) (num 1))))
                                       (app (id 'fib) (list (sub (id 'n) (num 2))))))))
                   (app (id 'fib) (list (num 5))))
                 (mtEnv)) (numT))
(test (typecheck (rec 'sum (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 0)
                             (add (id 'n) (app (id 'sum) (list (sub (id 'n) (num 1)))))))
                   (app (id 'sum) (list (num 10))))
                 (mtEnv))
      (numT))

(test (interp (rec 'sum (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 0)
                             (add (id 'n) (app (id 'sum) (list (sub (id 'n) (num 1)))))))
                   (app (id 'sum) (list (num 5))))
                 (mtSub))
      (numV 15))
(test (interp (rec 'sum (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 0)
                             (add (id 'n) (app (id 'sum) (list (sub (id 'n) (num 1)))))))
                   (app (id 'sum) (list (num 10))))
                 (mtSub))
      (numV 55))



(test (typecheck (rec 'fib (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 1)
                             (ifthenelse (equ (num 0) (sub (id 'n) (num 1)))
                                  (num 1)
                                  (add (app (id 'fib) (list (sub (id 'n) (num 1))))
                                       (app (id 'fib) (list (sub (id 'n) (num 2))))))))
                   (app (id 'fib) (list (num 5))))
                 (mtEnv)) (numT))

(test (interp (rec 'fib (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 1)
                             (ifthenelse (equ (num 0) (sub (id 'n) (num 1)))
                                  (num 1)
                                  (add (app (id 'fib) (list (sub (id 'n) (num 1))))
                                       (app (id 'fib) (list (sub (id 'n) (num 2))))))))
                   (app (id 'fib) (list (num 5))))
                 (mtSub)) (numV 8))
(test (interp (rec 'fib (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 1)
                             (ifthenelse (equ (num 0) (sub (id 'n) (num 1)))
                                  (num 1)
                                  (add (app (id 'fib) (list (sub (id 'n) (num 1))))
                                       (app (id 'fib) (list (sub (id 'n) (num 2))))))))
                   (app (id 'fib) (list (num 6))))
                 (mtSub)) (numV 13))



(test (interp (rec 'fib (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
                   (fun (list 'n 'a 'b) (list (numTE) (numTE) (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (id 'a)
                             (ifthenelse (equ (num 0) (sub (id 'n) (num 1)))
                                  (id 'b)
                                  (app (id 'fib) (list (sub (id 'n) (num 1)) (id 'b) (add (id 'a) (id 'b)))))))
                   (app (id 'fib) (list (num 5) (num 0) (num 1))))
                 (mtSub)) (numV 5))
(test (interp (rec 'fib (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
                   (fun (list 'n 'a 'b) (list (numTE) (numTE) (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (id 'a)
                             (ifthenelse (equ (num 0) (sub (id 'n) (num 1)))
                                  (id 'b)
                                  (app (id 'fib) (list (sub (id 'n) (num 1)) (id 'b) (add (id 'a) (id 'b)))))))
                   (app (id 'fib) (list (num 7) (num 0) (num 1))))
                 (mtSub)) (numV 13))


(test (typecheck (rec 'fib (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
                   (fun (list 'n 'a 'b) (list (numTE) (numTE) (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (id 'a)
                             (ifthenelse (equ (num 0) (sub (id 'n) (num 1)))
                                  (id 'b)
                                  (app (id 'fib) (list (sub (id 'n) (num 1)) (id 'b) (add (id 'a) (id 'b)))))))
                   (app (id 'fib) (list (num 7) (num 0) (num 1))))
                 (mtEnv)) (numT))

(test (typecheck
  (with-type 'outer
    'outerA (numTE)
    'outerB (numTE)
    (with-type 'inner
      'innerA (numTE)
        'innerB (arrowTE (list (numTE)) (numTE))
	    (app
		  (fun (list 'o 'i) (list (idTE 'outer) (idTE 'inner))
	           (cases 'outer (id 'o)
	   	  	   'outerA 'x
			    (cases 'inner (id 'i)
	    	   	        'innerA 'y (id 'x)
			         'innerB 'y (app (id 'y) (list (id 'x)))
	 	        )
			   'outerB 'x (id 'x)))
	   	     (list (app (id 'outerA) (list (num 5))) (app (id 'innerA) (list (num 10))))
     	   )))
  (mtEnv)) (numT))

(test (interp
  (with-type 'outer
    'outerA (numTE)
    'outerB (numTE)
    (with-type 'inner
      'innerA (numTE)
        'innerB (arrowTE (list (numTE)) (numTE))
	    (app
		  (fun (list 'o 'i) (list (idTE 'outer) (idTE 'inner))
	           (cases 'outer (id 'o)
	   	  	   'outerA 'x
			    (cases 'inner (id 'i)
	    	   	        'innerA 'y (id 'x)
			         'innerB 'y (app (id 'y) (list (id 'x)))
	 	        )
		   'outerB 'x (id 'x)))
   	     (list (app (id 'outerA) (list (num 5))) (app (id 'innerA) (list (num 10))))
     	   )))
  (mtSub)) (numV 5))

(test (interp
  (with-type 'outer
    'outerA (numTE)
    'outerB (numTE)
    (with-type 'inner
      'innerA (numTE)
        'innerB (arrowTE (list (numTE)) (numTE))
	    (app
		  (fun (list 'o 'i) (list (idTE 'outer) (idTE 'inner))
	           (cases 'outer (id 'o)
	   	  	   'outerA 'x
			    (cases 'inner (id 'i)
	    	   	        'innerA 'y (id 'x)
			         'innerB 'y (app (id 'y) (list (id 'x)))
	 	        )
	   'outerB 'x (id 'x)))
     (list (app (id 'outerA) (list (num 5))) (app (id 'innerB) (list (fun (list 'a) (list (numTE)) (sub (num 0) (id 'a))))))
   )))
  (mtSub)) (numV -5))

(test (typecheck
  (with-type 'outer
    'outerA (numTE)
    'outerB (numTE)
    (with-type 'inner
      'innerA (numTE)
        'innerB (arrowTE (list (numTE)) (numTE))
	    (app
		  (fun (list 'o 'i) (list (idTE 'outer) (idTE 'inner))
	           (cases 'outer (id 'o)
	   	  	   'outerA 'x
			    (cases 'inner (id 'i)
	    	   	        'innerA 'y (id 'x)
			         'innerB 'y (app (id 'y) (list (id 'x)))
		 	        )
	   'outerB 'x (id 'x)))
    (list (app (id 'outerA) (list (num 5))) (app (id 'innerA) (list (num 10))))
  )))
  (mtEnv)) (numT))

(test (interp
  (with-type 'outer
    'outerA (numTE)
    'outerB (numTE)
    (with-type 'inner
      'innerA (numTE)
        'innerB (arrowTE (list (numTE)) (numTE))
	    (app
		  (fun (list 'o 'i) (list (idTE 'outer) (idTE 'inner))
	           (cases 'outer (id 'o)
	   	  	   'outerA 'x
			    (cases 'inner (id 'i)
	    	   	        'innerA 'y (id 'x)
			         'innerB 'y (app (id 'y) (list (id 'x)))
		 	        )
	   'outerB 'x (id 'x)))
     (list (app (id 'outerA) (list (num 5))) (app (id 'innerA) (list (num 10))))
  )))
  (mtSub)) (numV 5))

(test (interp
  (with-type 'outer
    'outerA (numTE)
    'outerB (numTE)
    (with-type 'inner
      'innerA (numTE)
        'innerB (arrowTE (list (numTE)) (numTE))
	    (app
		  (fun (list 'o 'i) (list (idTE 'outer) (idTE 'inner))
	           (cases 'outer (id 'o)
	   	  	   'outerA 'x
			    (cases 'inner (id 'i)
	    	   	        'innerA 'y (id 'x)
			         'innerB 'y (app (id 'y) (list (id 'x)))
 	        )
	   'outerB 'x (id 'x)))
    (list (app (id 'outerA) (list (num 5))) (app (id 'innerB) (list (fun (list 'a) (list (numTE)) (sub (num 0) (id 'a))))))
  )))
  (mtSub)) (numV -5))

(test (typecheck
  (with-type 'ops
    'adder (arrowTE (list (numTE) (numTE)) (numTE))
    'ifer (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
    (app
       (fun (list 'f 'a 'b 'c) (list (idTE 'ops) (numTE) (numTE) (numTE))
          (cases 'ops (id 'f)
            'adder 'x (add (app (id 'x) (list (id 'a) (id 'b))) (id 'c))
            'ifer 'x (app (id 'x) (list (id 'a) (id 'b) (id 'c)))))
       (list (app (id 'adder) (list (fun (list 'a 'b) (list (numTE) (numTE)) (add (id 'a) (id 'b))))) (num 4) (num 5) (num 6))))
  (mtEnv)) (numT))

(test (typecheck
  (with-type 'ops
    'adder (arrowTE (list (numTE) (numTE)) (numTE))
    'ifer (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
    (app
       (fun (list 'f 'a 'b 'c) (list (idTE 'ops) (numTE) (numTE) (numTE))
          (cases 'ops (id 'f)
            'adder 'x (add (app (id 'x) (list (id 'a) (id 'b))) (id 'c))
            'ifer 'x (app (id 'x) (list (id 'a) (id 'b) (id 'c)))))
       (list (app (id 'ifer) (list (fun (list 'a 'b 'c) (list (numTE) (numTE) (numTE))
        (ifthenelse (equ (num 0) (id 'a))
         (id 'b) (id 'c))))) (num 0) (num 2) (num 4))))
  (mtEnv)) (numT))


(test (interp
  (with-type 'ops
    'adder (arrowTE (list (numTE) (numTE)) (numTE))
    'ifer (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
    (app
       (fun (list 'f 'a 'b 'c) (list (idTE 'ops) (numTE) (numTE) (numTE))
          (cases 'ops (id 'f)
            'adder 'x (add (app (id 'x) (list (id 'a) (id 'b))) (id 'c))
            'ifer 'x (app (id 'x) (list (id 'a) (id 'b) (id 'c)))))
       (list (app (id 'adder) (list (fun (list 'a 'b) (list (numTE) (numTE)) (add (id 'a) (id 'b))))) (num 4) (num 5) (num 6))))
  (mtSub)) (numV 15))

(test (interp
  (with-type 'ops
    'adder (arrowTE (list (numTE) (numTE)) (numTE))
    'ifer (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
    (app
       (fun (list 'f 'a 'b 'c) (list (idTE 'ops) (numTE) (numTE) (numTE))
          (cases 'ops (id 'f)
            'adder 'x (add (app (id 'x) (list (id 'a) (id 'b))) (id 'c))
            'ifer 'x (app (id 'x) (list (id 'a) (id 'b) (id 'c)))))
       (list (app (id 'ifer) (list (fun (list 'a 'b 'c) (list (numTE) (numTE) (numTE))
       (ifthenelse (equ (num 0) (id 'a))
        (id 'b) (id 'c))))) (num 0) (num 2) (num 4))))
  (mtSub)) (numV 2))

(test (interp
  (with-type 'ops
    'adder (arrowTE (list (numTE) (numTE)) (numTE))
    'ifer (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
    (app
       (fun (list 'f 'a 'b 'c) (list (idTE 'ops) (numTE) (numTE) (numTE))
          (cases 'ops (id 'f)
            'adder 'x (add (app (id 'x) (list (id 'a) (id 'b))) (id 'c))
            'ifer 'x (app (id 'x) (list (id 'a) (id 'b) (id 'c)))))
       (list (app (id 'ifer) (list (fun (list 'a 'b 'c) (list (numTE) (numTE) (numTE))
        (ifthenelse (equ (num 0) (id 'a)) (id 'b) (id 'c))))) (num 1) (num 3) (num 5))))
  (mtSub)) (numV 5))
(test (typecheck
  (with-type 'pet
    'cat (numTE)
    'dog (numTE)
    (app
      (fun (list 'a) (list (idTE 'pet))
        (cases 'pet (id 'a)
          'cat 'x (sub (num 0) (id 'x))
          'dog 'x (id 'x)))
      (list (app (id 'cat) (list(num 5))))))
  (mtEnv)) (numT))

(test (interp
  (with-type 'pet
    'cat (numTE)
    'dog (numTE)
    (app
      (fun (list 'a) (list (idTE 'pet))
        (cases 'pet (id 'a)
          'cat 'x (sub (num 0) (id 'x))
          'dog 'x (id 'x)))
      (list (app (id 'cat) (list(num 5))))))
  (mtSub)) (numV -5))

(test (typecheck
  (with-type 'ops
    'adder (arrowTE (list (numTE) (numTE)) (numTE))
    'ifer (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
    (app
       (fun (list 'f 'a 'b 'c) (list (idTE 'ops) (numTE) (numTE) (numTE))
          (cases 'ops (id 'f)
            'adder 'x (add (app (id 'x) (list (id 'a) (id 'b))) (id 'c))
            'ifer 'x (app (id 'x) (list (id 'a) (id 'b) (id 'c)))))
       (list (app (id 'adder) (list (fun (list 'a 'b) (list (numTE) (numTE)) (add (id 'a) (id 'b))))) (num 4) (num 5) (num 6))))
  (mtEnv)) (numT))

(test (typecheck
  (with-type 'ops
    'adder (arrowTE (list (numTE) (numTE)) (numTE))
    'ifer (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
    (app
       (fun (list 'f 'a 'b 'c) (list (idTE 'ops) (numTE) (numTE) (numTE))
          (cases 'ops (id 'f)
            'adder 'x (add (app (id 'x) (list (id 'a) (id 'b))) (id 'c))
            'ifer 'x (app (id 'x) (list (id 'a) (id 'b) (id 'c)))))
       (list (app (id 'ifer) (list (fun (list 'a 'b 'c) (list (numTE) (numTE) (numTE))
       (ifthenelse (equ (num 0) (id 'a)) (id 'b) (id 'c))))) (num 0) (num 2) (num 4))))
  (mtEnv)) (numT))

(test (interp
  (with-type 'ops
    'adder (arrowTE (list (numTE) (numTE)) (numTE))
    'ifer (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
    (app
       (fun (list 'f 'a 'b 'c) (list (idTE 'ops) (numTE) (numTE) (numTE))
          (cases 'ops (id 'f)
            'adder 'x (add (app (id 'x) (list (id 'a) (id 'b))) (id 'c))
            'ifer 'x (app (id 'x) (list (id 'a) (id 'b) (id 'c)))))
       (list (app (id 'adder) (list (fun (list 'a 'b) (list (numTE) (numTE)) (add (id 'a) (id 'b))))) (num 4) (num 5) (num 6))))
  (mtSub)) (numV 15))

(test (interp
  (with-type 'ops
    'adder (arrowTE (list (numTE) (numTE)) (numTE))
    'ifer (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
    (app
       (fun (list 'f 'a 'b 'c) (list (idTE 'ops) (numTE) (numTE) (numTE))
          (cases 'ops (id 'f)
            'adder 'x (add (app (id 'x) (list (id 'a) (id 'b))) (id 'c))
            'ifer 'x (app (id 'x) (list (id 'a) (id 'b) (id 'c)))))
       (list (app (id 'ifer) (list (fun (list 'a 'b 'c) (list (numTE) (numTE) (numTE))
       (ifthenelse (equ (num 0) (id 'a)) (id 'b) (id 'c))))) (num 0) (num 2) (num 4))))
  (mtSub)) (numV 2))

(test (interp
  (with-type 'ops
    'adder (arrowTE (list (numTE) (numTE)) (numTE))
    'ifer (arrowTE (list (numTE) (numTE) (numTE)) (numTE))
    (app
       (fun (list 'f 'a 'b 'c) (list (idTE 'ops) (numTE) (numTE) (numTE))
          (cases 'ops (id 'f)
            'adder 'x (add (app (id 'x) (list (id 'a) (id 'b))) (id 'c))
            'ifer 'x (app (id 'x) (list (id 'a) (id 'b) (id 'c)))))
       (list (app (id 'ifer) (list (fun (list 'a 'b 'c) (list (numTE) (numTE) (numTE))
       (ifthenelse (equ (num 0) (id 'a)) (id 'b) (id 'c))))) (num 1) (num 3) (num 5))))
  (mtSub)) (numV 5))
(test
 (interp
  (with-type 'fruit
             'apple (numTE)
             'banana (arrowTE (list (numTE)) (numTE))
             (rec 'len
               (arrowTE (list (numTE)) (numTE))
               (fun (list 'l)
                    (list (idTE 'fruit))
                    (cases 'fruit (id 'l)
                      'apple 'a (num 0)
                      'banana 'n (num 3)))
               (id 'banana)))
  (mtSub))
 (constructorV #t))



(test
 (interp
  (with-type 'fruit
             'apple (numTE)
             'banana (arrowTE (list (numTE)) (numTE))
             (rec 'len
               (arrowTE (list (numTE)) (numTE))
               (fun (list 'l)
                    (list (idTE 'fruit))
                    (cases 'fruit (id 'l)
                      'apple 'a (num 0)
                      'banana 'n (num 3)))
               (app (id 'apple) (list (num 2)))))
  (mtSub))
 (variantV #f (numV 2)))

(test
 (interp
  (rec 'a
    (arrowTE (list (numTE)) (numTE))
    (fun (list 'l)
         (list (numTE))
         (num 1))
    (rec 'b
      (arrowTE (list (numTE)) (numTE))
      (fun (list 'c)
           (list (numTE))
           (num 2))
      (app (id 'b) (list (num 10)))))
  (mtSub))
 (numV 2))




(test
 (interp
  (with-type 'fruit
             'apple (numTE)
             'banana (arrowTE (list (numTE)) (numTE))
             (rec 'len
               (arrowTE (list (numTE)) (numTE))
               (fun (list 'l)
                    (list (idTE 'fruit))
                    (cases 'fruit (id 'l)
                      'apple 'a (num 0)
                      'banana 'n (num 3)))
               (app (id 'banana) (list (num 2)))))
  (mtSub))
 (variantV #t (numV 2)))

(test (interp (with-type 'fruit 'apple (numTE)
                         'banana (arrowTE (list (numTE)) (numTE))
                         (app (id 'apple) (list (num 10))))
              (mtSub))
      (variantV false (numV 10)))

(test (interp (with-type 'fruit 'apple (numTE)
                         'banana (arrowTE (list (numTE)) (numTE))
                         (cases 'fruit (app (id 'apple) (list (num 10)))
                           'apple 'y (add (id 'y) (num 1))
                           'banana 'x (app (id 'x) (list (num 10)))))
              (mtSub))
      (numV 11))

(test (interp (with-type 'fruit 'apple (numTE)
                         'banana (arrowTE (list (numTE)) (numTE))
                         (cases 'fruit (app (id 'banana) (list (fun (list 'x) (list (numTE)) (num 3))))
                           'apple 'y (add (id 'y) (num 1))
                           'banana 'x (app (id 'x) (list (num 10)))))
              (mtSub))
      (numV 3))

(test (typecheck (with-type 'fruit 'apple (numTE)
                            'banana (arrowTE (list (numTE)) (numTE))
                            (app (id 'apple) (list (num 10))))
                 (mtEnv))
      (idT 'fruit))

(test (typecheck (with-type 'fruit 'apple (numTE)
                            'banana (arrowTE (list (numTE)) (numTE))
                            (fun (list 'x) (list (idTE 'fruit)) (num 10)))
                 (mtEnv))
      (arrowT (list (idT 'fruit)) (numT)))


(test (interp (ifthenelse (equ (num 0) (num 2)) (num 3) (num 1)) (mtSub)) (numV 1))
(test (interp (ifthenelse (equ (num 0) (num 0)) (num 3) (num 1)) (mtSub)) (numV 3))

(test (typecheck (ifthenelse (equ (num 0) (num 2)) (num 3) (num 1)) (mtEnv)) (numT))

(test (typecheck (with-type 'fruit 'apple (numTE)
                         'banana (arrowTE (list (numTE)) (numTE))
                         (cases 'fruit (app (id 'banana) (list (fun (list 'x) (list (numTE)) (num 3))))
                           'apple 'y (add (id 'y) (num 1))
                           'banana 'x (app (id 'x) (list (num 10)))))
              (mtEnv))
      (numT))

(test (interp (with-type 'fruit 'apple (numTE) 'banana (numTE)
                 (cases 'fruit (app (id 'apple) (list (num 42)))
                   'apple 'x (add (id 'x)(num 1))
                   'banana 'y (add (id 'y)(num 2)))) (mtSub)) (numV 43))
(test (interp (with-type 'fruit 'apple (numTE) 'banana (numTE)
                 (cases 'fruit (app (id 'banana) (list (num 42)))
                   'apple 'x (add (id 'x)(num 1))
                   'banana 'y (add (id 'y)(num 2)))) (mtSub)) (numV 44))
(test (typecheck (with-type 'fruit 'apple (numTE) 'banana (numTE)
                 (cases 'fruit (app (id 'apple) (list (num 42)))
                   'apple 'x (add (id 'x)(num 1))
                   'banana 'y (add (id 'y)(num 2)))) (mtEnv)) (numT))
(test (typecheck (with-type 'fruit 'apple (numTE) 'banana (numTE)
                 (cases 'fruit (app (id 'banana) (list (num 42)))
                   'apple 'x (add (id 'x)(num 1))
                   'banana 'y (add (id 'y)(num 2)))) (mtEnv)) (numT))
(test (parse-type (idTE 'x)) (idT 'x))

(test (interp (with-type 'fruit 'apple (numTE)
                         'banana (arrowTE (list (numTE)) (numTE))
                         (app (id 'apple) (list (num 10))))
              (mtSub))
      (variantV false (numV 10)))

(test (interp (with-type 'fruit 'apple (numTE)
                         'banana (arrowTE (list (numTE)) (numTE))
                         (cases 'fruit (app (id 'apple) (list (num 10)))
                           'apple 'y (add (id 'y) (num 1))
                           'banana 'x (app (id 'x) (list (num 10)))))
              (mtSub))
      (numV 11))

(test (typecheck (with-type 'fruit 'apple (numTE)
                            'banana (arrowTE (list (numTE)) (numTE))
                            (app (id 'apple) (list (num 10))))
                 (mtEnv))
      (idT 'fruit))

(test (typecheck (with-type 'fruit 'apple (numTE)
                            'banana (arrowTE (list (numTE)) (numTE))
                            (fun (list 'x) (list (idTE 'fruit)) (num 10)))
                 (mtEnv))
      (arrowT (list (idT 'fruit)) (numT)))

(test (typecheck (rec 'sum (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 0)
                             (add (id 'n) (app (id 'sum) (list (sub (id 'n) (num 1)))))))
                   (app (id 'sum) (list (num 10))))
                 (mtEnv))
      (numT))

(test (typecheck (rec 'fib (arrowTE (list (numTE)) (numTE))
                   (fun (list 'n) (list (numTE))
                        (ifthenelse (equ (num 0) (id 'n))
                             (num 1)
                             (ifthenelse (equ (num 0) (sub (id 'n) (num 1)))
                                  (num 1)
                                  (add (app (id 'fib) (list (sub (id 'n) (num 1))))
                                       (app (id 'fib) (list (sub (id 'n) (num 2))))))))
                   (app (id 'fib) (list (num 5))))
                 (mtEnv)) (numT))

(test (typecheck (rec 'f (arrowTE (list (numTE)) (numTE))
                    (id 'f) 
                    (app (id 'f) (list (num 10)))) (mtEnv)) (numT))

(test/exn (interp (rec 'f (arrowTE (list (numTE)) (numTE)) 
                    (id 'f) 
                    (app (id 'f) (list (num 10)))) (mtSub)) "eval: not a function")