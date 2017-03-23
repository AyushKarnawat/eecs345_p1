; Team members: Steven Leonis, Steven Xie, Ayush Karnawat

; This file is where the state is stored.
; All the functions in this file have to do with reading or writing from the state.
; In order to get the state, functions just need to ask for S.

; Redefining basic scheme keywords (car , cdr, cadr, cons, etc..) for abstraction
(define next car)
(define combine cons)
;(define rest cdr)   These rewordings are default behavior in PrettyBig
;(define second cadr)
;(define third caddr)

; The state S is a list of (name value) pairs for variables.
(define S '(()))

; In order to reset the state, functions will need to use setState. 
; This should only be used by functions in this file.
; 
; params:
;   x: the new value (in this case a list) to set the state S to
(define setState
  (lambda (x)
    (set! S x)))

(define addLayer
  (lambda ()
    (setState (combine '() S)) ))

(define removeLayer
  (lambda ()
    (setState (rest S))))


; Function declared looks through state S and returns true if variable is declared.
;
; params:
;   n: name of variable
(define declared?
  (lambda (n)
    (recursiveDeclared? n S) ))

; Helper function that allows outside functions to know if a variable is declared.
;
; params:
;   n: name of variable 
;   s: current state
(define recursiveDeclared?
  (lambda (n s)
    (cond
      ((null? s) #f)              ; base case: var not in state
      ((null? (next s)) (recursiveDeclared? n (rest s)))
      ((eq? n (next (next s))) #t)  ; if var found, return true
      ((list? (next (next s))) (or (recursiveDeclared? n (next s)) (recursiveDeclared? n (rest s))))
      (else (recursiveDeclared? n (rest s)) ) )))

; Adds a single variable (a (name value) pair) to the highest layer.
; Only use this function if the variable hasn't already been declared anywhere
;
; params:
;   n: name of variable
;   v: value assigned to that variable
(define addVar
  (lambda (n v)
    (setState (combine (assembleState (next S) (combine (combine n (combine v '())) '())) ; create the new layer
                       (rest S))) )) ; and combine it with the rest of the layers

; Use this function to change the value of a variable that already exists

(define changeVar
  (lambda (n v)
    (changeVar-cps n v S (lambda (v) (setState v)))))

; A helper function used by changeVar to recursively search through the state
; We use continuation passing style              
(define changeVar-cps
  (lambda (n v s return)
    (cond
      ((null? s) (return '()))
      ((null? (next s)) (changeVar-cps n v (rest s) (lambda (v) (return (combine (next s) v)))))
      ((eq? (next (next s)) n) (return (assembleState (removeVar n s)
                                                       (combine (combine n (combine v '())) '()))))
      ((list? (next (next s))) (changeVar-cps n v (next s)
                                              (lambda (v1) (changeVar-cps n v (rest s)
                                                                          (lambda (v2) (return (combine v1 v2)))))))
      (else (changeVar-cps n v (rest s) (lambda (v) (return (combine (next s) v))))) )))
      
               
(define return-cont
  (lambda (v)
    v))

; This function is a recursive function that looks through the current state s 
; and removes the first instance of a variable with the name n.
; 
; params:
;   n: variable with name n (i.e. 'a)
;   s: current state
(define removeVar
  (lambda (n s)
    (cond
      ((null? s) '())
      ((eq? n (next (next s))) (rest s))
      (else (combine (next s) (removeVar n (rest s)))) )))

; Helper function used by addVar to reassemble the state with the new (name value) pair.
; 
; params:
;   s: new layer of the state
;   v: new (name value) pair
(define assembleState
  (lambda (s v)
    (cond
      ((null? s) v) ; If s is empty, return the new variable pair to append to the end
      (else (combine (next s) (assembleState (rest s) v))) )))

; This function evaluates an expression recursively, dealing with both numerical and boolean operators
; In this way, it is performing both M_value and M_boolean operations
(define evalExp
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? l) l)
      ((eq? l 'true) #t)
      ((eq? l 'false) #f)
      ((atom? l) (if (declared? l) (if (not (null? (valueOf l S))) (valueOf l S)
                                                                   (error "Variable is not assigned!"))
                                   (error "Variable was not declared")))
      ((eq? (car l) '+) (+ (evalExp (second l)) (evalExp (third l))))
      ((eq? (car l) '-) (if (null? (rest (rest l))) (* (evalExp (second l)) -1)
                                                    (- (evalExp (second l)) (evalExp (third l))) ))
      ((eq? (car l) '*) (* (evalExp (second l)) (evalExp (third l))))
      ((eq? (car l) '/) (quotient (evalExp (second l)) (evalExp (third l))))
      ((eq? (car l) '%) (modulo (evalExp (second l)) (evalExp (third l))))
      ((eq? (car l) '==) (eq? (evalExp (second l)) (evalExp (third l))))
      ((eq? (car l) '!=) (not (eq? (evalExp (second l)) (evalExp (third l)))))
      ((eq? (car l) '<) (< (evalExp (second l)) (evalExp (third l))))
      ((eq? (car l) '>) (> (evalExp (second l)) (evalExp (third l))))
      ((eq? (car l) '<=) (<= (evalExp (second l)) (evalExp (third l))))
      ((eq? (car l) '>=) (>= (evalExp (second l)) (evalExp (third l))))
      ((eq? (car l) '&&) (and (evalExp (second l)) (evalExp (third l))))
      ((eq? (car l) '||) (or (evalExp (second l)) (evalExp (third l))))
      ((eq? (car l) '!) (not (evalExp (second l))))
      (else (if (declared? (next l)) (valueOf (next l) S) (error ("Variable was not declared!")))))))

; A recursive function that finds the value (int or bool) of a declared variable in the state
(define valueOf
  (lambda (x l)
    (cond
      ((null? l) '())
      ((null? (next l)) (valueOf x (rest l)))
      ((eq? (next (next l)) x) (second (first l)))
      ((list? (next (next l))) (if (null? (valueOf x (next l))) (valueOf x (rest l)) (valueOf x (next l)))) 
      (else (valueOf x (rest l))) )))


; Used under MIT License The Little Schemer. All Rights Reserved
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; End Copyright
