; Team members: Steven Leonis, Steven Xie, Ayush Karnawat

; This file is where the state is stored.
; All the functions in this file have to do with reading or writing from the state.
; In order to get the state, functions just need to ask for S.

; The state S is a list of (name value) pairs for variables.
(define S '())

; In order to reset the state, functions will need to use setState. 
; This should only be used by functions in this file.
; 
; params:
;   x: the new value (in this case a list) to set the state S to
(define setState
  (lambda (x)
    (set! S x)))

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
      ((eq? n (car (car s))) #t)  ; if var found, return true
      (else (recursiveDeclared? n (cdr s)) ) )))

; Adds a single variable (a (name value) pair) to the state.
; If that variable is already in the state, it removes the old pair and adds the new one.
; 
; params:
;   n: name of variable
;   v: value assigned to that variable
(define addVar
  (lambda (n v)
    (setState (assembleState (removeVar n S) ; reassemble the state using the new state after removing the variable
                             (cons (cons n (cons v '())) '()) )))) ; as well as the new variable pair

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
      ((eq? n (car (car s))) (cdr s))
      (else (cons (car s) (removeVar n (cdr s)))) )))

; Helper function used by addVar to reassemble the state with the new (name value) pair.
; 
; params:
;   s: new state
;   v: new (name value) pair
(define assembleState
  (lambda (s v)
    (cond
      ((null? s) v) ; If s is empty, return the new variable pair to append to the end
      (else (cons (car s) (assembleState (cdr s) v))) )))

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
      ((eq? (car l) '+) (+ (evalExp (cadr l)) (evalExp (caddr l))))
      ((eq? (car l) '-) (if (null? (cddr l)) (* (evalExp (cadr l)) -1)
                                             (- (evalExp (cadr l)) (evalExp (caddr l))) ))
      ((eq? (car l) '*) (* (evalExp (cadr l)) (evalExp (caddr l))))
      ((eq? (car l) '/) (quotient (evalExp (cadr l)) (evalExp (caddr l))))
      ((eq? (car l) '%) (modulo (evalExp (cadr l)) (evalExp (caddr l))))
      ((eq? (car l) '==) (eq? (evalExp (cadr l)) (evalExp (caddr l))))
      ((eq? (car l) '!=) (not (eq? (evalExp (cadr l)) (evalExp (caddr l)))))
      ((eq? (car l) '<) (< (evalExp (cadr l)) (evalExp (caddr l))))
      ((eq? (car l) '>) (> (evalExp (cadr l)) (evalExp (caddr l))))
      ((eq? (car l) '<=) (<= (evalExp (cadr l)) (evalExp (caddr l))))
      ((eq? (car l) '>=) (>= (evalExp (cadr l)) (evalExp (caddr l))))
      ((eq? (car l) '&&) (and (evalExp (cadr l)) (evalExp (caddr l))))
      ((eq? (car l) '||) (or (evalExp (cadr l)) (evalExp (caddr l))))
      ((eq? (car l) '!) (not (evalExp (cadr l))))
      (else (if (declared? (car l)) (valueOf (car l) S) (error ("Variable was not declared!")))))))

; A recursive function that finds the value (int or bool) of a declared variable in the state

(define valueOf
  (lambda (x l)
    (cond
      ((null? l) '())
      ((eq? (caar l) x) (cadar l))
      (else (valueOf x (cdr l))) )))

; Used under MIT License The Little Schemer. All Rights Reserved
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; End Copyright