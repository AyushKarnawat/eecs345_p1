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