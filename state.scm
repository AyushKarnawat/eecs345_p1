; This file is where the state is stored
; Any function in this file has to do with reading or writing from the state
; In order to get the state, functions just need to ask for S

; The state is just a list of (name value) pairs for variables.
; i.e. The state S is represented by ((name value) (name value) ... (name value)).
(define S '())

; In order to reset the state, functions will need to use setState. This should only be used by functions in this file.
(define setState
  (lambda (x)
    (set! S x)))

; This function adds a single variable (a (name value) pair) to the state
; If that variable is already in the state, it removes the old pair and adds the new one
(define addVar
  (lambda (n v)
    (setState (assembleState (removeVar n S) ; reassemble the state using the new state after removing the variable
                             (cons (cons n (cons v '())) '()) )))) ; as well as the new variable pair

; This function is a recursive function that looks through the current state s 
; and removes the first instance of a variable with the name n.
(define removeVar
  (lambda (n s)
    (cond
      ((null? s) '())
      ((eq? n (car (car s))) (cdr s))
      (else (cons (car s) (removeVar n (cdr s)))) )))

;This is a helper function used by addVar to reassemble the state with the new variable pair
(define assembleState
  (lambda (s v) ;The new state and the new variable pair
    (cond
      ((null? s) v) ;If s is empty, return the new variable pair to append to the end
      (else (cons (car s) (assembleState (cdr s) v))) )))