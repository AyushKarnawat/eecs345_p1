;The main function, it gets the parse tree and divides up the statements
; which are then read in sequential order
;Functions in this file can only access the state through other functions in states.scm
(load "simpleParser.scm")
(load "state.scm")

(define S '())

;In order to reset the state, functions will need to use setState. This should only be used by functions in this file.
(define setState
  (lambda (x)
    (set! S x)))

(define addVar
  (lambda (n v)
    (setState (assembleState (removeVar n S) ; reassemble the state using the new state after removing the variable
                             (cons (cons n (cons v '())) '()) )))) ; as well as the new variable pair

;This function is a recursive function that looks through the current state s 
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
    
(define declareVar
  (lambda (l)
    (cond
      ((null? (cdr l)) (addVar (car l) '())) ;
      ; consider cases of assignment coming after var
      (else (addVar (car l) (car (cdr l)))) )))


(define interpret
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (runStatement (car l) )))))

; Runs the line it is given.
; if run statements finds a return, it prints(return?) the proper value.
(define runStatement
  (lambda (l)
    (cond
      ((null? l) '())
      ((eq? (car l) 'var) declareVar(cdr l)); call varaible declaration on rest of the statement
      ((eq? (car l) '=) assign(cdr l));       call assignment on rest of statement
      ((eq? (car l) 'return) return(cdr l));  call return on rest of statement
      ((eq? (car l) 'if) conditional(cdr l)); call if on rest of statement
      ((eq? (car l) 'while) loop(cdr l));     call while on rest of statement
      (else
       (error 'Not a valid statement!)) )))