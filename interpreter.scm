; The main function, it gets the parse tree and divides up the statements
; which are then read in sequential order
; 
; Functions in this file can only access the state through other functions in states.scm

(load "simpleParser.scm")
(load "state.scm")

(define interpret
  (lambda (l)
    ; Parse the given filename into a tree
    (cond
      ((null? (parser l)) '()) ;assuming l is the filename
      (else (runStatementList (parser l)) ))))

; This function runs the entire statement tree recursively by using runStatement multiple times
; l should be the parsed tree in list format i.e ((var x) (= x 10)...)
(define runStatementList
  (lambda (l)
    (cond 
      ((null? l) '())
      ((null? (cdr l)) (runStatement (car l))) ; if there is the only statement in the list to be evaluated, do so
      (else (cons (runStatement (car l))       ; call runStatement on the first list  
                  (runStatementList (cdr l)) )))))  ;and on the rest of the list
      
; Runs the line it is given
; If run statements finds a return, it returns the proper value
(define runStatement
  (lambda (l)
    (cond
      ((null? l) '())
      ((eq? (car l) 'var) declareVar(cdr l))    ; call variable declaration on rest of the statement
      ((eq? (car l) '=) assign(cdr l))          ; call assignment on rest of statement
      ((eq? (car l) 'return) return(cdr l))     ; call return on rest of statement
      ((eq? (car l) 'if) conditional(cdr l))    ; call if on rest of statement
      ((eq? (car l) 'while) loop(cdr l))        ; call while on rest of statement
      (else
       (error "Not a valid statement!")) )))

; Function that takes a variable declaration statement and executes it
(define declareVar
  (lambda (l)
    (cond
      ((null? (cdr l)) (addVar (car l) '())) ; if the only given statement is the variable name, assign its value to an empty list
      (else (addVar (car l) (car (car l)))) ))) ; if an variable name and its value are given
      
(define assign
 (lambda (l)
  (cond
    ((null? ())) )))
  
(define return
  (lambda (l)
    (cond
      () )))
    
(define conditional
  (lambda (l)
    (cond
      () )))

(define loop
  (lambda (l)
    (cond
      () )))