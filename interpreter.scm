; The main function, it gets the parse tree and divides up the statements
; which are then read in sequential order.
; Functions in this file can only access the state through other functions in states.scm.


(load "simpleParser.scm")
(load "state.scm")

; Parses the given filename into a tree (through the parser function provided), 
; and pass the parse tree onto the runStatementList so that it can be evaluated.
; 
; params:
;   file: the name of the file to interpret and execute. 
(define interpret
  (lambda (file)
    (cond
      ((null? (parser file)) '())
      (else (runStatementList (parser file))) )))

; Runs the entire statement tree recursively by using runStatement multiple times.
; 
; params:
;   l: the parsed tree in list format i.e ((var x) (= x 10) ... (return x))
(define runStatementList
  (lambda (l)
    (cond 
      ((null? l) '())
      ((null? (cdr l)) (runStatement (car l)))      ; if there is the only one statement in the list to be evaluated
      (else (cons (runStatement (car l))            ; call runStatement on the first list  
                  (runStatementList (cdr l)) )))))  ; and on the rest of the list
      
; Runs the given statement by calling the necessary functions.
; When the function finds a return statement, it returns the value of the given variable name.
; 
; params:
;   l: statements (i.e. (var x), (= x 10), (return x) ...)
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

; Function that takes a variable declaration statement and executes it.
; 
; params:
;   l: variable declaration statement (i.e. )
(define declareVar
  (lambda (l)
    (cond
      ((null? (cdr l)) (addVar (car l) '()))    ; if the only given statement is the variable name, assign its value to an empty list
      (else (addVar (car l) (car (car l)))) ))) ; if an variable name and its value are given
      
(define assign
 (lambda (l)
  (cond
    ((null? ())) )))
  
(define return
  (lambda (l)
    (cond
      (null? ()) )))
    
(define conditional
  (lambda (l)
    (cond
      (null? ()) )))

(define loop
  (lambda (l)
    (cond
      (null? ()) )))