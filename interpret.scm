; Team members: Steven Leonis, Steven Xie, Ayush Karnawat

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
      ((null? (cdr l)) (runStatement (car l)))  ; if there is the only one statement in the list to be evaluated
      (else (runStatement (car l))              ; call runStatement on the first list  
            (runStatementList (cdr l))) )))     ; and on the rest of the list
      
; Runs the given statement by calling the necessary functions.
; When the function finds a return statement, it returns the value of the given variable name.
; 
; params:
;   l: statements (i.e. (var x), (= x 10), (return x) ...)
(define runStatement
  (lambda (l)
    (cond
      ((null? l) '())
      ((eq? (car l) 'var) (declareVar (cdr l))) ; call variable declaration on rest of the statement
      ((eq? (car l) '=) (assign (cdr l)))       ; call assignment on rest of statement
      ((eq? (car l) 'return) (return (cdr l)))  ; call return on rest of statement
      ((eq? (car l) 'if) (conditional (cdr l))) ; call if on rest of statement
      ((eq? (car l) 'while) (loop (cdr l)))     ; call while on rest of statement
      (else
       (error "Not a valid statement!")) )))

; Function that takes a variable declaration statement and executes it.
; 
; params:
;   l: variable declaration statement (i.e. '(x) or '(x 5)) for a (name value) pair
(define declareVar
  (lambda (l)
    (cond
      ((null? (cdr l)) (addVar (car l) '())) ; if the only given statement is the variable name, assign its value to an empty list
      (else (addVar (car l) (evalExp (cadr l)))) ))) ; if an variable name and its value are given

; Assigns a value to a variable.
; 
; params:
;   l: a (name value) pair related to the variable
(define assign
 (lambda (l)
  (cond
    ((null? l) '())
    ((declared? (car l)) (addVar (car l) (evalExp (cadr l))))
    (else ((declared? (car l)) (error "Variable is not declared."))) ))) ; if the variable is avaliable in the state, assign the value to the var

; Returns a given expression or variable.
;
; examples: (return (* x x)) 
;           (return x) 
;           (return (* x (+ x x))) 
; params:
;   l: the list passed in
(define return
  (lambda (l)
    (cond
      ((null? l) (error "Return was passed a null list"))
      ((eq? (evalExp (car l)) #t) 'true)
      ((eq? (evalExp (car l)) #f) 'false)
      (else (evalExp (car l))) )))

; example of if statement as passed:
;      expr    then       else (in this case, if)
; (if (> x y) (return x) (if (> (* x x) y) ... )
; Must evaluate expression after itself, then execute stuff after    
(define conditional
  (lambda (l)
    (cond
      ((evalExp (car l)) (runStatement (car (cdr l)))) ; test whether (car l) is true
      ((null? (cddr l)) '())
      (else (runStatement(caddr l))) ))) ;if expr to evaluate is false

; As the name suggests, loop until test-expr is true
;   test-expr using evalExpr | body of loop
; (while (!= (% y x) 3) (= y (+ y 1))) 
(define loop
  (lambda (l)
    (cond
      ((not (evalExp (car l))) '())
      (else (runStatement (cadr l)) (loop l)) ))) ; check statement, and loop again
