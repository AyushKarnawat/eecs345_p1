; Team members: Steven Leonis, Steven Xie, Ayush Karnawat

; The main function, it gets the parse tree and divides up the statements
; which are then read in sequential order.
; Functions in this file can only access the state through other functions in states.scm.

(load "simpleParser.scm")
(load "state.scm")

; Redefining basic scheme keywords (car , cdr, cadr, cons, etc..) for abstraction
(define next car)
(define combine cons)
;(define rest cdr)   These rewordings are default behavior in PrettyBig
;(define second cadr)
;(define third caddr)

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
      ((null? (rest l)) (runStatement (next l)))  ; if there is the only one statement in the list to be evaluated
      (else (runStatement (next l))              ; call runStatement on the first list  
            (runStatementList (rest l))) )))     ; and on the rest of the list
      
; Runs the given statement by calling the necessary functions.
; When the function finds a return statement, it returns the value of the given variable name.
; 
; params:
;   l: statements (i.e. (var x), (= x 10), (return x) ...)
(define runStatement
  (lambda (l)
    (cond
      ((null? l) '())
      ((eq? (next l) 'var) (declareVar (rest l))) ; call variable declaration on rest of the statement
      ((eq? (next l) '=) (assign (rest l)))       ; call assignment on rest of statement
      ((eq? (next l) 'return) (return (rest l)))  ; call return on rest of statement
      ((eq? (next l) 'if) (conditional (rest l))) ; call if on rest of statement
      ((eq? (next l) 'while) (loop (rest l)))     ; call while on rest of statement
      (else
       (error "Not a valid statement!")) )))

; Function that takes a variable declaration statement and executes it.
; 
; params:
;   l: variable declaration statement (i.e. '(x) or '(x 5)) for a (name value) pair
(define declareVar
  (lambda (l)
    (cond
      ((null? (rest l)) (addVar (next l) '())) ; if the only given statement is the variable name, assign its value to an empty list
      (else (addVar (next l) (evalExp (second l)))) ))) ; if an variable name and its value are given

; Assigns a value to a variable.
; 
; params:
;   l: a (name value) pair related to the variable
(define assign
 (lambda (l)
  (cond
    ((null? l) '())
    ((declared? (next l)) (addVar (next l) (evalExp (second l))))
    (else ((declared? (next l)) (error "Variable is not declared."))) ))) ; if the variable is avaliable in the state, assign the value to the var

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
      ((eq? (evalExp (next l)) #t) 'true)
      ((eq? (evalExp (next l)) #f) 'false)
      (else (evalExp (next l))) )))

; example of if statement as passed:
;      expr    then       else (in this case, if)
; (if (> x y) (return x) (if (> (* x x) y) ... )
; Must evaluate expression after itself, then execute stuff after    
(define conditional
  (lambda (l)
    (cond
      ((evalExp (next l)) (runStatement (next (rest l)))) ; test whether (car l) is true
      ((null? (rest (rest l))) '())
      (else (runStatement(third l))) ))) ;if expr to evaluate is false

; As the name suggests, loop until test-expr is true
;   test-expr using evalExpr | body of loop
; (while (!= (% y x) 3) (= y (+ y 1))) 
(define loop
  (lambda (l)
    (cond
      ((not (evalExp (next l))) '())
      (else (runStatement (second l)) (loop l)) ))) ; check statement, and loop again
