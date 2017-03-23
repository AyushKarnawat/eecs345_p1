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
      (else (call/cc (lambda (return) (runStatementList (parser file) return)))) )))

; Runs the entire statement tree recursively by using runStatement multiple times.
; 
; params:
;   l: the parsed tree in list format i.e ((var x) (= x 10) ... (return x))
(define runStatementList
  (lambda (l return)
    (cond 
      ((null? l) '())
      ((null? (rest l)) (runStatement (next l) return (lambda (k) (error "invalid break"))    ; if there is the only one statement in the list to be evaluated
                                                      (lambda (k) (error "invalid continue"))
                                                      (lambda (k) (error "invalid throw"))))  
      (else (runStatement (next l) return (lambda (k) (error "invalid break"))                ; call runStatement on the first list  
                                                      (lambda (k) (error "invalid continue"))
                                                      (lambda (k) (error "invalid throw")))               
            (runStatementList (rest l) return)) )))                                           ; and then on the rest of the list
      
; Runs the given statement by calling the necessary functions.
; When the function finds a return statement, it returns the value of the given variable name.
; 
; params:
;   l: statements (i.e. (var x), (= x 10), (return x) ...)
(define runStatement
  (lambda (l return break continue throw)
    (cond
      ((null? l) '())
      ((eq? (next l) 'var) (declareVar (rest l) return break continue throw)) ; call variable declaration on rest of the statement
      ((eq? (next l) '=) (assign (rest l) return break continue throw))       ; call assignment on rest of statement
      ((eq? (next l) 'return) (return_exp (rest l) return))                   ; call return on rest of statement
      ((eq? (next l) 'if) (conditional (rest l) return break continue throw)) ; call if on rest of statement
      ((eq? (next l) 'while) (call/cc (lambda (k) (loop (rest l) return k continue throw))))     ; call while on rest of statement and setup the break continuation to exit it
      ((eq? (next l) 'begin) (startBlock (rest l) return break continue throw))
      ((eq? (next l) 'break) (break (removeLayer))) ; when we hit a break statement, we want to call the break continuation to exit the current loop / block
      ((eq? (next l) 'continue) (continue (removeLayer))) ; when we hit a continue statement, we want to call the continue continuation to exit this iteration of the loop
      ((eq? (next l) 'try) (try (rest l) return break continue throw))
      ((eq? (next l) 'catch) (catch (rest l) return break continue throw))
      ((eq? (next l) 'finally) (finally (rest l) return break continue throw))
      ((eq? (next l) 'throw) (throw (second l)))
      (else
       (error "Not a valid statement!")) )))

; Function that takes a variable declaration statement and executes it.
; 
; params:
;   l: variable declaration statement (i.e. '(x) or '(x 5)) for a (name value) pair
(define declareVar
  (lambda (l return break continue throw)
    (cond
      ((declared? (next l)) (error "Variable is already declared"))
      ((null? (rest l)) (addVar (next l) '())) ; if the only given statement is the variable name, assign its value to an empty list
      (else (addVar (next l) (evalExp (second l)))) ))) ; if an variable name and its value are given

; Assigns a value to a variable 
; 
; params:
;   l: a (name value) pair related to the variable
(define assign
 (lambda (l return break continue throw)
  (cond
    ((null? l) '())
    ((declared? (next l)) (changeVar (next l) (evalExp (second l))))
    (else (error "Variable is not declared."))) )) ; if the variable is avaliable in the state, assign the value to the var

; Returns a given expression or variable.
;
; examples: (return (* x x)) 
;           (return x) 
;           (return (* x (+ x x))) 
; params:
;   l: the list passed in
(define return_exp
  (lambda (l return)
    (cond
      ((null? l) (error "Return was passed a null list"))
      ((eq? (evalExp (next l)) #t) (return 'true))
      ((eq? (evalExp (next l)) #f) (return 'false))
      (else (return (evalExp (next l)))) )))

; example of if statement as passed:
;      expr    then       else (in this case, if)
; (if (> x y) (return x) (if (> (* x x) y) ... )
; Must evaluate expression after itself, then execute stuff after    
(define conditional
  (lambda (l return break continue throw)
    (cond
      ((evalExp (next l)) (runStatement (next (rest l)) return break continue throw)) ; test whether (car l) is true
      ((null? (rest (rest l))) '())
      (else (runStatement (third l) return break continue throw) )))) ;if expr to evaluate is false

; As the name suggests, loop until test-expr is true
;   test-expr using evalExpr | body of loop
; (while (!= (% y x) 3) (= y (+ y 1))) 
(define loop
  (lambda (l return break continue throw)
    (cond
      ((not (evalExp (next l))) '()) ;check the condition
      (else (call/cc 
        (lambda (k) (runStatement (second l) return break k throw)))
        (loop l return break continue throw)) ))) ; else setup the new "continue" continuation 

(define startBlock
  (lambda (l return break continue throw)
    (addLayer)
    (runBlockStatements l return break continue throw)
    (removeLayer)))

; A helper function that recursively runs each statement inside of the block
(define runBlockStatements
  (lambda (l return break continue throw)
    (cond
      ((null? l)) ;the block is out of statements to run, we can exit
      (else (runStatement (next l) return break continue throw) (runBlockStatements (rest l) return break continue throw)) )))

; try-catch statement
; Example of a typical 'try catch throw finally' use
; ((var x)
; (try 
; Perhaps set a call/cc here to proceed to the catch if throw is reached?
; below is what 'try' recieves in try-stmt
; ((= x 20) (if (< x 0) (throw 10)) (= x (+ x 5)))
; (catch (e) ((= x e)))
; (finally ((= x (+ x 100)))) )
; (return x))

(define try
  (lambda (try-stmt return break continue throw)
    (cond
      ((and (hasCatch? try-stmt) (hasFinally? try-stmt)) (try-catch-finally try-stmt return break continue throw))
      ((hasCatch? try-stmt) (try-catch try-stmt return break continue throw))
      ((hasFinally? try-stmt) (try-finally try-stmt return break continue throw))
      (else (error "Invalid try statement")) )))

; Executes a try statement that only has a catch block
(define try-catch
 (lambda (try-stmt return break continue throw)
    (letrec ([exception (call/cc (lambda (k) (runBlockStatements (next try-stmt) return break continue k)))])
    (cond 
      ((eq? exception #t))
        (else 
          (addVar (next (second (second try-stmt))) exception)
          (runStatement (second try-stmt) return break continue exception)) ))))

; Executes a try statement that only has a finally block
(define try-finally
 (lambda (try-stmt return break continue throw)
  (runBlockStatements (next try-stmt) return break continue throw)
  (runBlockStatements (rest (rest try-stmt)) return break continue throw)))
    
; Executes a try statement that has both a catch block and a finally block
(define try-catch-finally
 (lambda (try-stmt return break continue throw)
  (letrec ([exception (call/cc (lambda (k) (runBlockStatements (next try-stmt) return break continue k)))])
  (cond 
    ((eq? exception #t))
    (else
      (begin
        (addVar (next (second (second try-stmt))) exception)
        (runStatement (second try-stmt) return break continue exception))) )
  (runBlockStatements (rest (rest try-stmt)) return break continue exception) )))

; catch executes with the thrown value it is handed and the code it's given
; by this point in execution, catch is known to have code to run
; should be able to run its statement since exception variable has been declared/assigned
(define catch
 (lambda (catch-stmt return break continue throw)
  (runBlockStatements (second catch-stmt) return break continue throw)))
 
(define finally
 (lambda (finally-stmt return break continue throw)
  (runBlockStatements (next finally-stmt) return break continue throw)))
 

(define helper_try
  (lambda (l return break continue throw)
    (cond
     ((null? l)) ;the block is out of statements to run, we can exit
     (else (runStatement (next l) return break continue throw) (helper_try (rest l) return break continue throw)) )))

; Returns true if the given try statement has a catch statement
(define hasCatch?
  (lambda (try-stmt)
    (cond
      ((< (length try-stmt) 3) (error "try statement is incorrect")) 
      (else (and (not (null? (second try-stmt))) (eq? (next (second try-stmt)) 'catch))) )))
    
; Returns true if the given try statement has a finally statement
(define hasFinally?
  (lambda (try-stmt)
    (cond
      ((< (length try-stmt) 3) (error "try statement is incorrect"))
      (else (and (not (null? (third try-stmt))) (eq? (car (third try-stmt)) 'finally))) )))