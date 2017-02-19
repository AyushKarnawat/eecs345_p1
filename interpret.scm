;The main function, it gets the parse tree and divides up the statements
; which are then read in sequential order
;Functions in this file can only access the state through other functions in states.scm
(load "simpleParser.scm")
(load "state.scm")

(define s 'a)

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

;Function that takes a variable declaration statement and executes it
(define declareVar
  (lambda (l)
    ()))
