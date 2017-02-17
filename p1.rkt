;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname p1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define interpret
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (car l) ))))

; Runs the line it is given.
; if run statements finds a return, it prints(return?) the proper value.
(define runStatement
  (lambda (l)
    (cond
      ((null? l) '())
      ((eq? (car l) 'var) declareVar(cdr l)); call varaible declaration on rest of the line
      ((eq? (car l) '=) assign(cdr l))
      ((eq? (car l) 'return) return(cdr l))
      ((eq? (car l) 'if) conditional(cdr l))
      ((eq? (car l) 'while) loop(cdr l))
      (else
       (error 'Not a valid statement!)) )))