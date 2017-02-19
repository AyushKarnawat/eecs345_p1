;This file is where the state is stored
;Any function in this file has to do with reading or writing from the state
;In order to get the state, functions just need to ask for S
(define S '())

;In order to reset the state, functions will need to use setState. This should only be used by functions in this file.
(define setState
  (lambda (x)
    (set! S x)))
