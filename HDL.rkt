#lang racket
(require plai
	 rackunit)
;; One small step for a man, one giant leap for a project Matt has been dreaming-of/dreading.

;;DATATYPES

(define-type Chip
	[IN  (in symbol?)]
	[OUT (out symbol?)]
	[PARTS (name Chip?)
	       (args list?)]
 )



;;PARSING

;;parse : s-exp -> AST
(define (parse sexp)

)


;;INTERPRETATION
