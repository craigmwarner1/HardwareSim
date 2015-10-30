#lang racket
(require plai
	 rackunit)
;; One small step for a man, one giant leap for a project Matt has been dreaming-of/dreading.

;;DATATYPES

(define-type CHIP
  [id    (sym symbol?)]
  [in    (ins list?)]
  [out   (outs list?)]
  [parts (name Chip?)
         (args list?)]
)


;;PARSING

;;first-is? : sexp -> sexp
(define (first-is? sexp sym)
  (equal? (first sexp) sym))

;;first-is-one-of? : sexp '()
;;for use with common/easily expressed Bool operations 
(define (first-is-one-of? sexp ls)
  (member (first sexp) ls))


;;parse : sexp -> AST
(define (parse sexp)
  (cond
    [(symbol? sexp) (id sexp)]
)


;;INTERPRETATION
(define (interp ast)
  (type-case Chip ast
    [in (ins) 
        (
  