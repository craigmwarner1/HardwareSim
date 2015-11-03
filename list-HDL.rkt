#lang racket
(require plai
         rackunit)

;;STRUCTS

(struct chip (ins outs parts)
  #:transparent)

(struct wire (name)
  #:transparent)

(struct bus (name size)
  #:transparent)

(struct part (name connections [built_in? #:auto])
  #:auto-value #f
  #:transparent)

(struct connection (in out)
  #:transparent)

;;DATATYPES


;;PARSING

;;first-is? : sexp -> sexp
(define (first-is? sexp sym)
  (equal? (first sexp) sym))

;;first-is-one-of? : sexp '()
;;for use with common/easily expressed Bool operations 
(define (first-is-one-of? sexp ls)
  (member (first sexp) ls))

;;parts-list - list of potential gates to be used
;;might consider changing to hash table
(define parts-list '[])

;;parse : sexp -> AST
(define (parse sexp)
  (cond
    [(first-is? sexp 'bus)
     (bus 
      (parse (second sexp))
      (parse (third sexp)))
     (parse (rest sexp))]
    [(first-is? sexp 'wire)
     (wire
      (parse (second sexp)))
     (parse (rest sexp))]
    [(first-is-one-of? sexp '[IN OUT])
     (parse (second sexp))
     (parse (rest sexp))]
    [(first-is-one-of? sexp parts-list)
     (part
      (parse (first sexp))
      (parse (second sexp))
      (cond
        [(equal? 'True (third sexp)) #t]
        [else #f]))
     (parse (rest sexp))]        
    [(first-is? sexp 'Parts)
     (parse (second sexp))
     (parse (rest sexp))]
    [(first-is? sexp 'CHIP)
     (chip 
      (parse (third sexp))
      (parse (fourth sexp))
      (parse (fifth sexp)))]))
