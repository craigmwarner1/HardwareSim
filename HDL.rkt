#lang racket
(require plai
	 rackunit)
;; One small step for a man, one giant leap for a project Matt has been dreaming-of/dreading.

;;DATATYPES

(define-type Chip
  [id    (id symbol?)]
  [var   (var symbol?)]
  [ins   (inputs symbol?)]
  [outs  (out symbol?)]
  [parts (name Chip?)
         (args list?)]
  [array (arr list?)]
)




;;PARSING

;;first-is? : sexp -> sexp
(define (first-is? sexp sym)
  (equal? (first sexp) sym))


;;parse : sexp -> AST
(define (parse sexp)
  (cond
    [(first-is? sexp 'CHIP)
      (id (second sexp))
      (parse rest sexp)]
    [(symbol? (first sexp)
      (cond
        [(equal? (first sexp) 'IN)
	  (ins )	//How to make it stop at colons???
	  (parse (rest sexp)] 
	[(equal? (first sexp) 'OUT)
	  (outs ) 	//How to make it stop at colons???
	  (parse (rest sexp)] 
	[(equal? (first sexp) 'PARTS)
	  (parts) 	//How to make it stop at colons???
	  (parse (rest sexp)]
  
)


;;INTERPRETATION
