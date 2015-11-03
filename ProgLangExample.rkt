#lang racket
(require plai
         rackunit)

;; DATATYPES
(define (env? o) (list? o))

(define-type WAE0
  [num (n number?)]
  [id  (sym symbol?)]
  [binop (op symbol?)
         (lhs WAE0?)
         (rhs WAE0?)]
  [with  (var id?)
         (val WAE0?)
         (body WAE0?)]
  [if0   (test-exp WAE0?)
         (true-exp WAE0?)
         (false-exp WAE0?)]
  [bif   (test-exp BEXP?)
         (true-exp WAE0?)
         (false-exp WAE0?)]
  [fun-def (formal id?)
           (body WAE0?)
           (enviro list?)]
  [fun-app (name id?)
           (param WAE0?)]
  [set     (id symbol?)
           (value number?)]
  [sequence (lot list?)])

(define-type BEXP
  [bool (b boolean?)]
  [bincomp (comp symbol?)
           (lhs WAE0?)
           (rhs WAE0?)]
  [binconj (conj symbol?)
           (lhs BEXP?)
           (rhs BEXP?)]
  [unbexp  (exp symbol?)
           (bval BEXP?)])
           

;; PARSING HELPERS
(define (first-is? sexp sym)
  (equal? (first sexp) sym))

;; first-is-one-of? : list-of-symbols list -> boolean
;; Checks to see if the first symbol in the s-expression is a member
;; of the list. This lets us ask if we are looking at an operator
;; that is member of a set. For example:
;;
;; (first-is-one-of? sexp '(+ - * /))
;;
;; Checks to see if the first think in the sexp is one of the common
;; arithmetic operators.
(define (first-is-one-of? sexp ls)
  (member (first sexp) ls))

;; convert-to-function : symbol -> function
;; Evals a symbol in the base namespace. This lets
;; us quickly convert the symbol '+ into the function +.
;; Great fun at parties. Impress all your friends.
(define (convert-to-function sym)
  (define ns (make-base-namespace))
  (eval sym ns))

;; PARSING FUNCTIONS

;; parse : s-expression -> AST
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(first-is-one-of? sexp '(+ - * /))
     (binop (first sexp)
            (parse (second sexp))
            (parse (third sexp)))]
    [(first-is? sexp 'with)
     (with (parse (first (second sexp)))
           (parse (second (second sexp)))
           (parse (third sexp)))]
    [(first-is? sexp 'if0)
     (if0 (parse (second sexp)))]
    [(first-is? sexp 'bif)
     (bif (parse-bexp '((second sexp)
          (third sexp)
          (fourth sexp))))]
    [(and (symbol? (first sexp)
                   (first-is? (second sexp) 'fun))
          (parse (second (second sexp)))
          (parse (second (third sexp)))
          '())]
    [(first-is? sexp 'fun)
     (fun-def (parse (second sexp))
              (parse (third sexp))
              '())]
    [(first-is? sexp 'set)
     (set (parse (second sexp))
          (parse (third sexp)))]
    [(first-is? sexp 'sequence)
     (sequence (parse (first lot))
               (parse (rest lot)))]
    [(symbol? (first sexp)) 
     (fun-app (parse (first sexp))
              (parse (second sexp)))]


;;parse-bexp : b-expression -> Bool
(define (parse-bexp bexp)
  (cond
    [(boolean? bexp) bexp]
    [(first-is-one-of? bexp '(> < ==))
     (bincomp bexp)]
    [(first-is-one-of? bexp '(and or))
     (binconj (parse-bexp (second bexp)
              (parse-bexp (third bexp))))]
    [(equal? (first bexp) 'not)
     (unbexp (parse-bexp (second bexp)))]
    ))
      
;; ENVIRONMENT HELPERS
(struct undefined ())
(struct binding (var val) #:transparent)

(define (empty-env) empty)

(define (extend-env obj env)
  (cons obj env))

(define (lookup id env)
  (cond
    [(empty? env) (undefined)]
    [(equal? id (id-sym (binding-var (first env))))
     (binding-val (first env))]
    [else 
     (lookup id (rest env))]))

;; INTERPRETING FUNCTIONS
(define (interp ast env)
  (type-case WAE0 ast
    [num (n) n]
    [id  (sym)
         (let ([found (lookup sym env)])
           (if (undefined? found)
               (error 'interp "Unbound identifier: ~a" sym)
               found))]
    [binop (op lhs rhs)
           ((convert-to-function op) (interp lhs env)
               (interp rhs env))]
    [with (var val body)
          (interp 
           body
           (extend-env (binding var (interp val env))
                       env))]
    [if0 (test-exp true-exp false-exp)
         (if 
          (= test-exp 0)
          true-exp 
          false-exp)]
    [bif (test-exp true-exp false-exp)
         (if
          (= (interp-bexp test-exp env) #t) ;;needs to go to inter-bexp
          true-exp
          false-exp)]
    [fun-def (formal body enviro)
             (interp body env)]
    [fun-app (name param)
     (interp param (lookup name))]
    ))

(define (interp-bexp bst env)
  (type-case BEXP bst
    [bool (b) b]
    [bincomp (comp lhs rhs) 
             ((convert-to-function comp) (interp lhs env)
                                         (interp rhs env))]
    [binconj (conj lhs rhs)
             (cond
               [(equal? (first bst) 'and) (and (interp-bexp lhs env)
                                               (interp-bexp rhs env))]
               [(equal? (first bst) 'or)  (or  (interp-bexp lhs env)
                                               (interp-bexp rhs env))])]
    [unbexp (exp bval)
            (not (interp-bexp bval env))]))

;; TESTS
;; These tests are not written using the full rackunit test suite,
;; because it seems like no one is willing to write them if I use
;; the full power of rackunit. 
;; 
;; Please extend these to test the work you do.
;; These do not cover the entirety of the interpreter.

;; PARSER TESTS
(check-equal? (parse 3) (num 3))
(check-equal? (parse '(+ 3 5)) (binop '+ (num 3) (num 5)))

;; INTERPRETER TESTS
(check-equal? (interp (parse 3) (empty-env)) 3)
(check-equal? (interp (parse '(+ 3 5)) (empty-env)) 8)
