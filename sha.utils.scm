(module (sha utils)
	(iter-until-1
	 iter-while-1
	 iter-until
	 iter-while

	 string-map

	 make-list
	 foldl

	 integer-range
	 cyclic-clamp
	 two-power-leq)

(import scheme)
(import (only srfi-23 error))

;;;; Control flow

(define (iter-until-1 f val p)
  (if (p val)
      val
      (iter-until-1 f (f val) p)))

(define (iter-while-1 f val p)
  (define (iter-while-2 f val p)
    (let ((next (f val)))
      (if (p next)
	  (iter-while-2 f next p)
	  val)))
  (if (p val)
      (iter-while-2 f val p)
      (error "Predicate should return true for the initial value" val)))

(define-syntax iter-until
  (syntax-rules ()
    ((_ arg f-body val p-body)
     (iter-until-1
      (lambda arg f-body)
      val
      (lambda arg p-body)))))

(define-syntax iter-while
  (syntax-rules ()
    ((_ arg f-body val p-body)
     (iter-while-1
      (lambda arg f-body)
      val
      (lambda arg p-body)))))

;;;; Characters and Strings

(define (string-map f s)
  (let ((new (string-copy s)))
    (do ((i 0 (+ i 1)))
	((>= i (string-length s)) new)
      (string-set! new i (f (string-ref new i))))))

;;;; Lists

(define (make-list len fill)
  (if (<= len 0)
      '()
      (cons fill (make-list (- len 1) fill))))

(define (foldl f initial-value l)
  (if (null? l)
      initial-value
      (foldl f
	     (f initial-value (car l))
	     (cdr l))))

(define (integer-range a b)
  (if (>= a b)
      (list b)
      (cons a (integer-range (+ a 1) b))))

;;;; Numbers

(define (cyclic-clamp x a b)
  (let ((range-size (- (+ b 1) a)))
    (+ a
       (modulo (- x a) range-size))))

(define (two-power-leq n)
  (if (< n 1)
      (error "Argument less than one" n)
      (iter-while (x) (* 2 x) 1 (<= x n))))

)
