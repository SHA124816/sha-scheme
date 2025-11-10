(module (sha random)
	(random)

(import scheme)

;;; Algorithm and magic numbers taken from glibc
(define (make-rng seed)
  (let ((a 1664525)
        (c 1013904223)
        (m (expt 2 32))
        (state seed))
    (lambda (n)
      (set! state (modulo (+ (* a state) c) m))
      (modulo state n))))

(define random (make-rng 12345))

)
