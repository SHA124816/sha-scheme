(module (sha bitwise)
	(arithmetic-shift
	 bitwise-and
	 bitwise-xor)

(import scheme)

;;;; Bitwise operations

(define (first-bit number)
  (remainder number 2))

(define (shift-right number)
  (quotient number 2))
(define (shift-left number)
  (* number 2))
(define (arithmetic-shift number k)
  (cond ((= k 0)
	 number)
	((> k 0)
	 (arithmetic-shift (shift-left number) (- k 1)))
	((< k 0)
	 (arithmetic-shift (shift-right number) (+ k 1)))))

(define (bitwise-and a b)
  (define (bit-and b1 b2)
    (if (= b1 b2 1) 1 0))
  (if (or (= a 0) (= b 0))
      0
      (+ (bit-and (first-bit a) (first-bit b))
	 (* 2 (bitwise-and (shift-right a) (shift-right b))))))

(define (bitwise-xor a b)
  (define (bit-xor b1 b2)
    (if (= b1 b2) 0 1))
  (if (and (= a 0) (= b 0))
      0
      (+ (bit-xor (first-bit a) (first-bit b))
	 (* 2 (bitwise-xor (shift-right a) (shift-right b))))))

)
