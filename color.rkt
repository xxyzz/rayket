#lang racket/base

(require racket/flonum)
(require racket/math)  ;; exact-floor

(provide write-color max-color-value)

(define max-color-value 255.0)

(define (color-r color)
  (flvector-ref color 0))

(define (color-g color)
  (flvector-ref color 1))

(define (color-b color)
  (flvector-ref color 2))

(define (clamp x min-val max-val)
  (cond [(fl< x min-val) min-val]
        [(fl> x max-val) max-val]
        [else x]))

(define (write-color color samples-per-pixel)
  ;; Divide the color by the number of samples and gamma-correct for gamma=2.0.
  ;; https://www.cambridgeincolour.com/tutorials/gamma-correction.htm
  (let ([r (flsqrt (fl/ (color-r color) samples-per-pixel))]
        [g (flsqrt (fl/ (color-g color) samples-per-pixel))]
        [b (flsqrt (fl/ (color-b color) samples-per-pixel))])
    (display
     (format "~a ~a ~a\n"
             (exact-floor (fl* max-color-value (clamp r 0.0 1.0)))
             (exact-floor (fl* max-color-value (clamp g 0.0 1.0)))
             (exact-floor (fl* max-color-value (clamp b 0.0 1.0)))))))
