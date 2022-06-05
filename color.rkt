#lang racket/base

(require racket/math)  ;; exact-floor

(provide write-color max-color-value)

(define max-color-value 255)

(define (color-r color)
  (vector-ref color 0))

(define (color-g color)
  (vector-ref color 1))

(define (color-b color)
  (vector-ref color 2))

(define (clamp x min-val max-val)
  (cond [(< x min-val) min-val]
        [(> x max-val) max-val]
        [else x]))

(define (write-color color samples-per-pixel)
  ;; Divide the color by the number of samples and gamma-correct for gamma=2.0.
  ;; https://www.cambridgeincolour.com/tutorials/gamma-correction.htm
  (let ([r (sqrt (/ (color-r color) samples-per-pixel))]
        [g (sqrt (/ (color-g color) samples-per-pixel))]
        [b (sqrt (/ (color-b color) samples-per-pixel))])
    (display
     (format "~a ~a ~a\n"
             (exact-floor (* max-color-value (clamp r 0 1)))
             (exact-floor (* max-color-value (clamp g 0 1)))
             (exact-floor (* max-color-value (clamp b 0 1)))))))
