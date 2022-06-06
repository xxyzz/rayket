#lang racket/base
(require racket/vector)  ;; vector-map

(provide vec-add vec-mul-val vec-div-val vec-neg vec-length vec-length-squared vec-sub vec-mul vec-div vec-dot vec-cross unit-vector vec-x vec-y vec-z random-in-unit-sphere random-unit-vector near-zero? reflect refract random-in-unit-disk random-inexact-range random-vec random-vec-range)

(define (vec-x vec)
  (vector-ref vec 0))

(define (vec-y vec)
  (vector-ref vec 1))

(define (vec-z vec)
  (vector-ref vec 2))

(define (vec-add vec . rest-vecs)
  (apply vector-map + vec rest-vecs))

(define (vec-mul-val vec v)
  (for/vector ([a vec])
    (* a v)))

(define (vec-div-val vec v)
  (for/vector ([a vec])
    (/ a v)))

(define (vec-neg vec)
  (for/vector ([v vec])
    (- v)))

(define (vec-length vec)
  (sqrt (vec-length-squared vec)))

(define (vec-length-squared vec)
  (for/fold ([prev 0])
            ([v vec])
    (+ prev (* v v))))

(define (vec-sub vec . rest-vecs)
  (apply vector-map - vec rest-vecs))

(define (vec-mul vec . rest-vecs)
  (apply vector-map * vec rest-vecs))

(define (vec-div vec . rest-vecs)
  (apply vector-map / vec rest-vecs))

(define (vec-dot vec-a vec-b)
  (for/fold ([sum 0])
            ([a vec-a]
             [b vec-b])
    (+ sum (* a b))))

(define (vec-cross vec-a vec-b)
  (let ([vec-a-x (vec-x vec-a)]
        [vec-a-y (vec-y vec-a)]
        [vec-a-z (vec-z vec-a)]
        [vec-b-x (vec-x vec-b)]
        [vec-b-y (vec-y vec-b)]
        [vec-b-z (vec-z vec-b)])
    (vector (- (* vec-a-y vec-b-z)
               (* vec-a-z vec-b-y))
            (- (* vec-a-z vec-b-x)
               (* vec-a-x vec-b-z))
            (- (* vec-a-x vec-b-y)
               (* vec-a-y vec-b-x)))))

(define (unit-vector vec)
  (vec-div-val vec (vec-length vec)))

(define (random-in-unit-sphere)
  (let ([p (vector (random) (random) (random))])
    (if (< (vec-length-squared p) 1)
        p
        (random-in-unit-sphere))))

(define (random-unit-vector)
  (unit-vector (random-in-unit-sphere)))

(define (near-zero? vec)
  (for/and ([v vec])
    (< (abs v) 1e-8)))

(define (reflect v n)
  (vec-sub v (vec-mul-val n (* 2 (vec-dot v n)))))

(define (refract uv n etai-over-etat)
  (let* ([cos-theta (min 1 (vec-dot (vec-neg uv) n))]
         [r-out-prep (vec-mul-val (vec-add uv (vec-mul-val n cos-theta))
                                  etai-over-etat)]
         [r-out-parallel (vec-mul-val n (- (sqrt (abs (- 1 (vec-length-squared r-out-prep))))))])
  (vec-add r-out-prep r-out-parallel)))

(define (random-inexact-range min-val max-val)
  (+ min-val (* (- max-val min-val) (random))))

(define (random-in-unit-disk)
  (let ([vec (vector (random-inexact-range -1 1) (random-inexact-range -1 1) 0)])
    (if (< (vec-length-squared vec) 1)
        vec
        (random-in-unit-disk))))

(define (random-vec)
  (for/vector ([_ (in-range 3)])
    (random)))

(define (random-vec-range min-val max-val)
  (for/vector ([_ (in-range 3)])
    (random-inexact-range min-val max-val)))
