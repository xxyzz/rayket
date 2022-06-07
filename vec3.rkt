#lang racket/base

(require racket/flonum)

(provide vec-add vec-mul-val vec-div-val vec-neg vec-length vec-length-squared vec-sub vec-mul vec-div vec-dot vec-cross unit-vector vec-x vec-y vec-z random-in-unit-sphere random-unit-vector near-zero? reflect refract random-in-unit-disk random-inexact-range random-vec random-vec-range)

(define (vec-x vec)
  (flvector-ref vec 0))

(define (vec-y vec)
  (flvector-ref vec 1))

(define (vec-z vec)
  (flvector-ref vec 2))

(define (flvector-map f v . vs)
  (for/flvector ([i (in-range (flvector-length v))])
    (apply f (map (lambda (x) (flvector-ref x i)) (cons v vs)))))

(define (vec-add vec . rest-vecs)
  (apply flvector-map fl+ vec rest-vecs))

(define (vec-mul-val vec v)
  (for/flvector ([a vec])
    (fl* a v)))

(define (vec-div-val vec v)
  (for/flvector ([a vec])
    (fl/ a v)))

(define (vec-neg vec)
  (for/flvector ([v vec])
    (fl- v)))

(define (vec-length vec)
  (flsqrt (vec-length-squared vec)))

(define (vec-length-squared vec)
  (for/fold ([prev 0.0])
            ([v vec])
    (fl+ prev (fl* v v))))

(define (vec-sub vec . rest-vecs)
  (apply flvector-map fl- vec rest-vecs))

(define (vec-mul vec . rest-vecs)
  (apply flvector-map fl* vec rest-vecs))

(define (vec-div vec . rest-vecs)
  (apply flvector-map fl/ vec rest-vecs))

(define (vec-dot vec-a vec-b)
  (for/fold ([sum 0.0])
            ([a vec-a]
             [b vec-b])
    (fl+ sum (fl* a b))))

(define (vec-cross vec-a vec-b)
  (let ([vec-a-x (vec-x vec-a)]
        [vec-a-y (vec-y vec-a)]
        [vec-a-z (vec-z vec-a)]
        [vec-b-x (vec-x vec-b)]
        [vec-b-y (vec-y vec-b)]
        [vec-b-z (vec-z vec-b)])
    (flvector (fl- (fl* vec-a-y vec-b-z)
                   (fl* vec-a-z vec-b-y))
              (fl- (fl* vec-a-z vec-b-x)
                   (fl* vec-a-x vec-b-z))
              (fl- (fl* vec-a-x vec-b-y)
                   (fl* vec-a-y vec-b-x)))))

(define (unit-vector vec)
  (vec-div-val vec (vec-length vec)))

(define (random-in-unit-sphere)
  (let ([p (flvector (random-inexact-range -1.0 1.0)
                     (random-inexact-range -1.0 1.0)
                     (random-inexact-range -1.0 1.0))])
    (if (fl< (vec-length-squared p) 1.0)
        p
        (random-in-unit-sphere))))

(define (random-unit-vector)
  (unit-vector (random-in-unit-sphere)))

(define (near-zero? vec)
  (for/and ([v vec])
    (fl< (flabs v) 1e-8)))

(define (reflect v n)
  (vec-sub v (vec-mul-val n (fl* 2.0 (vec-dot v n)))))

(define (refract uv n etai-over-etat)
  (let* ([cos-theta (flmin 1.0 (vec-dot (vec-neg uv) n))]
         [r-out-prep (vec-mul-val (vec-add uv (vec-mul-val n cos-theta))
                                  etai-over-etat)]
         [r-out-parallel (vec-mul-val n (fl- (flsqrt (flabs (fl- 1.0 (vec-length-squared r-out-prep))))))])
    (vec-add r-out-prep r-out-parallel)))

(define (random-inexact-range min-val max-val)
  (fl+ min-val (fl* (fl- max-val min-val) (random))))

(define (random-in-unit-disk)
  (let ([vec (flvector (random-inexact-range -1.0 1.0)
                       (random-inexact-range -1.0 1.0)
                       0.0)])
    (if (fl< (vec-length-squared vec) 1.0)
        vec
        (random-in-unit-disk))))

(define (random-vec)
  (for/flvector ([_ (in-range 3)])
    (random)))

(define (random-vec-range min-val max-val)
  (for/flvector ([_ (in-range 3)])
    (random-inexact-range min-val max-val)))
