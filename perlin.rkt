#lang racket/base

(require racket/flonum racket/list racket/math racket/class)
(require "vec3.rkt")

(provide perlin%)

(define point-count 256)

;; https://en.wikipedia.org/wiki/Perlin_noise
(define perlin%
  (class object%
    (field [random-vectors
            (for/vector ([_ (in-range point-count)])
              (unit-vector (random-vec-range -1.0 1.0)))]
           [range-list (range point-count)]
           [perm-x (list->vector (shuffle range-list))]
           [perm-y (list->vector (shuffle range-list))]
           [perm-z (list->vector (shuffle range-list))])
    (super-new)

    (define/public (turb p [depth 7.0])
      (flabs (for/sum ([i (in-range 0.0 depth)])
               (fl* (flexpt 0.5 i)
                    (noise (vec-mul-val p (flexpt 2.0 i)))))))

    (define/public (noise p)
      (let* ([u (fl- (vec-x p) (flfloor (vec-x p)))]
             [v (fl- (vec-y p) (flfloor (vec-y p)))]
             [w (fl- (vec-z p) (flfloor (vec-z p)))]
             ;; use a Hermite cubic to round off the interpolation:
             [uu (fl* u u (fl- 3.0 (fl* 2.0 u)))]
             [vv (fl* v v (fl- 3.0 (fl* 2.0 v)))]
             [ww (fl* w w (fl- 3.0 (fl* 2.0 w)))]
             [i (exact-floor (vec-x p))]
             [j (exact-floor (vec-y p))]
             [k (exact-floor (vec-z p))])
        ;; linear interpolate
        (for*/sum ([di (in-range 0.0 2.0)]
                   [dj (in-range 0.0 2.0)]
                   [dk (in-range 0.0 2.0)])
          (fl* (fl+ (fl* di uu)
                    (fl* (fl- 1.0 di) (fl- 1.0 uu)))
               (fl+ (fl* dj vv)
                    (fl* (fl- 1.0 dj) (fl- 1.0 vv)))
               (fl+ (fl* dk ww)
                    (fl* (fl- 1.0 dk) (fl- 1.0 ww)))
               (vec-dot
                (flvector (fl- u di) (fl- v dj) (fl- w dk))
                (vector-ref
                 random-vectors
                 (bitwise-xor (vector-ref perm-x
                                          (bitwise-and (sub1 point-count)
                                                       (+ i (fl->exact-integer di))))
                              (vector-ref perm-y
                                          (bitwise-and (sub1 point-count)
                                                       (+ j (fl->exact-integer dj))))
                              (vector-ref perm-z
                                          (bitwise-and (sub1 point-count)
                                                       (+ k (fl->exact-integer dk)))))))))))))
