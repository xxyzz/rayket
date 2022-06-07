#lang racket/base

(require racket/class racket/flonum)
(require "vec3.rkt" "ray.rkt" "hittable.rkt")

(provide sphere% moving-sphere%)

(define (find-hit-point r t-min t-max center radius material)
  (let* ([oc (vec-sub (ray-origin r) center)]
         [a (vec-length-squared (ray-direction r))]
         [half-b (vec-dot oc (ray-direction r))]
         [c (fl- (vec-length-squared oc) (fl* radius radius))]
         [discriminant (fl- (fl* half-b half-b) (fl* a c))])
    (if (negative? discriminant)
        null
        (let* ([sqrtd (flsqrt discriminant)]
               [root-1 (fl/ (fl- (fl- half-b) sqrtd) a)])
          (if (and (fl>= root-1 t-min) (fl<= root-1 t-max))
              (make-hit-record root-1 (ray-at r root-1) r center radius material)
              (let ([root-2 (fl/ (fl+ (fl- half-b) sqrtd) a)])
                (if (and (fl>= root-2 t-min) (fl<= root-2 t-max))
                    (make-hit-record root-2 (ray-at r root-2) r center radius material)
                    null)))))))

(define (make-hit-record t point r center radius material)
  (let* ([normal (vec-div-val (vec-sub point center) radius)]
         [front-face (negative? (vec-dot (ray-direction r) normal))])
    (hit-record point
                (if front-face normal (vec-neg normal))
                t
                front-face
                material)))

(define sphere%
  (class object%
    (init center radius material)
    (define center-field center)
    (define radius-field radius)
    (define material-field material)
    (super-new)

    (define/public (hit? r t-min t-max)
      (find-hit-point r t-min t-max center-field radius-field material-field))))

(define moving-sphere%
  (class object%
    (init center0 center1 time0 time1 radius material)
    (define center0-field center0)
    (define center1-field center1)
    (define time0-field time0)
    (define time1-field time1)
    (define radius-field radius)
    (define material-field material)
    (super-new)

    (define/public (hit? r t-min t-max)
      (find-hit-point r t-min t-max (center (ray-time r)) radius-field material-field))

    (define/public (center tm)
      (vec-add center0-field
               (vec-mul-val (vec-sub center1-field center0-field)
                            (fl/ (fl- tm time0-field)
                                 (fl- time1-field time0-field)))))))
