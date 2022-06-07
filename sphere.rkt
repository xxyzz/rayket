#lang racket/base

(require racket/class racket/flonum)
(require "vec3.rkt" "ray.rkt" "hittable.rkt")

(provide sphere%)

(define sphere%
  (class object%
    (init center radius material)
    (define center-field center)
    (define radius-field radius)
    (define material-field material)
    (super-new)

    (define/public (hit? r t-min t-max)
      (let* ([oc (vec-sub (ray-origin r) center-field)]
             [a (vec-length-squared (ray-direction r))]
             [half-b (vec-dot oc (ray-direction r))]
             [c (fl- (vec-length-squared oc) (fl* radius-field radius-field))]
             [discriminant (fl- (fl* half-b half-b) (fl* a c))])
        (if (negative? discriminant)
            null
            (let* ([sqrtd (flsqrt discriminant)]
                   [root-1 (fl/ (fl- (fl- half-b) sqrtd) a)])
              (if (and (fl>= root-1 t-min) (fl<= root-1 t-max))
                  (make-hit-record root-1 (ray-at r root-1) r)
                  (let ([root-2 (fl/ (fl+ (fl- half-b) sqrtd) a)])
                    (if (and (fl>= root-2 t-min) (fl<= root-2 t-max))
                        (make-hit-record root-2 (ray-at r root-2) r)
                        null)))))))

    (define/private (make-hit-record t point r)
      (let* ([normal (vec-div-val (vec-sub point center-field) radius-field)]
             [front-face (negative? (vec-dot (ray-direction r) normal))])
        (hit-record point
                    (if front-face normal (vec-neg normal))
                    t
                    front-face
                    material-field)))))
