#lang racket/base

(require racket/class)
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
             [c (- (vec-length-squared oc) (* radius-field radius-field))]
             [discriminant (- (* half-b half-b) (* a c))])
        (if (negative? discriminant)
            null
            (let* ([sqrtd (sqrt discriminant)]
                   [root-1 (/ (- (- half-b) sqrtd) a)]
                   [root-2 (/ (+ (- half-b) sqrtd) a)])
              (cond [(and (>= root-1 t-min) (<= root-1 t-max))
                     (make-hit-record root-1 (ray-at r root-1) r)]
                    [(and (>= root-2 t-min) (<= root-2 t-max))
                     (make-hit-record root-2 (ray-at r root-2) r)]
                    [else null])))))

    (define/private (make-hit-record t point r)
      (let* ([normal (vec-div-val (vec-sub point center-field) radius-field)]
             [front-face (negative? (vec-dot (ray-direction r) normal))])
        (hit-record point
                    (if front-face normal (vec-neg normal))
                    t
                    front-face
                    material-field)))))
