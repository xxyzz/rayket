#lang racket/base

(require racket/class racket/flonum racket/math)
(require "vec3.rkt" "ray.rkt" "hittable.rkt" "aabb.rkt")

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
  (let*-values ([(normal) (vec-div-val (vec-sub point center) radius)]
                [(front-face) (negative? (vec-dot (ray-direction r) normal))]
                [(u v) (get-shpere-uv point)])
    (hit-record point
                (if front-face normal (vec-neg normal))
                t
                front-face
                material
                u
                v)))

(define (get-shpere-uv p)
  ;; p: a given point on the sphere of radius one, centered at the origin.
  ;; u: returned value [0,1] of angle around the Y axis from X=-1.
  ;; v: returned value [0,1] of angle from Y=-1 to Y=+1.
  ;; <1 0 0> yields <0.50 0.50>    <-1  0  0> yields <0.00 0.50>
  ;; <0 1 0> yields <0.50 1.00>    < 0 -1  0> yields <0.50 0.00>
  ;; <0 0 1> yields <0.25 0.50>    < 0  0 -1> yields <0.75 0.50>
  (let ([theta (flacos (- (vec-y p)))]
        [phi (fl+ (flatan (fl/ (- (vec-z p)) (vec-x p)))
                  pi)])
    (values (fl/ phi (* 2 pi)) ;; u
            (fl/ theta pi))))  ;; v

(define sphere%
  (class object%
    (init-field center radius material)
    (super-new)

    (define/public (hit r t-min t-max)
      (find-hit-point r t-min t-max center radius material))

    (define/public (bounding-box time0 tim1)
      (aabb (vec-sub center (flvector radius radius radius))
            (vec-add center (flvector radius radius radius))))))

(define moving-sphere%
  (class object%
    (init-field center0 center1 time0 time1 radius material)
    (super-new)

    (define/public (hit r t-min t-max)
      (find-hit-point r t-min t-max (center (ray-time r)) radius material))

    (define/public (center tm)
      (vec-add center0
               (vec-mul-val (vec-sub center1 center0)
                            (fl/ (fl- tm time0)
                                 (fl- time1 time0)))))

    (define/public (bounding-box time0 time1)
      (let ([radius-vec (flvector radius radius radius)]
            [time0-center (center time0)]
            [time1-center (center time1)])
        (surrounding-box
         (aabb (vec-sub time0-center radius-vec)
               (vec-add time0-center radius-vec))
         (aabb (vec-sub time1-center radius-vec)
               (vec-add time1-center radius-vec)))))))
