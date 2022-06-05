#lang racket/base

(require racket/class)
(require "vec3.rkt" "ray.rkt" "hittable.rkt")

(provide lambertian% metal% dielectric%)

(define material%
  (class object%
    (init albedo)
    (define albedo-field albedo)
    (super-new)
    (define/public (get-attenuation)
      albedo-field)))

(define lambertian%
  (class material%
    (super-new)

    (define/public (scatter r rec)
      (let ([scatter-direction (vec-add (hit-record-normal rec)
                                        (random-unit-vector))])
        ;; Catch degenerate scatter direction
        (if (near-zero? scatter-direction)
            (ray (hit-record-p rec) (hit-record-normal rec))
            (ray (hit-record-p rec) scatter-direction))))))

(define metal%
  (class material%
    (init albedo fuzz)
    (define albedo-field albedo)
    (define fuzz-field (if (< fuzz 1) fuzz 1))
    (super-new [albedo albedo-field])

    (define/public (scatter r rec)
      (let ([reflected (reflect (unit-vector (ray-direction r))
                                (hit-record-normal rec))])
        (if (positive? (vec-dot reflected (hit-record-normal rec)))
            (ray (hit-record-p rec)
                 (vec-add reflected (vec-mul-val (random-in-unit-sphere) fuzz-field)))
            null)))))

(define dielectric%  ;; glass
  (class material%
    (init index-of-refraction)
    (define ir index-of-refraction)
    (super-new [albedo #(1 1 1)])

    (define/public (scatter r rec)
      ;; https://en.wikipedia.org/wiki/Snell's_law
      (let* ([refraction-ratio (if (hit-record-front-face rec) (/ 1 ir) ir)]
             [unit-direction (unit-vector (ray-direction r))]
             [cos-theta (min 1 (vec-dot (vec-neg unit-direction) (hit-record-normal rec)))]
             [sin-theta (sqrt (- 1 (* cos-theta cos-theta)))]
             [direction (if (or (> (* refraction-ratio sin-theta) 1)
                                (> (reflactance cos-theta refraction-ratio) (random)))
                            (reflect unit-direction (hit-record-normal rec))  ;; total internal reflection
                            (refract unit-direction (hit-record-normal rec) refraction-ratio))])
        (ray (hit-record-p rec) direction)))

    ;; Use Schlick's approximation for reflectance
    ;; https://en.wikipedia.org/wiki/Schlick%27s_approximation
    (define/private (reflactance cos-theta refraction-ratio)
      (let* ([r (/ (- 1 refraction-ratio) (add1 refraction-ratio))]
             [r0 (* r r)])
        (+ r0
           (* (- 1 r0)
              (expt (- 1 cos-theta) 5)))))))
