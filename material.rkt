#lang racket/base

(require racket/class racket/flonum)
(require "vec3.rkt" "ray.rkt" "hittable.rkt")

(provide lambertian% metal% dielectric%)

(define lambertian%
  (class object%
    (init-field albedo)
    (super-new)

    (define/public (scatter r rec)
      (let ([scatter-direction (vec-add (hit-record-normal rec)
                                        (random-unit-vector))]
            [color (if (flvector? albedo)
                       albedo
                       (send albedo value
                             (hit-record-u rec)
                             (hit-record-v rec)
                             (hit-record-p rec)))])

        ;; Catch degenerate scatter direction
        (if (near-zero? scatter-direction)
            (values (ray (hit-record-p rec) (hit-record-normal rec) (ray-time r))
                    color)
            (values (ray (hit-record-p rec) scatter-direction (ray-time r))
                    color))))))

(define metal%
  (class object%
    (init-field albedo fuzz)
    (field [fuzz-field (if (fl< fuzz 1.0) fuzz 1.0)])
    (super-new)

    (define/public (scatter r rec)
      (let ([reflected (reflect (unit-vector (ray-direction r))
                                (hit-record-normal rec))])
        (if (positive? (vec-dot reflected (hit-record-normal rec)))
            (values (ray (hit-record-p rec)
                         (vec-add reflected (vec-mul-val (random-in-unit-sphere) fuzz-field))
                         (ray-time r))
                    albedo)
            (values null null))))))

(define dielectric%  ;; glass
  (class object%
    (init-field index-of-refraction)
    (field [albedo (flvector 1.0 1.0 1.0)])
    (super-new)

    (define/public (scatter r rec)
      ;; https://en.wikipedia.org/wiki/Snell's_law
      (let* ([refraction-ratio (if (hit-record-front-face rec)
                                   (fl/ 1.0 index-of-refraction)
                                   index-of-refraction)]
             [unit-direction (unit-vector (ray-direction r))]
             [cos-theta (flmin 1.0 (vec-dot (vec-neg unit-direction) (hit-record-normal rec)))]
             [sin-theta (flsqrt (fl- 1.0 (fl* cos-theta cos-theta)))]
             [direction (if (or (fl> (fl* refraction-ratio sin-theta) 1.0)
                                (fl> (reflactance cos-theta refraction-ratio) (random)))
                            (reflect unit-direction (hit-record-normal rec))  ;; total internal reflection
                            (refract unit-direction (hit-record-normal rec) refraction-ratio))])
        (values (ray (hit-record-p rec) direction (ray-time r))
                albedo)))

    ;; Use Schlick's approximation for reflectance
    ;; https://en.wikipedia.org/wiki/Schlick%27s_approximation
    (define/private (reflactance cos-theta refraction-ratio)
      (let* ([r (fl/ (fl- 1.0 refraction-ratio) (fl+ 1.0 refraction-ratio))]
             [r0 (fl* r r)])
        (fl+ r0
             (fl* (fl- 1.0 r0)
                  (flexpt (fl- 1.0 cos-theta) 5.0)))))))
