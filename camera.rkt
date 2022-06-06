#lang racket/base

(require racket/math)  ;; degrees->radians
(require "vec3.rkt" "ray.rkt")

(provide camera-get-ray aspect-ratio)

(define aspect-ratio (/ 3 2))
(define vfov 20)  ;; vertical field-of-view in degrees
(define theta (degrees->radians vfov))
(define viewport-height (* 2 (tan (/ theta 2))))
(define viewport-width (* viewport-height aspect-ratio))

(define lookfrom #(13 2 3))
(define lookat #(0 0 0))
(define vup #(0 1 0))

(define w (unit-vector (vec-sub lookfrom lookat)))
(define u (unit-vector (vec-cross vup w)))
(define v (vec-cross w u))

(define origin lookfrom)
(define focus-dist 10)
(define aperture 0.1)
(define lens-radius (/ aperture 2))

(define horizontal (vec-mul-val u (* viewport-width focus-dist)))
(define vertical (vec-mul-val v (* viewport-height focus-dist)))
(define lower-left-corner
  (vec-sub origin
           (vec-div-val horizontal 2)
           (vec-div-val vertical 2)
           (vec-mul-val w focus-dist)))

(define (camera-get-ray s t)
  (let* ([rd (vec-mul-val (random-in-unit-disk) lens-radius)]
         [offset (vec-add (vec-mul-val u (vec-x rd))
                          (vec-mul-val v (vec-y rd)))])
    (ray (vec-add origin offset)
         (vec-add lower-left-corner
                  (vec-mul-val horizontal s)
                  (vec-mul-val vertical t)
                  (vec-neg origin)
                  (vec-neg offset)))))
