#lang racket/base

(require racket/flonum)
(require racket/math)  ;; degrees->radians
(require "vec3.rkt" "ray.rkt")

(provide camera-get-ray aspect-ratio)

(define aspect-ratio (fl/ 16.0 9.0))
(define vfov 20.0)  ;; vertical field-of-view in degrees
(define theta (degrees->radians vfov))
(define viewport-height (fl* 2.0 (fltan (fl/ theta 2.0))))
(define viewport-width (fl* viewport-height aspect-ratio))

(define lookfrom (flvector 13.0 2.0 3.0))
(define lookat (flvector 0.0 0.0 0.0))
(define vup (flvector 0.0 1.0 0.0))

(define w (unit-vector (vec-sub lookfrom lookat)))
(define u (unit-vector (vec-cross vup w)))
(define v (vec-cross w u))

(define origin lookfrom)
(define focus-dist 10.0)
(define aperture 0.1)
(define lens-radius (fl/ aperture 2.0))

(define horizontal (vec-mul-val u (fl* viewport-width focus-dist)))
(define vertical (vec-mul-val v (fl* viewport-height focus-dist)))
(define lower-left-corner
  (vec-sub origin
           (vec-div-val horizontal 2.0)
           (vec-div-val vertical 2.0)
           (vec-mul-val w focus-dist)))

(define time0 0.0)
(define time1 1.0)

(define (camera-get-ray s t)
  (let* ([rd (vec-mul-val (random-in-unit-disk) lens-radius)]
         [offset (vec-add (vec-mul-val u (vec-x rd))
                          (vec-mul-val v (vec-y rd)))])
    (ray (vec-add origin offset)
         (vec-add lower-left-corner
                  (vec-mul-val horizontal s)
                  (vec-mul-val vertical t)
                  (vec-neg origin)
                  (vec-neg offset))
         (random-inexact-range time0 time1))))
