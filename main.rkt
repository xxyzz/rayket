#lang racket/base

(require racket/class)
(require "color.rkt" "vec3.rkt" "ray.rkt" "hittable.rkt" "sphere.rkt" "camera.rkt" "material.rkt")

(define (ray-color r world depth)
  (if (zero? depth)
      #(0 0 0)
      (let ([rec (hit-any? world r 0.001 +inf.0)])
        (if (not (null? rec))
            (let ([scatter-ray (send (hit-record-material rec) scatter r rec)])
              (if (not (null? scatter-ray))
                  (vec-mul (ray-color scatter-ray world (sub1 depth))
                           (send (hit-record-material rec) get-attenuation))
                  #(0 0 0)))
            (let* ([unit-direction (unit-vector (ray-direction r))]
                   [t (/ (add1 (vec-y unit-direction)) 2)]) ;; 0 <= t <= 1
              (vec-add (vec-mul-val #(1 1 1) (- 1 t))       ;; blend white and blue
                       (vec-mul-val (vector 0.5 0.7 1) t)))))))

;; Image
(define image-width 400)
(define image-height (floor (/ image-width aspect-ratio)))
(define samples-per-pixel 100)
(define max-depth 50)

(define world
  (list (new sphere%  ;; ground
             [center #(0 -100.5 -1)]
             [radius 100]
             [material (new lambertian% [albedo #(0.8 0.8 0)])])
        (new sphere%  ;; center
             [center #(0 0 -1)]
             [radius 0.5]
             [material (new lambertian% [albedo #(0.1 0.2 0.5)])])
        (new sphere%  ;; left
             [center #(-1 0 -1)]
             [radius 0.5]
             [material (new dielectric% [index-of-refraction 1.5])])
        (new sphere% ;; right
             [center #(1 0 -1)]
             [radius 0.5]
             [material (new metal% [albedo #(0.8 0.6 0.2)] [fuzz 0.3])])))

;; Render
;; portable pixmap format: https://en.wikipedia.org/wiki/Netpbm
(display (format "P3\n~a ~a\n~a\n" image-width image-height max-color-value))
(for* ([j (in-range (sub1 image-height) -1 -1)]
       [i (in-range image-width)])
  (when (zero? i)
    (display (format "Scanlines remaining: ~a\n" j) (current-error-port)))
  ;; anti-aliasing
  (write-color (for/fold ([pixel-color #(0 0 0)])
                         ([_ (in-range samples-per-pixel)])
                 (let ([u (/ (+ i (random)) (sub1 image-width))]
                       [v (/ (+ j (random)) (sub1 image-height))])
                   (vec-add pixel-color
                            (ray-color (camera-get-ray u v) world max-depth))))
               samples-per-pixel))
(display "Done.\n" (current-error-port))
