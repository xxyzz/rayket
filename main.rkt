#lang racket/base

(require racket/class racket/flonum)
(require "color.rkt" "vec3.rkt" "ray.rkt" "hittable.rkt" "sphere.rkt" "camera.rkt" "material.rkt")

(define (ray-color r world depth)
  (if (zero? depth)
      (flvector 0.0 0.0 0.0)
      (let ([rec (hit-any? world r 0.001 +inf.0)])
        (if (not (null? rec))
            (let ([scatter-ray (send (hit-record-material rec) scatter r rec)])
              (if (not (null? scatter-ray))
                  (vec-mul (ray-color scatter-ray world (sub1 depth))
                           (send (hit-record-material rec) get-attenuation))
                  (flvector 0.0 0.0 0.0)))
            ;; blend white and blue
            (let* ([unit-direction (unit-vector (ray-direction r))]
                   [t (fl/ (add1 (vec-y unit-direction)) 2.0)]) ;; 0 <= t <= 1
              (vec-add (vec-mul-val (flvector 1.0 1.0 1.0) (fl- 1.0 t))
                       (vec-mul-val (flvector 0.5 0.7 1.0) t)))))))

;; Image
(define image-width 1200.0)
(define image-height (flfloor (fl/ image-width aspect-ratio)))
(define samples-per-pixel 100.0)
(define max-depth 50.0)

(define big-spheres
  (list (new sphere%  ;; ground
             [center (flvector 0.0 -1000.0 0.0)]
             [radius 1000.0]
             [material (new lambertian% [albedo (flvector 0.5 0.5 0.5)])])
        (new sphere%  ;; center
             [center (flvector 0.0 1.0 0.0)]
             [radius 1.0]
             [material (new dielectric% [index-of-refraction 1.5])])
        (new sphere%  ;; left
             [center (flvector -4.0 1.0 0.0)]
             [radius 1.0]
             [material (new lambertian% [albedo (flvector 0.1 0.2 0.5)])])
        (new sphere% ;; right
             [center (flvector 4.0 1.0 0.0)]
             [radius 1.0]
             [material (new metal% [albedo (flvector 0.7 0.6 0.5)] [fuzz 0.0])])))

(define (random-small-spheres)
  (for*/list ([a (in-range -11.0 11.0)]
              [b (in-range -11.0 11.0)])
    (let ([choose-mat (random)]
          [center (flvector (fl+ a (fl* 0.9 (random)))
                            0.2
                            (fl+ b (fl* 0.9 (random))))])
      (when (fl> (vec-length (vec-sub center (flvector 4.0 0.2 0.0))) 0.9)
        (cond [(fl< choose-mat 0.8)  ;; diffuse
               (new sphere%
                    [center center]
                    [radius 0.2]
                    [material (new lambertian%
                                   [albedo (vec-mul (random-vec) (random-vec))])])]
              [(fl< choose-mat 0.95) ;; metal
               (new sphere%
                    [center center]
                    [radius 0.2]
                    [material (new metal%
                                   [albedo (random-vec-range 0.5 1.0)]
                                   [fuzz (random-inexact-range 0.0 0.5)])])]
              [else  ;; glass
               (new sphere%
                    [center center]
                    [radius 0.2]
                    [material (new dielectric% [index-of-refraction 1.5])])])))))

(define world (append big-spheres
                      (filter (lambda (x) (not (void? x)))
                              (random-small-spheres))))

;; Render
;; portable pixmap format: https://en.wikipedia.org/wiki/Netpbm
(display (format "P3\n~a ~a\n~a\n"
                 (fl->exact-integer image-width)
                 (fl->exact-integer image-height)
                 (fl->exact-integer max-color-value)))
(for* ([j (in-range (sub1 image-height) -1.0 -1.0)]
       [i (in-range 0.0 image-width)])
  (when (zero? i)
    (display (format "Scanlines remaining: ~a\n" j) (current-error-port)))
  ;; anti-aliasing
  (write-color (for/fold ([pixel-color (flvector 0.0 0.0 0.0)])
                         ([_ (in-range samples-per-pixel)])
                 (let ([u (fl/ (fl+ i (random)) (sub1 image-width))]
                       [v (fl/ (fl+ j (random)) (sub1 image-height))])
                   (vec-add pixel-color
                            (ray-color (camera-get-ray u v) world max-depth))))
               samples-per-pixel))
(display "Done.\n" (current-error-port))
