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
(define image-width 1200)
(define image-height (floor (/ image-width aspect-ratio)))
(define samples-per-pixel 100)
(define max-depth 50)

(define big-spheres
  (list (new sphere%  ;; ground
             [center #(0 -1000 0)]
             [radius 1000]
             [material (new lambertian% [albedo #(0.5 0.5 0.5)])])
        (new sphere%  ;; center
             [center #(0 1 0)]
             [radius 1]
             [material (new dielectric% [index-of-refraction 1.5])])
        (new sphere%  ;; left
             [center #(-4 1 0)]
             [radius 1]
             [material (new lambertian% [albedo #(0.1 0.2 0.5)])])
        (new sphere% ;; right
             [center #(4 1 0)]
             [radius 1]
             [material (new metal% [albedo #(0.7 0.6 0.5)] [fuzz 0])])))

(define (random-small-spheres)
  (for*/list ([a (in-range -11 11)]
              [b (in-range -11 11)])
    (let ([choose-mat (random)]
          [center (vector (+ a (* 0.9 (random)))
                          0.2
                          (+ b (* 0.9 (random))))])
      (when (> (vec-length (vec-sub center #(4 0.2 0))) 0.9)
        (cond [(< choose-mat 0.8)  ;; diffuse
               (new sphere%
                    [center center]
                    [radius 0.2]
                    [material (new lambertian%
                                   [albedo (vec-mul (random-vec) (random-vec))])])]
              [(< choose-mat 0.95) ;; metal
               (new sphere%
                    [center center]
                    [radius 0.2]
                    [material (new metal%
                                   [albedo (random-vec-range 0.5 1)]
                                   [fuzz (random-inexact-range 0 0.5)])])]
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
