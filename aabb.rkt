#lang racket/base

(require racket/flonum)
(require "ray.rkt" "vec3.rkt")

(provide hit-aabb? (struct-out aabb) surrounding-box)

;; Axis-Aligned Bounding Boxes(AABB)
(struct aabb (minimum maximum))

(define (hit-aabb? aabb r t-min t-max)
  (for/and ([min-val (aabb-minimum aabb)]
            [max-val (aabb-maximum aabb)]
            [A (ray-origin r)]
            [b (ray-direction r)])
    (let* ([val-0 (fl/ (fl- min-val A) b)]
           [val-1 (fl/ (fl- max-val A) b)]
           [t0 (flmin val-0 val-1)]
           [t1 (flmax val-0 val-1)]
           [new-t-min (flmax t0 t-min)]
           [new-t-max (flmin t1 t-max)])
      (if (fl<= new-t-max new-t-min)
          #f
          #t))))

(define (surrounding-box box0 box1)
  (aabb (flvector (flmin (vec-x (aabb-minimum box0))
                         (vec-x (aabb-minimum box1)))
                  (flmin (vec-y (aabb-minimum box0))
                         (vec-y (aabb-minimum box1)))
                  (flmin (vec-z (aabb-minimum box0))
                         (vec-z (aabb-minimum box1))))
        (flvector (flmax (vec-x (aabb-maximum box0))
                         (vec-x (aabb-maximum box1)))
                  (flmax (vec-y (aabb-maximum box0))
                         (vec-y (aabb-maximum box1)))
                  (flmax (vec-z (aabb-maximum box0))
                         (vec-z (aabb-maximum box1))))))
