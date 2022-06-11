#lang racket/base
;; Bounding volume hierarchy

(require racket/class racket/flonum racket/list)
(require "aabb.rkt" "vec3.rkt" "hittable.rkt")

(provide bvh-node%)

(define (compair-box hittable-a hittable-b axis)
  (let ([box-a (send hittable-a bounding-box 0.0 0.0)]
        [box-b (send hittable-b bounding-box 0.0 0.0)])
    (if (or (null? box-a) (null? box-b))
        (error "No bounding box in bvh-node constructor.")
        (fl< (axis (aabb-minimum box-a))
             (axis (aabb-minimum box-b))))))

(define (compair-box-x hittable-a hittable-b)
  (compair-box hittable-a hittable-b vec-x))

(define (compair-box-y hittable-a hittable-b)
  (compair-box hittable-a hittable-b vec-y))

(define (compair-box-z hittable-a hittable-b)
  (compair-box hittable-a hittable-b vec-z))

(define bvh-node%
  (class object%
    (init-field objects time0 time1)
    (field [left null]
           [right null]
           [node-box null])

    (let* ([axis (random 0 3)]
           [comparator (cond [(= axis 0) compair-box-x]
                             [(= axis 1) compair-box-y]
                             [else compair-box-z])]
           [objects-len (length objects)])
      (cond [(= objects-len 1)
             (set-child-nodes (car objects) (car objects))]
            [(= objects-len 2)
             (if (comparator (car objects) (cadr objects))
                 (set-child-nodes (car objects) (cadr objects))
                 (set-child-nodes (cadr objects) (car objects)))]
            [else
             (let* ([sorted-objects (sort objects comparator)]
                    [half-len (quotient objects-len 2)]
                    [left-objects (take sorted-objects half-len)]
                    [right-objetcs (list-tail sorted-objects half-len)]
                    [left-node (new bvh-node%
                                    [objects left-objects]
                                    [time0 time0]
                                    [time1 time1])]
                    [right-node (new bvh-node%
                                     [objects right-objetcs]
                                     [time0 time0]
                                     [time1 time1])])
               (set-child-nodes left-node right-node))])

      (let ([left-box (send left bounding-box time0 time1)]
            [right-box (send right bounding-box time0 time1)])
        (when (or (not left-box) (not right-box))
          (error "No bounding box in bvh-node constructor."))
        (set-field! node-box this (surrounding-box left-box right-box))))

    (super-new)

    (define/private (set-child-nodes left-node right-node)
      (set-field! left this left-node)
      (set-field! right this right-node))

    (define/public (bounding-box t0 t1) node-box)

    (define/public (hit r t-min t-max)
      (if (not (hit-aabb? node-box r t-min t-max))
          null
          (let* ([left-hit-rec (send left hit r t-min t-max)]
                 [right-hit-rec (send right hit r t-min
                                      (if (null? left-hit-rec)
                                          t-max
                                          (hit-record-t left-hit-rec)))])
            (if (not (null? right-hit-rec))
                right-hit-rec
                left-hit-rec))))))
