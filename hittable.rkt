#lang racket/base

(require racket/class)
(require "aabb.rkt")

(provide (struct-out hit-record) hit-any? combine-bounding-boxes)

(struct hit-record (p normal t front-face material u v))

(define (hit-any? hittable-objects r t-min t-max)
  (get-hit-record hittable-objects r t-min t-max null))

(define (get-hit-record hittable-objects r t-min t-max closest-hit-record)
  (if (null? hittable-objects)
      closest-hit-record
      (let ([new-hit-record (send (car hittable-objects) hit r t-min t-max)])
        (if (null? new-hit-record)
            (get-hit-record (cdr hittable-objects) r t-min t-max closest-hit-record)
            (get-hit-record (cdr hittable-objects) r t-min (hit-record-t new-hit-record) new-hit-record)))))

(define (combine-bounding-boxes hittable-objects time0 time1)
  (combine-boxes hittable-objects time0 time1 null))

(define (combine-boxes hittable-objects time0 time1 combined-box)
  (if (null? hittable-objects)
      combined-box
      (let* ([current-box (send (car hittable-objects) bounding-box time0 time1)]
             [new-combined-box
              (cond [(null? current-box) combined-box]
                    [(null? combined-box) current-box]
                    [else (surrounding-box combined-box current-box)])])
        (combine-boxes (cdr hittable-objects) time0 time1 new-combined-box))))
