#lang racket/base

(require racket/class)

(provide (struct-out hit-record) hit-any?)

(struct hit-record (p normal t front-face material))

(define (hit-any? hittable-objects r t-min t-max)
  (get-hit-record hittable-objects r t-min t-max null))

(define (get-hit-record hittable-objects r t-min t-max closest-hit-record)
  (if (null? hittable-objects)
      closest-hit-record
      (let ([new-hit-record (send (car hittable-objects) hit? r t-min t-max)])
        (if (null? new-hit-record)
            (get-hit-record (cdr hittable-objects) r t-min t-max closest-hit-record)
            (get-hit-record (cdr hittable-objects) r t-min (hit-record-t new-hit-record) new-hit-record)))))
