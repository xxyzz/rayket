#lang racket/base

(require racket/class)

(provide (struct-out hit-record) hit-any?)

(struct hit-record (p normal t front-face material))

(define (hit-any? hittable-objects r t-min t-max)
  (let ([closest-t t-max]
        [closest-hit-record null])
    (for ([object hittable-objects])
      (let ([current-hit-record (send object hit? r t-min closest-t)])
        (when (not (null? current-hit-record))
          (set! closest-hit-record current-hit-record)
          (set! closest-t (hit-record-t current-hit-record)))))
    closest-hit-record))
