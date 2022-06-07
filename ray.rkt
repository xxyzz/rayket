#lang racket/base

(require "vec3.rkt")

(provide ray-at (struct-out ray))

(struct ray (origin direction time))

(define (ray-at r t)
  (vec-add (ray-origin r)
           (vec-mul-val (ray-direction r) t)))
