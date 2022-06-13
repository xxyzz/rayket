#lang racket/base

(require racket/flonum racket/class)
(require "perlin.rkt" "vec3.rkt")

(provide checker-texture% noise-texture%)

(define checker-texture%
  (class object%
    (init-field color0 color1)
    (super-new)

    (define/public (value u v p)
      (let ([sin-product (for/product ([val p])
                           (flsin (fl* 10.0 val)))])
        (if (negative? sin-product)
            color0
            color1)))))

(define noise-texture%
  (class object%
    (init-field scale)
    (field [perlin-obj (new perlin%)])
    (super-new)

    (define/public (value u v p)
      (vec-mul-val (flvector 1.0 1.0 1.0)
                   (fl* 0.5  ;; fix nan result for `flsqrt` in gamma correction
                        (fl+ 1.0
                             (flsin (fl+
                                     (fl* scale (vec-z p))
                                     (fl* 10.0 (send perlin-obj turb p))))))))))
