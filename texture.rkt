#lang racket/base

(require racket/flonum racket/class)

(provide checker-texture%)

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
