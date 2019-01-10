;; Experimenting with the Y combinator
(define-library (lib Y-comb)
  (import (scheme base))
  (export def-rec Y Y-multi)

  (begin
    ;; Y combinator
    (define Y
      (lambda (f)
        ((lambda (x) (f (lambda (y) ((x x) y))))
         (lambda (x) (f (lambda (y) ((x x) y)))))))

    ;; Y-multi combinator (for multi-arg functions)
    (define Y-multi
      (lambda (f)
        ((lambda (x) (x x))
         (lambda (x) (f (lambda args (apply (x x) args)))))))
    
    ;; Define a recursive function using Y or Y-multi
    (define-syntax def-rec
      (syntax-rules ()
        ((def-rec (name arg) bodies ...)
         (define name
           (Y (lambda (name)
                (lambda (arg)
                  bodies ...)))))
        ((def-rec (name args ...) bodies ...)
         (define name
           (Y-multi
            (lambda (name)
              (lambda (args ...)
                bodies ...)))))))))
