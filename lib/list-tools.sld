(define-library (lib list-tools)
  (import (scheme base)
          (srfi 27))
  (export remove-item shuffle-list)
  (begin
    ;; Remove an item at a particular index from a list
    (define (remove-item lst index)
      (let kernel ((head '())
                   (lst lst)
                   (n index))
        (cond ((eq? 0 n)
               (let inner ((head head)
                           (lst (cdr lst)))
                 (if (null? head)
                     lst
                     (inner (cdr head)
                            (cons (car head) lst)))))
              (else
               (kernel (cons (car lst) head)
                       (cdr lst)
                       (- n 1))))))
    ;; Shuffle a list using Fischer-Yates Shuffle
    (define (shuffle-list lst)
      (let kernel ((lst lst)
                   (shuffled '()))
        (if (not (null? lst))
            (let ((rand (random-integer (length lst))))
              (kernel (remove-item lst rand)
                      (cons (list-ref lst rand) shuffled)))
            shuffled)))))
