(define-library (lib list-tools)
  (import (scheme base)
          (srfi 27))
  (export remove-item shuffle-list shuffle-list!)
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
    ;; List numbers up to a particular number, sequentially
    (define (list-up-to n)
      (let kernel ((n (- n 1))
                   (lst '()))
        (if (>= n 0)
            (kernel (- n 1) (cons n lst))
            lst)))
    ;; Shuffle a list using Fischer-Yates Shuffle
    (define (shuffle-list lst)
      (let kernel ((lst lst)
                   (shuffled '()))
        (if (not (null? lst))
            (let ((rand (random-integer (length lst))))
              (kernel (remove-item lst rand)
                      (cons (list-ref lst rand) shuffled)))
            shuffled)))
    ;; Shuffle a list using Fischer-Yates Shuffle, using mutation
    (define (shuffle-list! lst)
      (let ((original (list-copy lst)))
        (let kernel ((order (shuffle-list (list-up-to (length lst))))
                     (n 0))
          (if (not (null? order))
              (let ((item (list-ref original (car order))))
                (list-set! lst n item)
                (kernel (cdr order) (+ n 1)))
              lst))))))
