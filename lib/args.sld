(define-library (lib args)
  (import (scheme base)
	  (scheme process-context))
  (export get-args)
  (begin
    (define (get-args)
      (remove-last (cddr (command-line-arguments))))
    (define (remove-last lst)
      (reverse
       (cdr (reverse lst))))))
