(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme file)
        (srfi 14)
        (srfi 98)
        (srfi 115)
        (windows srfi 138))

(define (read-commands file)
  (if (file-exists? file)
      (begin
        (display "Found config file: ")
        (display file)
        (newline)
        (call-with-input-file
            file
          (lambda (p)
            (let kernel ((contents '())
                         (line (read-line p)))
              (if (eof-object? line)
                  (reverse contents)
                  (kernel (cons line contents) (read-line p)))))))
      (error "read-commands: config file not found")))

(define (parse-file-contents file-contents)
  ; Build the charsets we'll use to parse the file
  (let* ((ascii-no-whitespace (char-set-intersection char-set:ascii
                                                     char-set:graphic))
         (reserved (list->char-set '(#\" #\* #\/ #\: #\< #\> #\? #\\ #\| #\;)))
         (filename-chars (char-set-difference ascii-no-whitespace reserved))
         (path-chars (char-set-adjoin filename-chars #\space)))
    ; Set up regexes to grab the necessary information from the file
    (let* ((input-regex (regexp `(: "-pgm" #\space ($ (+ ,filename-chars) ".scm"))))
           (output-regex (regexp `(: "-o" #\space ($ (+ ,filename-chars)) ".exe")))
           (path `($ (: ,char-set:letter ":\\" (* (+ ,path-chars) (? #\\)))))
           (libs `(: ,path (* (: #\; ,path))))
           (before-libs-regex (regexp `(: "-I" #\space ,libs)))
           (after-libs-regex (regexp `(: "-A" #\space ,libs))))
      (display "Built regexes")
      (newline)
      ; Look through the file contents for each regex (should I use map here?)
      (let ((input-file
             (find-in-list file-contents input-regex))
            (output-file
             (find-in-list file-contents output-regex))
            (before-libs
             (find-in-list file-contents before-libs-regex))
            (after-libs
             (find-in-list file-contents after-libs-regex)))
        (display "Parsed file -- starting compilation")
        (newline)
        ; Basic error handling
        (cond ((null? input-file)
               (error "parse-file-contents: please specify a file to compile"))
              ((> (length input-file) 1)
               (error "parse-file-contents: cannot compile multiple files at once"))
              ((> (length output-file) 1)
               (error "parse-file-contents: please specify a single output file"))
              (else
               (compile input-file output-file before-libs after-libs)))))))

;; Start compilation
(define (compile input-file output-file before-libs after-libs)
  (let ((input-file (caar (matches->lists input-file)))
        (output-file (if (not (null? output-file))
                         (caar (matches->lists output-file))
                         #f))
        (before-libs (if (not (null? before-libs))
                         (add-quotes (flatten (matches->lists before-libs)))
                         '()))
        (after-libs (if (not (null? after-libs))
                        (add-quotes (flatten (matches->lists after-libs)))
                        '())))
    (display "Calling compile-r7rs ")
    (display input-file)
    (display " ")
    (display output-file)
    (display " ")
    (display before-libs)
    (display " ")
    (display after-libs)
    (newline)
    (compile-r7rs input-file output-file before-libs after-libs)))

;; List Helper Functions
(define (add-quotes list)
  (let kernel ((list list)
               (quoted-list '()))
    (if (null? list)
        quoted-list
        (kernel (cdr list) (cons (string-append "\"" (string-append
                                                     (car list)
                                                     "\""))
                                 quoted-list)))))

(define (flatten list)
  (let kernel ((list list)
               (stack '())
               (fixed-list '()))
    (cond ((and (null? list) (null? stack)) fixed-list)
          ((not (null? stack))
           (kernel list (cdr stack) (cons (car stack) fixed-list)))
          ((list? (car list))
           (kernel (cdr list) (car list) fixed-list))
          (else
           (kernel (cdr list) stack (cons (car list) fixed-list))))))
           
(define (purge-false list)
  (let kernel ((list list)
               (response '()))
    (cond ((null? list) response)
          ((eqv? #f (car list))
           (kernel (cdr list) response))
          (else
           (kernel (cdr list) (cons (car list) response))))))

;; Regex functions
(define (matches->lists matches)
  (map (lambda (item)
         (purge-false (cdr (regexp-match->list item))))
       matches))

(define (find-in-list list regex)
  (let* ((matching (lambda (str) (regexp-matches regex str)))
         (result (map matching list)))
    (purge-false result)))

(parse-file-contents (read-commands (get-environment-variable "config")))
