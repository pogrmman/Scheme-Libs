;;; Given the pathname for an R7RS top-level program,
;;; the pathname for an executable file to be produced,
;;; a list of directories to be searched before the standard directories,
;;; a list of directories to be searched after the standard directories,
;;; and a list of feature identifiers to be recognized by cond-expand,
;;; compiles the program and produces an executable file that will run
;;; the program.
;;;
;;; The first argument is required.  All others are optional.
;;;
;;; The outfile may be #f, in which case a default outfile is constructed
;;; from the name of the pgm or, as a last resort, assumed to be a.out
;;;
;;; Assumes all libraries needed by the program have been pre-compiled,
;;; don't need to be compiled, or are contained within the program file.
;;;
;;; Works only for Windows systems

(define compile-r7rs
  (case-lambda
   ((pgm)
    (compile-r7rs-shared pgm #f '() '() '()))
   ((pgm outfile)
    (compile-r7rs-shared pgm outfile '() '() '()))
   ((pgm outfile dirs)
    (compile-r7rs-shared pgm outfile dirs '() '()))
   ((pgm outfile dirs dirs2)
    (compile-r7rs-shared pgm outfile dirs dirs2 '()))
   ((pgm outfile dirs dirs2 features)
    (compile-r7rs-shared pgm outfile dirs dirs2 features))))

(define (compile-r7rs-shared pgm outfile dirs dirs2 features)
  (let ((pgmfile (name-of-pgmfile pgm))
        (outfile (name-of-outfile pgm outfile)))
    (assert (not (string=? pgmfile "")))
    (assert (or (not outfile)
                (not (string=? pgmfile outfile))))
    (parameterize ((current-require-path
                    (append dirs (current-require-path) dirs2))
                   (larceny:current-declared-features
                    (append (larceny:current-declared-features)
                            features)))
     (compile-file pgm pgmfile))
    (write-outfile outfile pgmfile dirs dirs2 features)))

;;; SRFI 138 says the .scm suffix is to be treated specially,
;;; with implementations free to do as they like with other suffixes.

(define recognized-program-suffixes
  '(".scm" ".sps"))

;;; Somewhat accidentally, compile-r7rs can be used to compile
;;; libraries as well as programs.  It's almost as easy to support
;;; that properly as to disable it.

(define recognized-library-suffixes
  '(".sld" ".sls"))

(define (file-suffix filename)
  (let* ((suffixes (filter (lambda (suffix)
                             (textual-suffix? suffix filename))
                           (append
                            recognized-program-suffixes
                            recognized-library-suffixes)))
         (suffix (if (null? suffixes) #f (car suffixes))))
    suffix))

(define (file-basename filename)
  (let* ((suffix (file-suffix filename))
         (basename (if suffix
                       (substring filename
                                  0
                                  (- (string-length filename)
                                     (string-length suffix)))
                       filename)))
    basename))

;;; Here we are free to do as we like.
;;; Compiling foo.scm or foo.sps to foo.slfasl doesn't work
;;; because there might be a foo.sld or foo.sls file in the
;;; same directory that had been compiled to foo.slfasl.

(define (name-of-pgmfile pathname)
  (let* ((suffix (file-suffix pathname))
         (basename (if (and suffix
                            (member suffix
                                    recognized-library-suffixes))
                       (file-basename pathname)
                       pathname)))
    (string-append basename ".slfasl")))

;;; Here we have to follow the SRFI 138 spec, but can do as we like
;;; with the .sps and library suffixes.  For library suffixes, we
;;; effectively use /dev/null as the outfile.

(define (name-of-outfile pgm outfile)
  (let ((suffix (file-suffix pgm)))
    (cond (outfile outfile)
          ((and suffix
                (member suffix recognized-library-suffixes))
           #f)
          (suffix (file-basename pgm))
          (else "a.out"))))

(define (write-outfile outfile pgmfile dirs dirs2 features)
  (if outfile
      (begin
				(write-outfile-batch outfile pgmfile dirs dirs2 features)
				(write-sed-file outfile pgmfile))))

(define (write-outfile-batch outfile pgmfile dirs dirs2 features)
	(let ((ofile (string-append outfile ".bat")))
		(delete-file ofile)
		(call-with-output-file
				ofile
			(lambda (p)
				(define (write-opts opt things)
					(for-each (lambda (thing)
											(display opt p)
											(display " " p)
											(display thing p)
											(display " " p))
										things))
				(display "@echo off" p)
				(newline p)
				(display "title " p)
				(display outfile p)
				(newline p)
				(display "larceny --r7rs " p)
				(write-opts "-I" dirs)
				(write-opts "-A" dirs2)
				(write-opts "-D" features)
				(display "--program " p)
				(display pgmfile p)))))

;; Grab the start of the sed file
(define (read-template template)
	(begin
		(let ((file (open-input-file template)))
			(let kernel ((contents '())
									 (obj (read-char file)))
				(if (eof-object? obj)
						(begin
							(close-input-port file)
							(list->string (reverse contents)))
						(kernel (cons obj contents) (read-char file)))))))

(define (write-sed-file outfile pgmfile)
	(let ((exefile (string-append outfile ".exe"))
				(sedfile (string-append outfile ".sed"))
				(batfile (string-append outfile ".bat")))
		(delete-file outfile)
		(call-with-output-file
				sedfile
			(lambda (p)
				(let ((start (read-template "sed_start.sed")))
					(display start p))
				(newline p)
				(display "TargetName=" p)
				(display exefile p)
				(newline p)
				(display "FriendlyName=" p)
				(display outfile p)
				(newline p)
				(display "AppLaunched = cmd /c " p)
				(display batfile p)
				(newline p)
				(display "PostInstallCmd=<None>" p)
				(newline p)
				(display "AdminQuietInstallCmd=" p)
				(newline p)
				(display "UserQuietInstallCmd=" p)
				(newline p)
				(display "FILE0=\"" p)
				(display batfile p)
				(display "\"" p)
				(newline p)
				(display "FILE1=\"" p)
				(display pgmfile p)
				(display "\"" p)
				(newline p)
				(let ((end (read-template "sed_end.sed")))
					(display end p))))))
