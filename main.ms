(import (swish mat) (swish testing))

;; These mats run main.ss in a subprocess. Tests of the (theme) library,
;; including theme:main, are in theme.ms.

(define (make-tmp-dir)
  (path-combine (or (getenv "TMPDIR") "/tmp")
    (format "theme-mat-~a" (uuid->string (osi_make_uuid)))))

(define (write-file fn . exprs)
  (let ([op (open-file-to-replace (make-directory-path fn))])
    (on-exit (close-port op)
      (for-each (lambda (x) (write x op) (newline op)) exprs))))

;; Calls (f dir) with a fresh directory holding config/theme/config.ss
;; and removes the directory afterward.
(define (with-tmp-config f)
  (let ([dir (make-tmp-dir)])
    (on-exit (delete-tree dir)
      (write-file (path-combine dir "config" "theme" "config.ss")
        '(color "ib-red" "255,0,0")
        '(theme "ocean" "0,0,64" "ffffff"))
      (f dir))))

(define (osc n hex) (format "~c]~d;#~a~c" #\esc n hex #\bel))
(define (bg-osc hex) (osc 11 hex))
(define (fg-osc hex) (osc 10 hex))

;; Variables that run-script sets or removes from the inherited
;; environment.
(define variables
  '("HOME" "INSIDE_EMACS" "THEME_COMPLETION" "COMP_WORD" "THEME_FG" "THEME_ID"
    "XDG_CACHE_HOME" "XDG_CONFIG_HOME"))

;; Runs main.ss on args and returns (exit-status stdout-lines
;; stderr-lines). Of the variables above, the subprocess sees only those
;; in env, an alist, plus defaults for THEME_ID and XDG_* based on dir.
;; A #f value in env leaves that variable unset.
(define (run-script dir env . args)
  (let* ([env (append env
                `(("THEME_ID" . "7")
                  ("XDG_CONFIG_HOME" . ,(path-combine dir "config"))
                  ("XDG_CACHE_HOME" . ,(path-combine dir "cache"))))]
         [set (filter (lambda (var) (cond [(assoc var env) => cdr] [else #f]))
                variables)]
         [unset (remp (lambda (var) (member var set)) variables)])
    (match (run-os-process "/usr/bin/env"
             ;; env requires every -u option before the first assignment.
             `(,@(fold-right (lambda (var acc) (cons* "-u" var acc)) '() unset)
               ,@(map (lambda (var) (format "~a=~a" var (cdr (assoc var env)))) set)
               ,(osi_get_executable_path) "main.ss" ,@args)
             (lambda (op) (void))
             (scale-timeout 30000)
             '())
      [`(<os-result> ,stdout ,stderr ,exit-status)
       (list exit-status stdout stderr)])))

(isolate-mat help ()
  (with-tmp-config
   (lambda (dir)
     (match (run-script dir '())
       [(0 ("Usage: main [-hvq] [--now] [--key <key>] [--local <dir>] [options] <ref> ..."
            . ,_)
         ())
        'ok]))))

(isolate-mat inside-emacs ()
  (with-tmp-config
   (lambda (dir)
     (assert (equal? (run-script dir '(("INSIDE_EMACS" . "t")) "ff0000")
               '(0 () ()))))))

(isolate-mat set-color ()
  (with-tmp-config
   (lambda (dir)
     (assert (equal? (run-script dir '() "ff0000")
               `(0 (,(string-append (bg-osc "FF0000") (fg-osc "1E1E1E"))) ())))
     (assert (equal? (run-script dir '() "ocean")
               `(0 (,(string-append (bg-osc "000040") (fg-osc "FFFFFF"))) ())))
     (match (run-script dir '() "-q")
       [(0 (,first ,second) ())
        (assert (pregexp-match (pregexp-quote "{\"bg\":\"FF0000\",\"fg\":\"1E1E1E\",\"fg-auto?\":true}") first))
        (assert (pregexp-match (pregexp-quote "{\"bg\":\"000040\",\"fg\":\"FFFFFF\",\"name\":\"ocean\"}") second))]))))

(isolate-mat theme-fg ()
  (with-tmp-config
   (lambda (dir)
     (assert (equal? (run-script dir '(("THEME_FG" . "complementary")) "ff0000")
               `(0 (,(string-append (bg-osc "FF0000") (fg-osc "00FFFF"))) ()))))))

(isolate-mat unrecognized ()
  (with-tmp-config
   (lambda (dir)
     (assert (equal? (run-script dir '() "bogus")
               '(2 ("main: \"bogus\" not recognized."
                    "Use a comma separated color triple, a hexadecimal color triple, or one of:")
                 ()))))))

(isolate-mat errors ()
  (with-tmp-config
   (lambda (dir)
     (assert (equal? (run-script dir '(("THEME_ID" . #f)) "ff0000")
               '(2 () ("main: Need to export THEME_ID."))))
     (assert (equal? (run-script dir '(("XDG_CONFIG_HOME" . #f)) "ff0000")
               '(2 () ("main: HOME or XDG_CONFIG_HOME environment variable not set."))))
     (assert (equal? (run-script dir '() "--bogus")
               '(2 () ("main: unexpected --bogus.")))))))

(isolate-mat completion ()
  (with-tmp-config
   (lambda (dir)
     (define env '(("THEME_COMPLETION" . "1")))
     (assert (equal? (run-script dir `(("COMP_WORD" . "--re") ,@env))
               '(0 ("--reset" "--restore") ())))
     (assert (equal? (run-script dir `(("COMP_WORD" . "oc") ,@env))
               '(0 ("ocean") ())))
     (assert (equal? (run-script dir env) '(0 () ()))))))
