(import (color) (swish mat) (swish testing) (theme))

(define config-entries
  '((color "ib-red" "255,0,0")
    (color "Gray" "808080")
    (color "bad" "nope")
    (color 1 "2,3,4")
    (theme "ocean" "0,0,64" "ffffff")
    (theme "night" "ib-red" #f)
    (bogus entry)))

(define (make-tmp-dir)
  (path-combine (or (getenv "TMPDIR") "/tmp")
    (format "theme-mat-~a" (uuid->string (osi_make_uuid)))))

(define (write-file fn . exprs)
  (let ([op (open-file-to-replace (make-directory-path fn))])
    (on-exit (close-port op)
      (for-each (lambda (x) (write x op) (newline op)) exprs))))

(define (config-file dir)
  (path-combine dir "config" "theme" "config.ss"))

;; Calls (f dir) with a fresh directory holding config/theme/config.ss
;; and removes the directory afterward.
(define (with-tmp-config f)
  (let ([dir (make-tmp-dir)])
    (on-exit (delete-tree dir)
      (apply write-file (config-file dir) config-entries)
      (f dir))))

;; Calls (f dir) with the theme parameters bound to a fresh directory
;; and the 'db process running on a freshly imported database.
(define (with-theme-db f)
  (with-tmp-config
   (lambda (dir)
     (parameterize ([config-dir (path-combine dir "config")]
                    [cache-dir (path-combine dir "cache")]
                    [session-id "7"]
                    [fg-strategy "threshold"]
                    [use-time? #f]
                    [verbose? #f])
       (on-exit (db:stop 'db)
         (setup-db)
         (f dir))))))

(define-syntax capture
  (syntax-rules ()
    [(_ e0 e1 ...) (with-output-to-string (lambda () e0 e1 ...))]))

(define (osc n hex) (format "~c]~d;#~a~c" #\esc n hex #\bel))
(define (bg-osc hex) (osc 11 hex))
(define (fg-osc hex) (osc 10 hex))

;; Returns the session details for id as JSON strings, oldest first.
(define session-rows
  (case-lambda
   [() (session-rows (session-id))]
   [(id)
    (map
     (lambda (row)
       (match row
         [#(,details)
          (json:object->string (json:bytevector->object details))]))
     (transaction 'db
       (execute "select details from sessions where id=? order by timestamp" id)))]))

(define (names table)
  (map
   (lambda (row) (match row [#(,name) name]))
   (transaction 'db
     (execute (format "select name from ~a order by name" table)))))

;; Evaluates the body, discarding anything it prints.
(define-syntax quietly
  (syntax-rules ()
    [(_ e0 e1 ...)
     (parameterize ([current-output-port (open-output-string)]) e0 e1 ...)]))

(define (exit-reason thunk)
  (match (catch (thunk))
    [#(EXIT ,reason) reason]))

(define (error-text reason)
  (apply format (condition-message reason) (condition-irritants reason)))

(define (keyed-hex key)
  (color->hex
   (hsv (modulo (apply + (map char->integer (string->list key))) 360) 1 0.3)))

;; Returns the output of with-preview given SGR parameters for each
;; color it sets, followed by text.
(define (preview . args)
  (let ([args (reverse args)])
    (format "~c[0m~{~a~}~a~c[0m"
      #\esc
      (map (lambda (sgr) (format "~c[~am" #\esc sgr)) (reverse (cdr args)))
      (car args)
      #\esc)))

;; Distinct timestamps keep "order by timestamp" deterministic.
(define (tick) (sleep-ms 2))

(isolate-mat setup-db-imports-config ()
  (with-theme-db
   (lambda (dir)
     (assert (equal? (names "colors") '("Gray" "ib-red")))
     (assert (equal? (names "themes") '("night" "ocean")))
     (match (transaction 'db
              (execute "select details from colors where name='Gray'"))
       [(#(,details))
        (assert (equal? (->color details) (rgb 128 128 128)))
        (let ([obj (json:bytevector->object details)])
          (assert (= (json:ref obj 'v #f) 128.0))
          (assert (= (json:ref obj 's #f) 0.0)))]))))

(isolate-mat setup-db-reimports-on-change ()
  (with-theme-db
   (lambda (dir)
     (db:stop 'db)
     ;; Unchanged mtime: entries deleted from the DB stay deleted.
     (setup-db)
     (transaction 'db (execute "delete from colors"))
     (db:stop 'db)
     (setup-db)
     (assert (equal? (names "colors") '()))
     (db:stop 'db)
     ;; Changed mtime: the config is imported again.
     (sleep-ms 1100)
     (write-file (config-file dir) '(color "teal" "0,128,128"))
     (setup-db)
     (assert (equal? (names "colors") '("teal"))))))

(isolate-mat setup-db-without-config ()
  (let ([dir (make-tmp-dir)])
    (on-exit (delete-tree dir)
      (parameterize ([config-dir (path-combine dir "config")]
                     [cache-dir (path-combine dir "cache")])
        (on-exit (db:stop 'db)
          (assert (eq? (setup-db) 'ok))
          (assert (equal? (names "colors") '()))
          (assert (file-exists? (path-combine dir "cache" "theme" "theme.db3"))))))))

(isolate-mat choose-fg ()
  (parameterize ([fg-strategy "threshold"])
    (assert (equal? (choose-fg (rgb 180 0 0)) (rgb 255 255 255)))
    (assert (equal? (choose-fg (rgb 181 0 0)) (rgb 30 30 30)))
    (assert (equal? (choose-fg (rgb 0 0 0)) (rgb 255 255 255))))
  (parameterize ([fg-strategy "complementary"])
    (assert (equal? (choose-fg (rgb 1 2 3)) (rgb 254 253 252))))
  (parameterize ([fg-strategy "bogus"])
    (assert (equal? (error-text (exit-reason (lambda () (choose-fg (rgb 0 0 0)))))
              "Unknown THEME_FG value \"bogus\""))))

(isolate-mat set-rgb-color ()
  (assert (equal? (capture (set-rgb-color 'bg (rgb 1 2 255))) (bg-osc "0102FF")))
  (assert (equal? (capture (set-rgb-color 'fg (rgb 0 0 0))) (fg-osc "000000"))))

(isolate-mat with-preview ()
  (define (esc fmt . args) (apply format (string-append "~c" fmt) #\esc args))
  (assert (equal? (capture (with-preview (rgb 1 2 3) (rgb 4 5 6) (lambda () (display "x"))))
            (string-append (esc "[0m") (esc "[48;2;1;2;3m") (esc "[38;2;4;5;6m")
              "x" (esc "[0m"))))
  (assert (equal? (capture (with-preview #f #f (lambda () (display "x"))))
            (string-append (esc "[0m") "x" (esc "[0m")))))

(isolate-mat set-theme* ()
  (with-theme-db
   (lambda (dir)
     (assert (equal? (capture (set-theme* (rgb 1 2 3) (rgb 4 5 6)))
               (string-append (bg-osc "010203") (fg-osc "040506"))))
     (tick)
     (assert (equal? (capture (set-theme* (rgb 1 2 3) `#(auto ,(rgb 4 5 6)) "mine" #t))
               (string-append (bg-osc "010203") (fg-osc "040506"))))
     (tick)
     (assert (equal? (capture (set-theme* #f #f)) ""))
     (assert (equal? (capture (set-theme* (rgb 7 8 9) #f #f)) (bg-osc "070809")))
     (assert (equal? (session-rows)
               '("{\"bg\":\"010203\",\"fg\":\"040506\"}"
                 "{\"bg\":\"010203\",\"fg\":\"040506\",\"fg-auto?\":true,\"name\":\"mine\"}"
                 "{}"))))))

(isolate-mat set-theme-color ()
  (with-theme-db
   (lambda (dir)
     (assert (equal? (capture (set-theme "ff0000"))
               (string-append (bg-osc "FF0000") (fg-osc "1E1E1E"))))
     (tick)
     (assert (equal? (capture (set-theme "0,0,64"))
               (string-append (bg-osc "000040") (fg-osc "FFFFFF"))))
     (tick)
     (assert (equal? (capture (set-theme "ib-red"))
               (string-append (bg-osc "FF0000") (fg-osc "1E1E1E"))))
     (assert (equal? (session-rows)
               '("{\"bg\":\"FF0000\",\"fg\":\"1E1E1E\",\"fg-auto?\":true}"
                 "{\"bg\":\"000040\",\"fg\":\"FFFFFF\",\"fg-auto?\":true}"
                 "{\"bg\":\"FF0000\",\"fg\":\"1E1E1E\",\"fg-auto?\":true}"))))))

(isolate-mat set-theme-named-theme ()
  (with-theme-db
   (lambda (dir)
     (assert (equal? (capture (set-theme "OCEAN"))
               (string-append (bg-osc "000040") (fg-osc "FFFFFF"))))
     (tick)
     (assert (equal? (capture (set-theme "night")) (bg-osc "FF0000")))
     (assert (equal? (session-rows)
               '("{\"bg\":\"000040\",\"fg\":\"FFFFFF\",\"name\":\"ocean\"}"
                 "{\"bg\":\"FF0000\",\"name\":\"night\"}"))))))

(isolate-mat set-theme-two-refs ()
  (with-theme-db
   (lambda (dir)
     (assert (equal? (capture (set-theme "000040" "Gray"))
               (string-append (bg-osc "000040") (fg-osc "808080"))))
     (assert (equal? (session-rows) '("{\"bg\":\"000040\",\"fg\":\"808080\"}")))
     (assert (equal? (quietly (exit-reason (lambda () (set-theme "000040" "nope"))))
               '#(unrecognized "nope")))
     (assert (equal? (quietly (exit-reason (lambda () (set-theme "nope" "000040"))))
               '#(unrecognized "nope"))))))

(isolate-mat set-theme-unrecognized ()
  (with-theme-db
   (lambda (dir)
     (let* ([op (open-output-string)]
            [reason (parameterize ([current-output-port op])
                      (exit-reason (lambda () (set-theme "ib"))))])
       (assert (equal? reason '#(unrecognized "ib")))
       (match (split (get-output-string op) #\newline)
         [(,_ "Use a comma separated color triple, a hexadecimal color triple, or one of:"
           ,ib-red "")
          (assert (ends-with? ib-red (format " 255,  0,  0 : ib-red~c[0m" #\esc)))]))
     (let ([op (open-output-string)])
       (parameterize ([current-output-port op])
         (exit-reason (lambda () (set-theme "cea"))))
       (assert (ends-with? (get-output-string op) (format " ocean~c[0m\n" #\esc))))
     (assert (equal? (session-rows) '())))))

(isolate-mat set-theme-delta ()
  (with-theme-db
   (lambda (dir)
     ;; Nothing happens before there is any history.
     (assert (equal? (capture (set-theme "+10")) ""))
     (capture (set-theme "808080"))
     (tick)
     (assert (equal? (capture (set-theme "+60"))
               (string-append (bg-osc "BCBCBC") (fg-osc "1E1E1E"))))
     (tick)
     (assert (equal? (capture (set-theme "-100"))
               (string-append (bg-osc "585858") (fg-osc "FFFFFF"))))
     (tick)
     (assert (equal? (capture (set-theme "0")) ""))
     (assert (equal? (capture (set-theme "+0")) ""))
     ;; Brightness clamps at the ends of the range.
     (assert (equal? (capture (set-theme "-1000"))
               (string-append (bg-osc "000000") (fg-osc "FFFFFF"))))
     (tick)
     (assert (equal? (length (session-rows)) 4)))))

(isolate-mat set-theme-delta-keeps-explicit-fg ()
  (with-theme-db
   (lambda (dir)
     (capture (set-theme "808080" "00ff00"))
     (tick)
     (assert (equal? (capture (set-theme "+60"))
               (string-append (bg-osc "BCBCBC") (fg-osc "00FF00")))))))

(isolate-mat set-theme-reset ()
  (with-theme-db
   (lambda (dir)
     (capture (set-theme "ocean"))
     (tick)
     (capture (set-theme "00ff00"))
     (tick)
     (assert (equal? (capture (set-theme "Reset"))
               (string-append (bg-osc "00FF00") (fg-osc "1E1E1E"))))
     ;; Restoring doesn't add to the history.
     (assert (equal? (length (session-rows)) 2)))))

(isolate-mat restore-theme ()
  (with-theme-db
   (lambda (dir)
     (parameterize ([session-id "8"])
       (capture (set-theme "ocean")))
     (capture (set-theme "00ff00"))
     (assert (equal? (capture (restore-theme '("8")))
               (string-append (bg-osc "000040") (fg-osc "FFFFFF"))))
     (assert (equal? (capture (restore-theme '()))
               (string-append (bg-osc "00FF00") (fg-osc "1E1E1E"))))
     (assert (equal? (error-text (exit-reason (lambda () (restore-theme '("7" "8")))))
               "too many things"))
     ;; With no history, restore falls back to a theme named "default".
     (parameterize ([session-id "9"])
       (assert (equal? (quietly (exit-reason (lambda () (restore-theme '()))))
                 '#(unrecognized "default")))))))

(isolate-mat set-keyed-theme ()
  (with-theme-db
   (lambda (dir)
     (let ([expected (string-append (bg-osc (keyed-hex "abc")) (fg-osc "FFFFFF"))])
       (assert (equal? (keyed-hex "abc") (color->hex (hsv 294 1 0.3))))
       (assert (equal? (capture (set-keyed-theme "abc")) expected))
       (assert (equal? (capture (set-keyed-theme "cba")) expected))
       (assert (not (equal? (capture (set-keyed-theme "abd")) expected)))
       (assert (equal? (capture (set-keyed-theme ""))
                 (string-append (bg-osc "4C0000") (fg-osc "FFFFFF"))))))))

(isolate-mat set-time-theme ()
  (with-theme-db
   (lambda (dir)
     (assert (equal? (capture (set-time-theme 725))
               (capture (set-time-theme 5))))
     (assert (equal? (capture (set-time-theme 0))
               (string-append (bg-osc "4C0000") (fg-osc "FFFFFF"))))
     (assert (equal? (capture (set-time-theme 120))
               (string-append (bg-osc "004C00") (fg-osc "FFFFFF")))))))

(isolate-mat find-themerc ()
  (let ([dir (make-tmp-dir)])
    (on-exit (delete-tree dir)
      (let ([rc (path-combine dir "a" ".themerc")]
            [deep (path-combine dir "a" "b" "c")])
        (write-file rc '(theme "ocean"))
        (make-directory-path (path-combine deep "x"))
        (assert (equal? (find-themerc (path-combine dir "a")) rc))
        (assert (equal? (find-themerc deep) rc))
        (assert (not (find-themerc dir)))))))

(isolate-mat set-file-theme ()
  (with-theme-db
   (lambda (dir)
     (let ([one (path-combine dir "one")]
           [two (path-combine dir "two")]
           [bad (path-combine dir "bad")])
       (write-file one '(theme "ocean"))
       (write-file two '(theme "ff0000" "00ff00"))
       (write-file bad '(colors "ocean"))
       (assert (equal? (capture (set-file-theme one))
                 (string-append (bg-osc "000040") (fg-osc "FFFFFF"))))
       (assert (equal? (capture (set-file-theme two))
                 (string-append (bg-osc "FF0000") (fg-osc "00FF00"))))
       (assert (equal? (error-text (exit-reason (lambda () (set-file-theme bad))))
                 "Unknown theme expression (colors \"ocean\")"))))))

(isolate-mat set-theme-here ()
  (with-theme-db
   (lambda (dir)
     (let ([with-rc (path-combine dir "proj" "src")]
           [without-rc (path-combine dir "other")])
       (write-file (path-combine dir "proj" ".themerc") '(theme "ocean"))
       (make-directory-path (path-combine with-rc "x"))
       (make-directory-path (path-combine without-rc "x"))
       (parameterize ([cd with-rc])
         (assert (equal? (capture (set-theme "here"))
                   (string-append (bg-osc "000040") (fg-osc "FFFFFF")))))
       (parameterize ([cd without-rc])
         (assert (equal? (capture (set-theme "HERE"))
                   (string-append (bg-osc (keyed-hex (cd))) (fg-osc "FFFFFF"))))
         ;; With use-time?, the hue comes from the clock instead of the path.
         (parameterize ([use-time? #t])
           (assert (starts-with? (capture (set-theme "here"))
                     (format "~c]11;#" #\esc)))
           (assert (equal? (length (session-rows)) 3))))))))

(isolate-mat set-local-theme ()
  (with-theme-db
   (lambda (dir)
     (let ([with-rc (path-combine dir "proj")]
           [without-rc (path-combine dir "other")])
       (write-file (path-combine with-rc ".themerc") '(theme "ocean"))
       (make-directory-path (path-combine without-rc "x"))
       (assert (equal? (capture (set-local-theme with-rc))
                 (string-append (bg-osc "000040") (fg-osc "FFFFFF"))))
       (assert (equal? (capture (set-local-theme without-rc))
                 (string-append (bg-osc (keyed-hex (get-real-path without-rc)))
                   (fg-osc "FFFFFF"))))))))

(isolate-mat shell-escape ()
  (assert (equal? (shell-escape "plain-name_1") "plain-name_1"))
  (assert (equal? (shell-escape "a b$c'd") "a\\ b\\$c\\'d"))
  (assert (equal? (shell-escape "x:y=z") "x\\:y\\=z")))

(isolate-mat completions ()
  (with-theme-db
   (lambda (dir)
     (assert (equal? (completions "--r") '("--reset" "--restore")))
     (assert (equal? (completions "-q") '("-q")))
     (assert (equal? (completions "--help") '("--help")))
     (assert (equal? (completions "--x") '()))
     (assert (equal? (completions "o") '("ocean")))
     (assert (equal? (completions "g") '("Gray")))
     (assert (equal? (completions "") '("Gray" "ib-red" "night" "ocean")))
     (assert (equal? (completions "z") '())))))

(isolate-mat print-history ()
  (with-theme-db
   (lambda (dir)
     (assert (equal? (capture (print-history)) ""))
     (capture (set-theme "ocean"))
     (tick)
     (capture (set-theme "night"))
     (assert (equal? (capture (print-history))
               (string-append
                (preview "48;2;0;0;64" "38;2;255;255;255"
                  "{\"bg\":\"000040\",\"fg\":\"FFFFFF\",\"name\":\"ocean\"}")
                "\n"
                (preview "48;2;255;0;0"
                  "{\"bg\":\"FF0000\",\"name\":\"night\"}")
                "\n"))))))

(isolate-mat print-lscolors ()
  (with-theme-db
   (lambda (dir)
     (assert (equal? (capture (print-lscolors "red"))
               (string-append
                (preview "48;2;255;0;0" "     ")
                (preview "38;2;255;0;0" " 255,  0,  0 : ib-red")
                "\n")))
     (assert (equal? (capture (print-lscolors "nothing")) ""))
     ;; The default query lists ib-* colors plus 10 random colors.
     (let ([lines (split (capture (print-lscolors "default")) #\newline)])
       (assert (ends-with? (car lines) (format " 255,  0,  0 : ib-red~c[0m" #\esc)))
       (assert (= (length lines) 4))))))

;; Runs theme:main on args with an environment based on dir (with HOME
;; and THEME_ID unless overridden) plus the alist env. Returns (exit-code
;; stdout).
(define (run-main dir env . args)
  (let ([env (append env
               `(("XDG_CONFIG_HOME" . ,(path-combine dir "config"))
                 ("XDG_CACHE_HOME" . ,(path-combine dir "cache"))
                 ("THEME_ID" . "7")))])
    (define (getenv var)
      (match (assoc var env)
        [(,_ . ,val) val]
        [#f #f]))
    (let* ([code #f]
           [output (capture (set! code (theme:main args getenv)))])
      (list code output))))

(isolate-mat theme:main-help ()
  (with-tmp-config
   (lambda (dir)
     (match (run-main dir '())
       [(0 ,output) (assert (starts-with? output "Usage: "))])
     (match (run-main dir '() "--help")
       [(0 ,output) (assert (starts-with? output "Usage: "))])
     ;; Help doesn't need the database.
     (assert (not (file-exists? (path-combine dir "cache")))))))

(isolate-mat theme:main-inside-emacs ()
  (with-tmp-config
   (lambda (dir)
     (assert (equal? (run-main dir '(("INSIDE_EMACS" . "t")) "ff0000") '(0 "")))
     (assert (not (file-exists? (path-combine dir "cache")))))))

(isolate-mat theme:main-environment-errors ()
  (with-tmp-config
   (lambda (dir)
     (assert (equal? (error-text
                      (exit-reason
                       (lambda () (theme:main '("ff0000") (lambda (var) #f)))))
               "HOME or XDG_CONFIG_HOME environment variable not set"))
     (assert (equal? (error-text
                      (exit-reason
                       (lambda ()
                         (theme:main '("ff0000")
                           (lambda (var) (and (equal? var "HOME") dir))))))
               "Need to export THEME_ID"))
     (assert (equal? (error-text
                      (exit-reason (lambda () (run-main dir '() "--bogus"))))
               "unexpected --bogus")))))

(isolate-mat theme:main-home ()
  (with-tmp-config
   (lambda (dir)
     (assert (equal? (quietly (theme:main '("ff0000")
                                (lambda (var)
                                  (match var
                                    ["HOME" dir]
                                    ["THEME_ID" "7"]
                                    [,_ #f]))))
               0))
     (assert (file-exists? (path-combine dir ".cache" "theme" "theme.db3"))))))

(isolate-mat theme:main-set-and-query ()
  (with-tmp-config
   (lambda (dir)
     (assert (equal? (run-main dir '() "ocean")
               `(0 ,(string-append (bg-osc "000040") (fg-osc "FFFFFF")))))
     (tick)
     (assert (equal? (run-main dir '(("THEME_FG" . "complementary")) "ff0000")
               `(0 ,(string-append (bg-osc "FF0000") (fg-osc "00FFFF")))))
     (tick)
     (assert (equal? (run-main dir '() "ff0000" "Gray")
               `(0 ,(string-append (bg-osc "FF0000") (fg-osc "808080")))))
     (match (run-main dir '() "-q")
       [(0 ,output)
        (assert (equal? (length (split output #\newline)) 4))])
     (assert (equal? (run-main dir '(("THEME_ID" . "8")) "-q") '(0 ""))))))

(isolate-mat theme:main-unrecognized ()
  (with-tmp-config
   (lambda (dir)
     (match (run-main dir '() "bogus")
       [(2 ,output) (assert (starts-with? output "theme: \"bogus\" not recognized.\n"))])
     (match (run-main dir '() "ff0000" "bogus")
       [(2 ,output) (assert (starts-with? output "\"bogus\" not recognized."))])
     (match (exit-reason (lambda () (run-main dir '() "a" "b" "c")))
       [,reason
        (assert (equal? (error-text reason)
                  "Unknown theme arguments (\"a\" \"b\" \"c\")"))]))))

(isolate-mat theme:main-options ()
  (with-tmp-config
   (lambda (dir)
     (assert (equal? (run-main dir '() "--key" "abc")
               `(0 ,(string-append (bg-osc (keyed-hex "abc")) (fg-osc "FFFFFF")))))
     (match (run-main dir '() "--now")
       [(0 ,output) (assert (starts-with? output (format "~c]11;#" #\esc)))])
     (tick)
     (run-main dir '() "ocean")
     (tick)
     (assert (equal? (run-main dir '() "--reset")
               `(0 ,(string-append (bg-osc "000040") (fg-osc "FFFFFF")))))
     (run-main dir '(("THEME_ID" . "8")) "00ff00")
     (assert (equal? (run-main dir '() "--restore" "8")
               `(0 ,(string-append (bg-osc "00FF00") (fg-osc "1E1E1E")))))
     (match (run-main dir '() "--lscolors" "gray")
       [(0 ,output) (assert (ends-with? output (format " 128,128,128 : Gray~c[0m\n" #\esc)))]))))

(isolate-mat theme:main-local ()
  (with-tmp-config
   (lambda (dir)
     (let ([proj (path-combine dir "proj")]
           [other (path-combine dir "other")])
       (write-file (path-combine proj ".themerc") '(theme "ocean"))
       (make-directory-path (path-combine other "x"))
       ;; --local returns before positional refs are applied.
       (assert (equal? (run-main dir '() "--local" proj "ff0000")
                 `(0 ,(string-append (bg-osc "000040") (fg-osc "FFFFFF")))))
       (assert (equal? (run-main dir '() "--local" other)
                 `(0 ,(string-append (bg-osc (keyed-hex (get-real-path other)))
                        (fg-osc "FFFFFF")))))))))

(isolate-mat theme:main-completion ()
  (with-tmp-config
   (lambda (dir)
     (define env '(("THEME_COMPLETION" . "1")))
     (assert (equal? (run-main dir `(("COMP_WORD" . "--re") ,@env)) '(0 "--reset\n--restore\n")))
     (assert (equal? (run-main dir `(("COMP_WORD" . "o") ,@env)) '(0 "ocean\n")))
     (assert (equal? (run-main dir env) '(0 "")))
     ;; Completion still works inside Emacs.
     (assert (equal? (run-main dir `(("COMP_WORD" . "ib") ("INSIDE_EMACS" . "t") ,@env))
               '(0 "ib-red\n"))))))

