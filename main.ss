(import (color))

(define cli
  (cli-specs
   default-help
   [now --now bool "random color based on current time"]
   [key --key (string "<key>") "a key to generate a deterministic color"]
   [local --local (string "<dir>") "look for a .themerc file in <dir>"]
   [reset --reset bool "reapply the current theme"]
   [restore --restore (list . "<THEME_ID>") "restore the theme associated with THEME_ID"
     (conflicts '(reset))]
   [verbose -v bool "enable verbose output"]
   [query -q bool "show current session history"]
   [rest (list . "<ref>") "A single <ref> is treated as as background color or a theme name. A second <ref> is treated as a foreground color."]
   [lscolors --lscolors (string "<query>") "EXPERIMENT with LS_COLORS" #;(default 'default)]
   ))

(define opt (parse-command-line-arguments cli))

(define completions? (getenv "THEME_COMPLETION"))

(when (and (or (opt 'help) (zero? (hashtable-size (opt))))
           (not completions?))
  (display-help (app:name) cli (opt))
  (exit 0))

(when (and (getenv "INSIDE_EMACS")
           (not completions?))
  (exit 0))

(define verbose? (and (opt 'verbose)))

(define-syntax LOG
  (syntax-rules ()
    [(_ fmt arg ...)
     (when verbose?
       (fprintf (console-error-port) fmt arg ...))]))

(define config-dir
  (cond
   [(getenv "XDG_CONFIG_HOME")]
   [(getenv "HOME") =>
    (lambda (home) (path-combine home ".config"))]
   [else
    (errorf #f "HOME or XDG_CONFIG_HOME environment variable not set")]))

(define cache-dir
  (cond
   [(getenv "XDG_CACHE_HOME")]
   [(getenv "HOME") =>
    (lambda (home) (path-combine home ".cache"))]
   [else
    (errorf #f "HOME or XDG_CACHE_HOME environment variable not set")]))

(define session-id
  (or (getenv "THEME_ID")
      (errorf #f "Need to export THEME_ID")))

(define (scalar x)
  (match x
    [() #f]
    [#(,v) v]
    [(#(,v)) v]))

(define (one x)
  (match x
    [() #f]
    [(,row) row]))

(define (try-import fn)
  (when (file-exists? fn)
    (LOG "attempting import from ~a\n" fn)
    (let ([ip (open-file-to-read fn)])
      (on-exit (close-port ip)
        (let lp ()
          (let ([entry (read ip)])
            (unless (eof-object? entry)
              (match entry
                [(color ,name ,str)
                 (guard (and (string? name) (string? str)))
                 (match (->color str)
                   [#f
                    (LOG "~s: unknown color specification ~s\n" name str)]
                   [`(<color> ,r ,g ,b)
                    (let-values ([(h s v) (rgb->hsv r g b)])
                      (db:log 'db "insert into colors (name,details) values(?,?)"
                        name
                        (json:object->bytevector
                         (json:make-object
                          [r r] [g g] [b b]
                          [h (inexact h)] [s (inexact s)] [v (inexact v)]))))])]
                [(theme ,name ,bg ,fg)
                 (guard (and (string? name)
                             (or (not bg) (string? bg))
                             (or (not fg) (string? fg))))
                 (db:log 'db "insert into themes (name,details) values(?,?)"
                   name
                   (json:object->bytevector
                    (let ([obj (json:make-object)])
                      (when bg (json:extend-object obj [bg bg]))
                      (when fg (json:extend-object obj [fg fg]))
                      obj)))]
                [,_
                 (LOG "unknown specification entry: ~s\n" entry)])
              (lp))))))))

(define (setup-db)
  (db:start 'db (make-directory-path (path-combine cache-dir "theme" "theme.db3")) 'create)
  (match
   (transaction 'db
     (execute
      (ct:join #\space
        "create table if not exists [props] ("
        "[key] text unique collate nocase,"
        "[value] blob,"
        "unique([key]) on conflict replace"
        ")"))
     (execute
      (ct:join #\space
        "create table if not exists [colors] ("
        "[name] text unique collate nocase,"
        "[details] blob,"
        "unique([name]) on conflict replace"
        ")"))
     (execute
      (ct:join #\space
        "create table if not exists [themes] ("
        "[name] text unique collate nocase,"
        "[details] blob,"
        "unique([name]) on conflict replace"
        ")"))
     (create-table sessions
       [timestamp integer]
       [id integer]
       [details blob])
     (scalar (execute "select value from props where key=?" "config_import_timestamp")))
   [,last-timestamp
    (let ([fn (path-combine config-dir "theme" "config.ss")])
      (match (get-stat fn)
        [`(<stat> [mtime (,ts . ,_)])
         (when (or (not last-timestamp)
                   (not (= ts last-timestamp)))
           (try-import fn)
           (db:log 'db "insert into props (key,value) values(?,?)"
             "config_import_timestamp"
             ts))]
        [,_ (void)])
      'ok)]))

(define (details->json details)
  (if (bytevector? details)
      (json:bytevector->object details)
      (json:string->object details)))

(define (details->colors details)
  (let* ([details (details->json details)]
         [bg (json:ref details 'bg #f)]
         [bg (and bg (->color bg))]
         [fg (json:ref details 'fg #f)]
         [fg (and fg (->color fg))])
    (values bg fg)))

(define (restore-theme id)
  (LOG "session ~a, id ~s\n" session-id id)
  (match
   (scalar
    (transaction 'db
      (execute
       (ct:join #\space
         "select details"
         "from sessions"
         "where id=?"
         "order by timestamp desc"
         "limit 1")
       (match id
         [() session-id]
         [(,id) id]
         [,_ (errorf #f "too many things")]))))
   [#f
    (LOG "no theme found\n")
    (set-theme "default")]
   [,details
    (let-values ([(bg fg) (details->colors details)])
      (set-theme* bg fg #f #f))]))

(define (set-time-theme ts)
  (let* ([h (modulo ts 360)]
         [bg (hsv h 1 0.3)])
    (set-theme* bg `#(auto ,(choose-fg bg)))))

(define (set-keyed-theme key)
  (let* ([h (do ([i 0 (+ i 1)]
                 [h 0 (+ h (char->integer (string-ref key i)))])
                ((= i (string-length key)) (modulo h 360)))]
         [bg (hsv h 1 0.3)])
    (set-theme* bg `#(auto ,(choose-fg bg)))))

(define (find-themerc dir)
  (let ([fn (path-combine dir ".themerc")])
    (if (file-exists? fn)
        fn
        (let ([parent (path-parent dir)])
          (if (string=? parent dir)
              #f
              (find-themerc parent))))))

(define (set-file-theme fn)
  (let ([ip (open-file-to-read fn)])
    (on-exit (close-port ip)
      (match (read ip)
        [(theme ,ref)
         (set-theme ref)]
        [(theme ,ref1 ,ref2)
         (set-theme ref1 ref2)]
        [,expr
         (errorf #f "Unknown theme expression ~s" expr)]))))

(define (set-rgb-color type color)
  (<color> open color [r g b])
  (printf "~c]~d;#~2,'0x~2,'0x~2,'0x~c"
    #\esc
    (match type
      [fg 10]
      [bg 11])
    r g b
    #\bel))

(define (choose-fg bg)
  (<color> open bg [r g b])
  (match (or (getenv "THEME_FG") "threshold")
    ["threshold"
     (let-values ([(h s v) (rgb->hsv r g b)])
       ;; Some sources say that using a near 127 value is good, while
       ;; another points to using perceived brightness which leads to a
       ;; threshold of 180.
       (if (<= v 180)
           (rgb 255 255 255)
           (rgb 30 30 30)))]
    ["complementary"
     (rgb (- 255 r) (- 255 g) (- 255 b))]
    [,unhandled
     (errorf #f "Unknown THEME_FG value ~s" unhandled)]))

(define (with-preview bg fg thunk)
  (define (reset)
    (printf "~c[0m" #\esc))
  (define (preview type color)
    (<color> open color [r g b])
    (printf "~c[~d;~d;~d;~d;~dm"
      #\esc
      (match type
        [fg 38]
        [bg 48])
      2 r g b))
  (reset)
  (when bg (preview 'bg bg))
  (when fg (preview 'fg fg))
  (thunk)
  (reset))

(define set-theme*
  (case-lambda
   [(bg fg) (set-theme* bg fg #f #t)]
   [(bg fg log?) (set-theme* bg fg #f log?)]
   [(bg fg name log?)
    (let ([details (json:make-object)])
      (when bg
        (set-rgb-color 'bg bg)
        (json:extend-object details [bg (color->hex bg)]))
      (match fg
        [#f (void)]
        [`(<color>)
         (set-rgb-color 'fg fg)
         (json:extend-object details [fg (color->hex fg)])]
        [#(auto ,fg)
         (set-rgb-color 'fg fg)
         (json:extend-object details [fg (color->hex fg)] [fg-auto? #t])])
      (when log?
        (when name
          (json:extend-object details [name name]))
        (db:log 'db "insert into sessions (timestamp,id,details) values (?,?,?)"
          (erlang:now)
          session-id
          (json:object->bytevector details))))]))

(define set-theme
  (case-lambda
   [(ref)
    (cond
     [(string-ci=? ref "reset")
      (restore-theme '())]
     [(string-ci=? ref "here")
      (let ([dir (cd)])
        (cond
         [(find-themerc dir) => set-file-theme]
         [(opt 'now) (set-time-theme (erlang:now))]
         [else (set-keyed-theme dir)]))]
     [(one
       (transaction 'db
         (execute "select name,details from themes where name=? limit 1" ref))) =>
      (lambda (row)
        (match row
          [#(,name ,details)
           (let-values ([(bg fg) (details->colors details)])
             (set-theme* bg fg name #t))]))]
     [(->color ref) =>
      (lambda (color)
        (set-theme* color `#(auto ,(choose-fg color))))]
     [(and (> (string-length ref) 0)
           (memv (string-ref ref 0) '(#\+ #\- #\0))
           (string->number ref)) =>
      (lambda (delta)
        (cond
         [(zero? delta) (void)]
         [(scalar
           (transaction 'db
             (execute "select details from sessions where id=? order by timestamp desc limit 1"
               session-id))) =>
          (lambda (details)
            (let-values ([(bg fg) (details->colors details)])
              (when bg
                (match-let* ([`(<color> ,r ,g ,b) bg])
                  (let-values ([(h s v) (rgb->hsv r g b)])
                    (let ([new-bg (hsv h s (/ (+ v delta) 255))])
                      (set-theme* new-bg
                        (if (json:ref (details->json details) 'fg-auto? #f)
                            `#(auto ,(choose-fg new-bg))
                            fg))))))))]))]
     [else
      (printf "~a: ~s not recognized.\n" (app:name) ref)
      (printf "Use a comma separated color triple, a hexadecimal color triple, or one of:\n")
      (for-each
       (lambda (row)
         (match row
           [#(,name ,details)
            (let-values ([(bg fg) (details->colors details)])
              (with-preview bg fg
                (lambda () (printf " xyz ")))
              (with-preview bg fg
                (lambda () (printf " ~a" name)))
              (newline))]))
       (let ([query (format "%~a%" ref)])
         (transaction 'db
           (execute "select name,details from themes where name like ? order by name" query))))
      (for-each
       (lambda (row)
         (match row
           [#(,name ,details)
            (let ([color (->color details)])
              (<color> open color [r g b])
              (with-preview color (choose-fg color)
                (lambda () (printf " xyz ")))
              (with-preview #f color
                (lambda () (printf " ~3@a,~3@a,~3@a : ~a" r g b name)))
              (newline))]))
       (let ([query (format "%~a%" ref)])
         (transaction 'db
           (execute "select name,details from colors where name like ? order by name" query))))
      (exit 2)])]
   [(ref1 ref2)
    (let ([bg (->color ref1)]
          [fg (->color ref2)])
      (unless (and bg fg)
        (printf "~s not recognized. Use a comma separated color triple or a hexadecimal color triple, or a named color.\n"
          (if (not bg) ref1 ref2))
        (exit 2))
      (set-theme* bg fg))]))

(let ([eh (exit-handler)])
  (exit-handler
   (lambda args
     (try (db:stop 'db))
     (apply eh args))))
(setup-db)

(when completions?
  (let ()
    (define op
      (open-output-string)
      #;(open-file-to-append "/tmp/debug-info"))
    (define (trace-getenv var)
      (let ([val (getenv var)])
        (fprintf op "~a = ~a\n" var val)
        (flush-output-port op)
        val))
    (define (shell-escape s)
      (let ([op (open-output-string)] [len (string-length s)])
        (do ([i 0 (fx+ i 1)]) ((fx= i len))
          (let ([c (string-ref s i)])
            (when (memv c '(#\\ #\space #\tab #\newline
                            #\$ #\` #\" #\'
                            #\< #\> #\| #\& #\;
                            #\( #\) #\[ #\] #\{ #\}
                            #\* #\? #\! #\# #\~ #\= #\:))
              (write-char #\\ op))
            (write-char c op)))
        (get-output-string op)))
    (let ([word (trace-getenv "COMP_WORD")])
      (cond
       [(not word) (void)]
       [(starts-with? word "-")
        (printf "~{~a\n~}"
          (map shell-escape
            (fold-right
             (lambda (spec acc)
               (<arg-spec> open spec [short long])
               (let* ([short (and short (format "-~a" short))]
                      [acc (if (and short (starts-with? short word))
                               (cons short acc)
                               acc)]
                      [long (and long (format "--~a" long))]
                      [acc (if (and long (starts-with? long word))
                               (cons long acc)
                               acc)])
                 acc))
             '()
             cli)))]
       [else
        (printf "~{~a\n~}"
          (map shell-escape
            (map scalar
              (transaction 'db
                (execute
                 (ct:join #\space
                   "select name from themes where name like ?1"
                   "union"
                   "select name from colors where name like ?1")
                 (format "~a%" word))))))])))
  (exit 0))

(when (opt 'query)
  (for-each
   (lambda (row)
     (match row
       [#(,details)
        (let-values ([(bg fg) (details->colors details)])
          (let ([details (details->json details)])
            (with-preview bg fg
              (lambda ()
                (json:write (current-output-port) details)))
            (newline)))]))
   (transaction 'db
     (execute "select details from sessions where id=? order by timestamp" session-id)))
  (exit 0))

(cond
 [(opt 'lscolors) =>
  (lambda (query)
    (for-each
     (lambda (row)
       (match row
         [#(,name ,details)
          (let ([color (->color details)])
            (<color> open color [r g b])
            (with-preview color #f
              (lambda () (printf "     ")))
            (with-preview #f color
              (lambda () (printf " ~3@a,~3@a,~3@a : ~a" r g b name)))
            (newline))]))
     (match query
       ["default"
        (append
         (transaction 'db
           (execute "select name,details from colors where name in ('white', 'black', 'red', 'green', 'blue')"))
         (transaction 'db
           (execute "select name,details from colors where name like 'ib-%'"))
         (transaction 'db
           (execute "select name,details from colors order by random() limit 10")))]
       [,_
        (let ([query (format "%~a%" query)])
          (transaction 'db
            (execute "select name,details from colors where name like ?" query)))]))
    (exit 0))])

(when (opt 'reset)
  (restore-theme '()))

(cond
 [(opt 'restore) => restore-theme])

(cond
 [(opt 'local) =>
  (lambda (dir)
    (let ([fn (path-combine dir ".themerc")])
      (cond
       [(file-exists? fn)
        (set-file-theme fn)
        (exit 0)]
       [(opt 'now)
        (set-time-theme (erlang:now))
        (exit 0)]
       [else
        (set-keyed-theme (get-real-path dir))
        (exit 0)])))])

(cond
 [(opt 'now) (set-time-theme (erlang:now))])

(cond
 [(opt 'key) => set-keyed-theme])

(match (opt 'rest)
  [#f (exit 0)]
  [(,ref) (set-theme ref)]
  [(,ref1 ,ref2) (set-theme ref1 ref2)]
  [,args
   (errorf #f "Unknown theme arguments ~s" args)])
