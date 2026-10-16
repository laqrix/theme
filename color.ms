(import (color) (swish mat) (swish testing))

(define (rgb-of c)
  (match c
    [`(<color> ,r ,g ,b) (list r g b)]))

(define-syntax values->list
  (syntax-rules ()
    [(_ e) (call-with-values (lambda () e) list)]))

;; Runs (f) with a 'db process backed by a fresh database that contains
;; the named colors in colors, an alist of (name . details-json-string).
(define (with-color-db colors f)
  (let ([dir (path-combine (or (getenv "TMPDIR") "/tmp")
               (format "theme-mat-~a" (uuid->string (osi_make_uuid))))])
    (on-exit (delete-tree dir)
      (db:start 'db (make-directory-path (path-combine dir "theme.db3")) 'create)
      (on-exit (db:stop 'db)
        (transaction 'db
          (execute "create table colors (name text unique collate nocase, details blob)")
          (for-each
           (lambda (entry)
             (execute "insert into colors (name,details) values(?,?)"
               (car entry)
               (string->utf8 (cdr entry))))
           colors))
        (f)))))

(isolate-mat rgb-constructor ()
  (assert (equal? (rgb 1 2 3) (->color "1,2,3")))
  (assert (equal? (rgb-of (rgb 1 2 3)) '(1 2 3))))

(isolate-mat hsv-constructor ()
  (assert (equal? (rgb-of (hsv 0 1 1)) '(255 0 0)))
  (assert (equal? (rgb-of (hsv 60 1 1)) '(255 255 0)))
  (assert (equal? (rgb-of (hsv 120 1 1)) '(0 255 0)))
  (assert (equal? (rgb-of (hsv 180 1 1)) '(0 255 255)))
  (assert (equal? (rgb-of (hsv 240 1 1)) '(0 0 255)))
  (assert (equal? (rgb-of (hsv 300 1 1)) '(255 0 255)))
  (assert (equal? (rgb-of (hsv 0 0 0)) '(0 0 0)))
  (assert (equal? (rgb-of (hsv 0 0 1)) '(255 255 255)))
  (assert (equal? (rgb-of (hsv 0 0 1/2)) '(127 127 127))))

(isolate-mat hsl-constructor ()
  (assert (equal? (rgb-of (hsl 0 1 1/2)) '(255 0 0)))
  (assert (equal? (rgb-of (hsl 120 1 1/2)) '(0 255 0)))
  (assert (equal? (rgb-of (hsl 240 1 1/2)) '(0 0 255)))
  (assert (equal? (rgb-of (hsl 0 0 0)) '(0 0 0)))
  (assert (equal? (rgb-of (hsl 0 0 1)) '(255 255 255)))
  (assert (equal? (rgb-of (hsl 0 1 1/4)) '(127 0 0))))

(isolate-mat hue-wraps ()
  (assert (equal? (hsv 360 1 1) (hsv 0 1 1)))
  (assert (equal? (hsv 480 1 1) (hsv 120 1 1)))
  (assert (equal? (hsv -120 1 1) (hsv 240 1 1)))
  (assert (equal? (hsl 480 1 1/2) (hsl 120 1 1/2))))

(isolate-mat byte-clamping ()
  (assert (equal? (rgb-of (hsv 0 0 2)) '(255 255 255)))
  (assert (equal? (rgb-of (hsv 0 0 -1)) '(0 0 0)))
  (assert (equal? (rgb-of (hsv 0 1 0.3)) '(76 0 0))))

(isolate-mat hsv->rgb-values ()
  (assert (equal? (values->list (hsv->rgb 0 1 1)) '(1 0 0)))
  (assert (equal? (values->list (hsv->rgb 210 1/2 1)) '(1/2 3/4 1))))

(isolate-mat hsl->rgb-values ()
  (assert (equal? (values->list (hsl->rgb 0 1 1/2)) '(1 0 0)))
  (assert (equal? (values->list (hsl->rgb 0 0 1/2)) '(1/2 1/2 1/2))))

(isolate-mat rgb->hsv-values ()
  (assert (equal? (values->list (rgb->hsv 255 0 0)) '(0 1 255)))
  (assert (equal? (values->list (rgb->hsv 0 255 0)) '(120 1 255)))
  (assert (equal? (values->list (rgb->hsv 0 0 255)) '(240 1 255)))
  (assert (equal? (values->list (rgb->hsv 255 0 255)) '(300 1 255)))
  (assert (equal? (values->list (rgb->hsv 100 100 100)) '(0 0 100)))
  (assert (equal? (values->list (rgb->hsv 0 0 0)) '(0 0 0))))

(isolate-mat rgb->hsl-values ()
  (assert (equal? (values->list (rgb->hsl 1 0 0)) '(0 1 1/2)))
  (assert (equal? (values->list (rgb->hsl 0 1/2 1)) '(210 1 1/2)))
  (assert (equal? (values->list (rgb->hsl 1/2 1/2 1/2)) '(0 0 1/2)))
  (assert (equal? (values->list (rgb->hsl 1 1 1)) '(0 0 1))))

(isolate-mat hsv-round-trip ()
  (for-each
   (lambda (bytes)
     (let-values ([(h s v) (apply rgb->hsv bytes)])
       (assert (equal? (rgb-of (hsv h s (/ v 255))) bytes))))
   '((255 0 0) (0 0 0) (255 255 255) (0 128 255) (255 128 0) (51 102 153))))

(isolate-mat color->hex ()
  (assert (equal? (color->hex (rgb 0 0 0)) "000000"))
  (assert (equal? (color->hex (rgb 1 2 3)) "010203"))
  (assert (equal? (color->hex (rgb 255 171 205)) "FFABCD")))

(isolate-mat color->csv ()
  (assert (equal? (color->csv (rgb 0 0 0)) "0,0,0"))
  (assert (equal? (color->csv (rgb 1 22 255)) "1,22,255")))

(isolate-mat hex ()
  (assert (equal? (->color "ff8000") (rgb 255 128 0)))
  (assert (equal? (->color "FF8000") (rgb 255 128 0)))
  (assert (equal? (->color "000000") (rgb 0 0 0)))
  (assert (equal? (->color (color->hex (rgb 18 52 86))) (rgb 18 52 86))))

(isolate-mat hex-invalid ()
  (assert (not (->color "fffff")))
  (assert (not (->color "fffffff")))
  (assert (not (->color "-fffff")))
  (assert (not (->color "+fffff")))
  (assert (not (->color "12/abc")))
  (assert (not (->color "ggg000")))
  (assert (not (->color "#ff800"))))

(isolate-mat csv ()
  (assert (equal? (->color "255,128,0") (rgb 255 128 0)))
  (assert (equal? (->color "0,0,0") (rgb 0 0 0)))
  (assert (equal? (->color "007,08,9") (rgb 7 8 9))))

(isolate-mat csv-invalid ()
  (assert (not (->color "1,2")))
  (assert (not (->color "1,2,3,4")))
  (assert (not (->color "1,,3")))
  (assert (not (->color "a,b,c")))
  (assert (not (->color "256,0,0")))
  (assert (not (->color "300,0,0")))
  (assert (not (->color "-1,0,0")))
  (assert (not (->color "1.5,0,0")))
  (assert (not (->color "0001,0,0")))
  (assert (not (->color " 1,2,3"))))

(isolate-mat json ()
  (assert (equal? (->color "{\"r\":1,\"g\":2,\"b\":3}") (rgb 1 2 3)))
  (assert (equal? (->color "{\"h\":120,\"s\":1,\"v\":1}") (rgb 0 255 0)))
  ;; r,g,b takes precedence over h,s,v
  (assert (equal? (->color "{\"r\":1,\"g\":2,\"b\":3,\"h\":120,\"s\":1,\"v\":1}")
            (rgb 1 2 3)))
  (assert (equal? (->color (json:make-object [r 4] [g 5] [b 6])) (rgb 4 5 6)))
  (assert (not (->color "{\"r\":1,\"g\":2}")))
  (assert (not (->color "{}"))))

(isolate-mat bytevector ()
  (assert (equal? (->color (string->utf8 "010203")) (rgb 1 2 3)))
  (assert (equal? (->color (string->utf8 "{\"r\":1,\"g\":2,\"b\":3}")) (rgb 1 2 3))))

(isolate-mat unrecognized ()
  (assert (not (->color "")))
  (assert (not (->color "nope")))
  (assert (not (->color "{\"name\":\"red\"}")))
  (assert (not (->color 5)))
  (assert (not (->color 'red)))
  (assert (not (->color #f))))

(isolate-mat named ()
  (with-color-db
   '(("red" . "{\"r\":255,\"g\":0,\"b\":0}")
     ("Dark Blue" . "{\"r\":0,\"g\":0,\"b\":139}"))
   (lambda ()
     (assert (equal? (->color "red") (rgb 255 0 0)))
     (assert (equal? (->color "RED") (rgb 255 0 0)))
     (assert (equal? (->color "dark blue") (rgb 0 0 139)))
     (assert (equal? (->color "{\"name\":\"red\"}") (rgb 255 0 0)))
     (assert (equal? (->color (string->utf8 "red")) (rgb 255 0 0)))
     (assert (not (->color "green"))))))
