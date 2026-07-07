#!/usr/bin/env swish

(import (read))

(define (read-rgb.txt)
  (define fn (path-combine "palettes" "rgb.txt"))
  (cond
   [(not (file-exists? fn)) '()]
   [else
    (let ([ip (open-file-to-read fn)])
      (on-exit (close-port ip)
        (let lp ()
          (let ([line (get-line ip)])
            (cond
             [(eof-object? line) '()]
             [else
              (match (pregexp-match (re "(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(.+)") line)
                [(,_ ,r ,g ,b ,name)
                 (cons `(color ,name ,(join (list r g b) #\,)) (lp))]
                [,_ (lp)])])))))]))

(define (read-doom-themes)
  (define dn (path-combine "palettes" "themes"))
  (cond
   [(not (directory? dn)) '()]
   [else
    (fold-files dn '() (lambda (x) #t)
      (lambda (fn acc)
        (let ([annotated-code (read-code (utf8->string (read-file fn)))]
              [bg #f]
              [fg #f])
          (define (fixup-name x)
            (pregexp-replace* (re "-theme$")
              (pregexp-replace* (re "^doom-") x "")
              ""))
          (define (fixup-color x)
            (pregexp-replace* (re "^#") x ""))
          (walk-annotations annotated-code
            (lambda (x)
              (match x
                [`(annotation [stripped (bg (quote (,val . ,_)))])
                 (set! bg (fixup-color val))]
                [`(annotation [stripped (fg (quote (,val . ,_)))])
                 (set! fg (fixup-color val))]
                [,_ (void)])))
          (cond
           [bg                         ; foreground colors matter less
            (cons `(theme ,(fixup-name (path-root (path-last fn))) ,bg ,fg) acc)]
           [else acc]))))]))

(define (read-sexp filename)
  (define fn (path-combine "palettes" filename))
  (cond
   [(not (file-exists? fn)) '()]
   [else
    (let ([ip (open-file-to-read fn)])
      (on-exit (close-port ip)
        (let lp ()
          (let ([x (read ip)])
            (cond
             [(eof-object? x) '()]
             [else
              (match x
                [(,name ,str)
                 (cons `(color ,name ,str) (lp))]
                [(,name ,bg ,fg)
                 (cons `(theme ,name ,bg ,fg) (lp))])])))))]))

(for-each
 (lambda (x)
   (printf "~s\n" x))
 (append
  (read-rgb.txt)
  (read-sexp "sherwin-williams.ss")
  (read-sexp "crayola.ss")
  (read-sexp "css.ss")
  (read-sexp "user-colors.ss")))

(for-each
 (lambda (x)
   (printf "~s\n" x))
 (append
  (read-doom-themes)
  (read-sexp "user-themes.ss")))
