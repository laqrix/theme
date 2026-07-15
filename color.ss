#!chezscheme
(library (color)
  (export
   ->color
   <color>
   color->csv
   color->hex
   hsl
   hsl->rgb
   hsv
   hsv->rgb
   rgb
   rgb->hsl
   rgb->hsv
   )
  (import
   (chezscheme)
   (swish imports)
   )
  (define-tuple <color> r g b)

  (define (hcx->rgb hp c x m)
    (define (rgb r1 g1 b1 m)
      (values (+ r1 m) (+ g1 m) (+ b1 m)))
    (cond
     [(<= 0 hp 1) (rgb c x 0 m)]
     [(<= 1 hp 2) (rgb x c 0 m)]
     [(<= 2 hp 3) (rgb 0 c x m)]
     [(<= 3 hp 4) (rgb 0 x c m)]
     [(<= 4 hp 5) (rgb x 0 c m)]
     [(<= 5 hp 6) (rgb c 0 x m)]))

  (define (hsl->rgb h s l)
    ;; h = [0, 360], s,l = [0, 1]
    (define (rgb r1 g1 b1 m)
      (values (+ r1 m) (+ g1 m) (+ b1 m)))
    (let* ([hp (/ (mod h 360) 60)]
           [c (* (- 1 (abs (- (* 2 l) 1))) s)]
           [x (* c (- 1 (abs (- (mod hp 2) 1))))]
           [m (- l (/ c 2))])
      (hcx->rgb hp c x m)))

  (define (hsv->rgb h s v)
    ;; h = [0, 360], s,v = [0, 1]
    (define (rgb r1 g1 b1 m)
      (values (+ r1 m) (+ g1 m) (+ b1 m)))
    (let* ([hp (/ (mod h 360) 60)]
           [c (* v s)]
           [x (* c (- 1 (abs (- (mod hp 2) 1))))]
           [m (- v c)])
      (hcx->rgb hp c x m)))

  (define (hue&chroma r g b)
    ;; r,g,b = [0, 1]
    (let* ([m (min r g b)]
           [M (max r g b)]
           [c (- M m)]
           [hp (if (= c 0)
                   0
                   (cond
                    [(= M r) (mod (/ (- g b) c) 6)]
                    [(= M g) (+ (/ (- b r) c) 2)]
                    [(= M b) (+ (/ (- r g) c) 4)]
                    [else (errorf 'hue&chroma "undefined hue")]))]
           [h (* hp 60)])
      (values h c m M)))

  (define (rgb->hsl r g b)
    (let-values ([(h c m M) (hue&chroma r g b)])
      (let* ([l (/ (+ M m) 2)]
             [s (if (= c 0)
                    0
                    (/ c (- 1 (abs (- (* 2 l) 1)))))])
        (values h s l))))

  (define (rgb->hsv r g b)
    (let-values ([(h c m M) (hue&chroma r g b)])
      (let* ([v M]
             [s (if (= c 0)
                    0
                    (/ c v))])
        (values h s v))))

  (define (as-byte x)
    (exact (min (truncate (* (max 0 x) 255)) 255)))

  (define (rgb r g b)
    (<color> make [r r] [g g] [b b]))

  (define (hsv h s v)
    (let-values ([(r g b) (hsv->rgb h s v)])
      (rgb (as-byte r) (as-byte g) (as-byte b))))

  (define (hsl h s l)
    (let-values ([(r g b) (hsl->rgb h s l)])
      (rgb (as-byte r) (as-byte g) (as-byte b))))

  (define (json->color x)
    (cond
     [(let ([r (json:ref x 'r #f)]
            [g (json:ref x 'g #f)]
            [b (json:ref x 'b #f)])
        (and r g b (rgb r g b)))]
     [(let ([h (json:ref x 'h #f)]
            [s (json:ref x 's #f)]
            [v (json:ref x 'v #f)])
        (and h s v (hsv h s v)))]
     [(let ([n (json:ref x 'name #f)])
        (and n (string->color n)))]
     [else #f]))

  (define (color->csv c)
    (<color> open c [r g b])
    (format "~a,~a,~a" r g b))

  (define (csv->color x)
    (and (do ([i 0 (+ i 1)]
              [comma? #f (or comma? (char=? (string-ref x i) #\,))])
             ((= i (string-length x)) comma?))
         (match (split x #\,)
           [(,r ,g ,b)
            (let ([r (string->number r)]
                  [g (string->number g)]
                  [b (string->number b)])
              (and r g b (rgb r g b)))]
           [,_ #f])))

  (define (color->hex c)
    (<color> open c [r g b])
    (format "~2,'0x~2,'0x~2,'0x" r g b))

  (define (hex->color str)
    (cond
     [(and (= (string-length str) 6)
           (string->number str 16)) =>
      (lambda (c24)
        (rgb
         (#3%logand #xFF (#3%fxarithmetic-shift-right c24 16))
         (#3%logand #xFF (#3%fxarithmetic-shift-right c24 8))
         (#3%logand #xFF c24)))]
     [else #f]))

  (define (scalar x)
    (match x
      [() #f]
      [#(,v) v]
      [(#(,v)) v]))

  (define (string->color str)
    (cond
     [(and (char=? (string-ref str 0) #\{)
           (json->color (json:string->object str)))]
     [(csv->color str)]
     [(hex->color str)]
     [(let* ([db-pid (whereis 'db)]
             [x (and db-pid
                     (scalar
                      (transaction db-pid
                        (execute "select details from colors where name=? limit 1" str))))])
        (and x (->color x)))]
     [else #f]))

  (define (->color x)
    (cond
     [(string? x) (string->color x)]
     [(bytevector? x) (string->color (utf8->string x))]
     [(json:object? x) (json->color x)]
     [else #f]))
  )
