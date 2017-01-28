#lang racket

(require 2htdp/image
         2htdp/universe
         lang/posn
         math)

(define WIDTH 1000)
(define HEIGHT 600)

(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

;; Tank:
;; pos is a posn describing the location of the tank
;; ang is the angle of the turret in radians
;; col is the color of the tank
;; los is a list of symbols representing different bullet types
(struct tank (pos ang col rad pow los score) #:mutable)

(define tank1 (tank (make-posn 100 400) (/ pi 2) "blue" 0 50 '(single trip quint) 0))
(define tank2 (tank (make-posn 900 400) (/ pi 2) "red" 0 50 '(single trip quint) 0))
(define terrain-img (empty-scene 0 0))

(define collider (interface () tick collision? on-collision get-pos))

(define default-bullet
  (class* object% (collider)
    (super-new)
    (init-field pos)
    (init-field pow)
    (init-field ang)

    (define k .3)
    (define gravity .5)

    (define vel (make-posn (* k (cos ang) pow -1) (* k (sin ang) pow -1)))

    (define/public (get-pos)
      pos)

    (define/public (tick)
      (set-posn-x! pos (+ (posn-x pos) (posn-x vel)))
      (set-posn-y! pos (+ (posn-y pos) (posn-y vel)))
      (set-posn-y! vel (+ (posn-y vel) gravity))
      (list this))

    ;; checks if bullet collides with a tank
    (define (bullet-col-tank? t)
      (and (> (posn-x pos) (- (posn-x (tank-pos t)) 17))
           (< (posn-x pos) (+ (posn-x (tank-pos t)) 17))
           (>= (posn-y pos) (- (posn-y (tank-pos t)) 10))))

    (define/public (collision? world)
      (or (< (posn-x pos) 0)
          (> (posn-x pos) WIDTH)
          (let* ([terr (world-terr world)]
                 [x (exact-round (posn-x pos))]
                 [height (vector-ref terr x)])
            (>= (posn-y pos) height))
          (bullet-col-tank? (world-t1 world))
          (bullet-col-tank? (world-t2 world))))

    (define/public (on-collision w)
      (match-define (world t1 t2 terr _ state) w)
      (let* ([x (exact-round (posn-x pos))]
             [y (posn-y pos)]
             [rad 20])
        (for ([i (in-range (* -1 rad) rad)]
              #:when (and (< (+ x i) WIDTH)
                          (> (+ x i) 0)
                          (< (vector-ref terr (+ x i)) y)))
          (vector-set! terr (+ x i) (- (+ (vector-ref terr (+ x i)) rad) (abs i))))
        (set! terrain-img (draw-terrain terr))
        (cond [(and (bullet-col-tank? t1) (symbol=? 'anim1 state))
               (set-tank-score! t1 (- (tank-score t1) 10))]
              [(bullet-col-tank? t1) (set-tank-score! t2 (+ (tank-score t2) 10))]
              [(and (bullet-col-tank? t2) (symbol=? 'anim2 state))
               (set-tank-score! t2 (- (tank-score t2) 10))]
              [(bullet-col-tank? t2) (set-tank-score! t1 (+ (tank-score t1) 10))])
        world))))

;; World:
;; t1, t2 is a tank struct
;; terr is a Terrain
;; lob is a Listof Bullets
;; state is one of:
;; 'turn1
;; 'turn2
;; 'animating
(struct world (t1 t2 terr lob state) #:mutable)

;; generates terrain
(define (generate-terrain w h d r)
  (let* ([power (inexact->exact (expt 2 (ceiling (/ (log w) (log 2)))))]
         [ground (make-vector (+ power 1))]
         [init (- (+ (/ HEIGHT 2) (* (random) d 2)) d)]
         [displace (* d r)]
         [i 1])
    (vector-set! ground 0 init)
    (vector-set! ground power init)
    (while (< i power)
           (for ([j (in-range (/ (/ power i) 2) power (/ power i))])
             (vector-set! ground j (/ (+ (vector-ref ground
                                                     (- j (/ (/ power i) 2)))
                                         (vector-ref ground
                                                     (+ j (/ (/ power i) 2))))
                                      2))
             (vector-set! ground j (+ (vector-ref ground j)
                                      (- (* (random) displace 2) displace))))
           (set! displace (* displace r))
           (set! i (* i 2)))
    (define min-val (foldr (λ (x y) (min x y)) HEIGHT (vector->list ground)))
    (vector-map (λ (x) (+ x (- (/ HEIGHT 2) min-val))) ground)))

;; Terrain is an vector of hieghts
(define terrain (generate-terrain WIDTH HEIGHT 400 .5))
;; starting world
(define init (world tank1 tank2 terrain '() 't1))

;; draws the terrain
(define (draw-terrain t)
  (for/fold ([scene (empty-scene 1000 600 "light blue")])
            ([val t]
             [x WIDTH])
    (add-line scene x HEIGHT x val "dark green")))

(set! terrain-img (draw-terrain terrain))

;; render function for big-bang
(define (render w)
  (let* ([t1 (world-t1 w)]
         [t2 (world-t2 w)]
         [cur-tank
          (if (or (symbol=? 't1 (world-state w)) (symbol=? 'anim1 (world-state w)))
              (world-t1 w)
              (world-t2 w))]
         [tn (world-terr w)]
         [twt (place-images
               (list (draw-tank t1 tn)
                     (draw-tank t2 tn))
               (list (make-posn (posn-x (tank-pos t1))
                                (- (vector-ref tn
                                               (posn-x (tank-pos t1)))
                                   8))
                     (make-posn (posn-x (tank-pos t2))
                                (- (vector-ref tn
                                               (posn-x (tank-pos t2)))
                                   8)))
               terrain-img)])
    (define BG (place-images 
                (list (text (~a (tank-score t1)) 18 (tank-col t1))
                      (text (~a (tank-score t2)) 18 (tank-col t2))
                      (text (~a "Angle: " (round (radians->degrees (tank-rad cur-tank))))
                            18
                            (tank-col cur-tank))
                      (text (~a "Power: " (tank-pow cur-tank)) 18 (tank-col cur-tank)))
                (list (make-posn 50 50)
                      (make-posn 950 50)
                      (make-posn 500 38)
                      (make-posn 500 63))
                twt))
    (for/fold ([scene BG])
              ([val (world-lob w)])
      (place-image (circle 3 "solid" "dark grey")
                   (posn-x (send val get-pos))
                   (posn-y (send val get-pos))
                   scene))))

;; generates a bullet using the position power angle and 3 functions
;; function are defaulted to generic functions
(define (gen-bul pos pow rad)
  (new default-bullet [pos pos] [pow pow] [ang rad]))

;; generates the default bullet using a tank the terrain and an angle
(define (gen-sing tnk terr rad)
  (gen-bul (make-posn (posn-x (tank-pos tnk))
                      (+ -12 (vector-ref terr (posn-x (tank-pos tnk)))))
           (tank-pow tnk)
           rad))

(define (generate-bullets symb tnk terr)
  (cond [(symbol=? symb 'single)
         (list (gen-sing tnk terr (tank-rad tnk)))]
        [(symbol=? symb 'trip)
         (list (gen-sing tnk terr (tank-rad tnk))
               (gen-sing tnk terr (- (tank-rad tnk) (/ pi 32)))
               (gen-sing tnk terr (+ (tank-rad tnk) (/ pi 32))))]
        [(symbol=? symb 'quint)
         (list (gen-sing tnk terr (tank-rad tnk))
               (gen-sing tnk terr (- (tank-rad tnk) (/ pi 32)))
               (gen-sing tnk terr (- (tank-rad tnk) (/ pi 64)))
               (gen-sing tnk terr (+ (tank-rad tnk) (/ pi 32)))
               (gen-sing tnk terr (+ (tank-rad tnk) (/ pi 64))))]))

(define (handle-key w k)
  (let* ([cur-tank (if (symbol=? 't1 (world-state w))
                       (world-t1 w)
                       (world-t2 w))]
         [tn (world-terr w)]
         [symb (list-ref (tank-los cur-tank)
                         (random (length (tank-los cur-tank))))])
    
    (cond [(key=? "shift" k)
           (set-world-terr! w (generate-terrain WIDTH HEIGHT 400 .5))
           (set! terrain-img (draw-terrain (world-terr w)))]
          [(or (symbol=? (world-state w) 'anim1)
               (symbol=? (world-state w) 'anim2)) w]
          [(key=? "d" k) (set-posn-x! (tank-pos cur-tank)
                                      (+ (posn-x (tank-pos cur-tank)) 1))]
          [(key=? "a" k) (set-posn-x! (tank-pos cur-tank)
                                      (- (posn-x (tank-pos cur-tank)) 1))]
          [(key=? "e" k) (set-tank-ang! cur-tank
                                        (+ (tank-ang cur-tank)
                                           (degrees->radians 1)))]
          [(key=? "q" k) (set-tank-ang! cur-tank
                                        (- (tank-ang cur-tank)
                                           (degrees->radians 1)))]
          [(key=? "w" k) (set-tank-pow! cur-tank
                                        (min (+ (tank-pow cur-tank) 1) 100))]
          [(key=? "s" k) (set-tank-pow! cur-tank
                                        (max (- (tank-pow cur-tank) 1) 0))]
          [(key=? " " k)
           (set-world-lob! w
                           (generate-bullets symb cur-tank tn))
           (if (symbol=? 't1 (world-state w))
               (set-world-state! w 'anim1)
               (set-world-state! w 'anim2))]))
  w)


;; draws a tank
(define (draw-tank tk tn)
  (let* ([x (posn-x (tank-pos tk))]
         [y (vector-ref tn x)]
         [col (tank-col tk)]
         [ang (tank-ang tk)]
         [x1 (- x 15)]
         [y1 (vector-ref tn x1)]
         [x2 (+ x 15)]
         [y2 (vector-ref tn x2)]
         [rad (atan (- y2 y1) (- x2 x1))]
         [deg (radians->degrees rad)])
    
    (set-tank-rad! tk (+ rad ang))
    (set-posn-y! (tank-pos tk) y)
    (rotate (* -1 deg)(add-line (tank-image col) 14 0
                                (- 14 (* 12 (cos ang)))
                                (- 0 (* 12 (sin ang)))
                                (pen "black" 3 "solid" "butt" "bevel")))))

;; tank image
(define (tank-image c)
  (polygon (list (make-posn 0 0)
                 (make-posn 5 0)
                 (make-posn 10 -5)
                 (make-posn 20 -5)
                 (make-posn 25 0)
                 (make-posn 30 0)
                 (make-posn 25 5)
                 (make-posn 5 5))
           "solid"
           c))

(define (tick w)
  (cond [(or (symbol=? (world-state w) 'anim1)
             (symbol=? (world-state w) 'anim2))
         (let* ([lob '()])
           (for ([bul (world-lob w)])
             (if (send bul collision? w)
                 (send bul on-collision w)
                 (set! lob (append (send bul tick) lob))))
           (set-world-lob! w lob)
           (when (empty? lob)
             (set-world-state! w (if (symbol=? (world-state w) 'anim1)
                                     't2
                                     't1))))
         w]
        [else w]))

(define FINAL-SCORE 100)

(define (score w)
  (let ([t1 (world-t1 w)]
        [t2 (world-t2 w)])
    (or (>= (tank-score t1) FINAL-SCORE)
        (>= (tank-score t2) FINAL-SCORE))))

(define (game-over w)
  (let ([t1 (world-t1 w)]
        [t2 (world-t2 w)])
    (if (>= (tank-score t1 ) FINAL-SCORE)
        (place-image (text (~a "Winner: " (tank-col t1)) 24 (tank-col t1)) 500 300 (render w))
        (place-image (text (~a "Winner: " (tank-col t2)) 24 (tank-col t2)) 500 300 (render w)))))

(define (play [init init])
  (big-bang init
            (on-tick tick (/ 1 60))
            (to-draw render)
            (stop-when score game-over)
            (on-key handle-key)))

;; Use this to start a game ==> (play init)
(play)

