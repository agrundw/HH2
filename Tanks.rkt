#lang racket

(require math/matrix)
(require lang/posn)
(require math)
(require 2htdp/universe)
(require 2htdp/image)

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
(struct tank (pos ang col) #:mutable)

(define tank1 (tank (make-posn 100 400) (/ pi 2) "blue"))
(define tank2 (tank (make-posn 900 400) (/ pi 2) "red"))

;; Bullets:
;; pos is a posn describing the location of the tank
;; prop is a function that describes how this bullet acts
;; both movement, and special affects
;; col? will determine if the bullet has hit anything
(struct bullet (pos prop col? on-col) #:mutable)

;; World:
;; t1, t2 is a tank struct
;; terr is a Terrain
;; lob is a Listof Bullets
;; state is one of:
;; 'turn1
;; 'turn2
;; 'animating
(struct world (t1 t2 terr lob state) #:mutable)

;; default bullet propagater
(define (prop-bullet pos pow ang)
  (let* ([k .4]
         [vel (make-posn (* k (cos ang) pow) (* k (sin ang) pow))])
    (λ (bul) 
      (set-posn-x! pos (+ (posn-x pos) (posn-x vel)))
      (set-posn-y! pos (+ (posn-y pos) (posn-y vel)))
      (set-bullet-pos! bul pos)
      (set-posn-y! vel (- (posn-y vel) .6))
      (list bul))))

;; default collision checker
(define (bullet-collide? b world)
  (or (< (posn-x (bullet-pos b)) 0)
      (> (posn-x (bullet-pos b)) WIDTH)
      (let* ([terr (world-terr world)]
             [height (vector-ref terr
                                 (inexact->exact (round (posn-x (bullet-pos b)))))])
        (<= (posn-y (bullet-pos b)) height))))

;; default collider
(define (collide-bullet b world)
  world)

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
    (vector-map (λ (x) (+ x (- (/ HEIGHT 2) min-val))) ground)
    ))

;; Terrain is a m x n matrix
(define terrain (generate-terrain WIDTH HEIGHT 400 .55))
;; starting world
(define init (world tank1 tank2 terrain '() 't1))

;; draws the terrain
(define (draw-terrain t)
  (for/fold ([scene (empty-scene 1000 600)])
            ([val t]
             [x WIDTH])
    (add-line scene x HEIGHT x val "fuchsia")))

;; render function for big-bang
(define (render w)
  (let* ([t1 (world-t1 w)]
         [t2 (world-t2 w)]
         [tn (world-terr w)]
         [twt (place-images (list (draw-tank t1 tn)
                                  (draw-tank t2 tn))
                            (list (make-posn (posn-x (tank-pos t1))
                                             (- (vector-ref tn (posn-x (tank-pos t1))) 8))
                                  (make-posn (posn-x (tank-pos t2))
                                             (- (vector-ref tn (posn-x (tank-pos t2))) 8)))
                            (draw-terrain tn))])
    
    (for/fold ([scene twt])
              ([val (world-lob w)])
      (place-image (circle 5 "solid" "red")
                   (posn-x (bullet-pos val))
                   (posn-y (bullet-pos val))
                   scene))
    ))

(define (handle-key w k)
  (let ([cur-tank (if (symbol=? 't1 (world-state w))
                      (world-t1 w)
                      (world-t2 w))]
        [tn (world-terr w)])
    
    (cond [(key=? "shift" k)
           (set-world-terr! w (generate-terrain WIDTH HEIGHT 400 .55))]
          [(or (symbol=? (world-state w) 'anim1) (symbol=? (world-state w) 'anim2)) w]
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
          [(key=? " " k)
           (set-world-lob! w
                           (list (bullet (make-posn (posn-x (tank-pos cur-tank))
                                                    (+ 10 (vector-ref tn (posn-x (tank-pos cur-tank)))))
                                         (prop-bullet (make-posn (posn-x (tank-pos cur-tank))
                                                                 (+ 10 (vector-ref tn (posn-x (tank-pos cur-tank)))))
                                                      50
                                                      (tank-ang cur-tank))
                                         bullet-collide?
                                         collide-bullet)))
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
         [rot (radians->degrees (atan (- y2 y1) (- x2 x1)))])
    
    (add-line (rotate (* -1 rot)(tank-image col)) 14 0
              (- 14 (* 12 (cos ang)))
              (- 0 (* 12 (sin ang)))
              (pen "black" 3 "solid" "butt" "bevel")))
  )

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
             (if ((bullet-col? bul) bul w)
                 ((bullet-on-col bul) bul w)
                 (set! lob (append ((bullet-prop bul) bul) lob))))
           (set-world-lob! w lob)
           (when (empty? lob)
             (set-world-state! w (if (symbol=? (world-state w) 'anim1)
                                     't2
                                     't1))))
         w]
        [else w]))

(define (play init)
  (big-bang init
            (on-tick tick)
            (to-draw render)
            ;(on-mouse ...)
            (on-key handle-key)
            ))
