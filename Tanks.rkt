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
(struct bullet (pos prop col?) #:mutable)

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

;; World:
;; t1, t2 is a tank struct
;; terr is a Terrain
;; lob is a Listof Bullets
;; State is one of
;; - 't1
;; - 't2
;; - 'anim
(struct world (t1 t2 terr lob state) #:mutable)
(define init (world tank1 tank2 terrain '() 't1))

(define (draw-terrain t)
  (for/fold ([scene (empty-scene 1000 600)])
            ([val t]
             [x WIDTH])
    (add-line scene x HEIGHT x val "fuchsia")))

(define (render w)
  (let* ([t1 (world-t1 w)]
         [t2 (world-t2 w)]
         [tn (world-terr w)])
    (place-images (list (draw-tank t1 tn)
                        (draw-tank t2 tn))
                  (list (make-posn (posn-x (tank-pos t1))
                                   (- (vector-ref tn (posn-x (tank-pos t1))) 8))
                        (make-posn (posn-x (tank-pos t2))
                                   (- (vector-ref tn (posn-x (tank-pos t2))) 8)))
                  (draw-terrain tn))
    ))

(define (handle-key w k)
  (let ([cur-tank (if (symbol=? 't1 (world-state w))
                      (world-t1 w)
                      (world-t2 w))])
      (cond [(key=? " " k)
             (set-world-terr! w (generate-terrain WIDTH HEIGHT 400 .55))]
            [(key=? "d" k) (set-posn-x! (tank-pos cur-tank)
                                        (+ (posn-x (tank-pos cur-tank)) 1))]
            [(key=? "a" k) (set-posn-x! (tank-pos cur-tank)
                                        (- (posn-x (tank-pos cur-tank)) 1))]
            [(key=? "e" k) (set-tank-ang! cur-tank
                                          (+ (tank-ang cur-tank)
                                             (degrees->radians 1)))]
            [(key=? "q" k) (set-tank-ang! cur-tank
                                          (- (tank-ang cur-tank)
                                             (degrees->radians 1)))]))
  w)

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
    (rotate (* -1 rot)
            (add-line (tank-image col) 14 0
                      (- 14 (* 12 (cos ang)))
                      (- 0 (* 12 (sin ang)))
                      (pen "black" 3 "solid" "butt" "bevel")))
    ))

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

(define (play init)
  (big-bang init
            ;(on-tick ...)
            (to-draw render)
            ;(on-mouse ...)
            (on-key handle-key)
            ))
