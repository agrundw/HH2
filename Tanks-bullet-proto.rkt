#lang racket

(require lang/posn)
(require math/matrix)
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
;; ang is the angle of the turret 
(struct tank (pos ang) #:mutable)

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
  (and (not (or (< (posn-x (bullet-pos b)) 0)
                (> (posn-x (bullet-pos b)) WIDTH)))
       (let* ([terr (world-terr world)]
              [height (vector-ref terr
                                  (inexact->exact (round (posn-x (bullet-pos b)))))])
         (<= (posn-y (bullet-pos b)) height))))

;; default collider
(define (collide-bullet b world)
  world)

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

;; Terrain is a m x n matrix
(define terrain (generate-terrain WIDTH HEIGHT 400 .55))

(define (render w)
  (let ([terrain (for/fold ([scene (empty-scene 1000 600)])
                           ([val (world-terr w)]
                            [x WIDTH])
                   (place-image/align (rectangle 1 val "solid" "green")
                                      x HEIGHT
                                      "right" "bottom"
                                      scene))])
    (for/fold ([scene terrain])
              ([val (world-lob w)])
      (place-image (circle 5 "solid" "red")
                   (posn-x (bullet-pos val))
                   (- HEIGHT (posn-y (bullet-pos val)))
                   scene))))

(define (handle-key w k)
  (cond [(and (key=? " " k) (symbol=? (world-state w) 't1))
         (set-world-lob! w
                         (list (bullet (make-posn 0 (+ 10 (vector-ref terrain 0)))
                                       (prop-bullet (make-posn 0 (+ 10 (vector-ref terrain 0))) 50 (/ pi 4))
                                       bullet-collide?
                                       collide-bullet)))
         (set-world-state! w 'anim1)
         w]
        [(and (key=? " " k) (symbol=? (world-state w) 't2))
         (set-world-lob! w
                         (list (bullet (make-posn 0 (+ 10 (vector-ref terrain 0)))
                                       (prop-bullet (make-posn 0 (+ 10 (vector-ref terrain 0))) 50 (/ pi 4))
                                       bullet-collide?
                                       collide-bullet)))
         (set-world-state! w 'anim2)
         w]
        [else w]))

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
  (big-bang (world null null terrain null 't1)
            (on-tick tick 0.001)
            (to-draw render)
            (on-key handle-key)))
(play 1)