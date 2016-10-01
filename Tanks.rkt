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
(struct bullet (pos prop col?) #:mutable)

#;(define (generate-terrain)
    (let ([ground (for/vector ([i WIDTH])
                    (random (/ HEIGHT 4) (* 3 (/ HEIGHT 4))))])
      ground))

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
    ground
    ))

(define (smooth t)
  (for/vector ([i t])
    (hamming i)))

(define (hamming n)
  (* HEIGHT (- .54 (* .46 (cos (/ (* 2 pi n) (- WIDTH 1)))))))

;; Terrain is a m x n matrix
(define terrain (generate-terrain WIDTH HEIGHT 200 .6))

;; World:
;; t1, t2 is a tank struct
;; terr is a Terrain
;; lob is a Listof Bullets
(struct world (t1 t2 terr lob) #:mutable)

(define (render w)
  (for/fold ([scene (empty-scene 1000 600)])
            ([val w]
             [x WIDTH])
    (place-image (square 2 "solid" "green") x val scene)))

(define (handle-key w k)
  (generate-terrain WIDTH HEIGHT 200 .6))

(define (play init)
  (big-bang terrain
            ;(on-tick ...)
            (to-draw render)
            ;(on-mouse ...)
            (on-key handle-key)
            ))
