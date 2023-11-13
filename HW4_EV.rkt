;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW4_EV) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;Emma Vonbuelow--Homework 4
(require 2htdp/image)
(require 2htdp/universe)

;--Problem 1
;A LectureHall is a (make-lecture-hall Number Number 
(define-struct lecture-hall [number capacity])
;--LH-temp: LectureHall-> ??
(define (LH-temp h)
  (...  (...(lecture-hall-number   h))
        (...(lecture-hall-capacity h))))
;make-lecture-hall                          <-Constructor
;lecture-hall-number  lecture-hall-capacity <-Selectors

;A Automobile is a (make-automobile Number (4 digit year) String String)
(define-struct automobile [year make model])
;--Auto-temp: Automobile-> ??
(define (Auto-temp a)
  (...  (...(automobile-year  a))
        (...(automobile-make  a))
        (...(automobile-model a))))
;make-automobile                                  <-Constuctor
;automobile-year automobile-make automobile-model <-Selectors

;A FootballPlayer is a (make-football player String String Number)
(define-struct football-player [name position number])
;--FB-temp: FootballPlayer-> ??
(define (FB-temp f)
  (...  (...(football-player-name     f))
        (...(football-player-position f))
        (...(football-player-number   f))))
;make-football-player <-Constructor
;football-player-name football-player-position football-player-number <-Selectors

;A Shirt is a (make-shirt String String(S, M, L, XL) String)
(define-struct shirt [material size color])
;--Shirt-temp: Shirt-> ??
(define (Shirt-temp a)
  (...  (...(shirt-material a))
        (...(shirt-size     a))
        (...(shirt-color    a))))
;make-shirt <-Constructor
;shirt-material shirt-size shirt-color <-Selectors                                

;--Problem 2
;--A Movie is one of:
; - Regular (product ID), base price, and #yrs stored
;      *Discount annually price(0.965)^years (2$ >=)
; - Classic (product ID) and price (2$ >=)

;--A regularMovie is a (make-regM String Number Number)  <--String, and 2 Numbers defined above
(define-struct regM [prodID bPrice yr])

;--A classicMovie is a (make-claM String Number)
(define-struct claM [prodID price])

;--Movie-> ??
(define (movie-temp m)
  (cond [(regM? m)(...(regM-prodID m)
                   ...(regM-bPrice m)
                   ...(regM-yr m))]
         [else     (...(claM-prodID m)
                    ...(claM-price  m))]))

;--Current price of a Movie
;--Movie (regularMovie or classicMovie)-> Number
(define (cPrice m)
  (if (claM? m)
      (claM-price m)
      (if (> (* (regM-bPrice m) (expt 0.965 (regM-yr m))) 2)
          (* (regM-bPrice m) (expt 0.965 (regM-yr m)))
          2)))

(check-expect (cPrice (make-claM "234-87-1DX" 4.65)) 4.65)
(check-expect (cPrice (make-regM "234-87-1DX" 2 8))    2)
(check-expect (cPrice (make-regM "234-87-1DX" 9.65 4)) (* 9.65 (expt 0.965 4)))

;--Problem 3

;--A 2D-Vec is a (make-posn Number Number)

;--vec-temp: 2D-Vec-> ??
(define (vec-temp t)
  (... (...(posn-x t))
       (...(posn-y t))))
       
;--Add two Vec[tors] together
;--2D-Vec-> 2D-Vec 
(define (vec+ v w)
  (make-posn (+ (posn-x v) (posn-x w))
             (+ (posn-y v) (posn-y w))))
(check-expect (vec+ (make-posn 2 4)(make-posn 5 9)) (make-posn 7 13))
(check-expect (vec+ (make-posn 6 14)(make-posn 0 0))(make-posn 6 14))

;--A random Vec given a [low, high) range for x & y
;--Number < Number Number < Number -> 2D-Vec 
(define (rand-vec xlo xhi ylo yhi)
  (make-posn (+ xlo (random (- xhi xlo))) (+ ylo (random (- yhi ylo)))))

(check-random (rand-vec 3 8 4 9)(make-posn (+ 3 (random (- 8 3))) (+ 4 (random (- 9 4)))))

;--A GamePiece is one of:
;  - (make-ship Number)
;  - (make-ufo  Number)
(define-struct ship [size])  ; A NASA space ship, with a given size
(define-struct ufo  [size])  ; An alien UFO, with a given size

;--An image of one of the GamePieces
;--GamePiece-> Image
(define (piece->image gp)
  (if (ship? gp) (square   (ship-size gp) "solid" "purple")
                 (triangle (ufo-size  gp) "solid" "light purple")))

(check-expect (piece->image (make-ship 40))(square   40 "solid" "purple"))
(check-expect (piece->image (make-ufo 40)) (triangle 40 "solid" "light purple"))

;--An image of the given game piece to the background scene at the given location
;--GamePiece 2D-Vec Image-> Image
(define (piece+scene gp loc scene)
  (place-image (piece->image gp) (posn-x loc) (posn-y loc) scene))

(check-expect (piece+scene (make-ship 40) (make-posn 2 3) (empty-scene 160 90))
              (place-image (square 40 "solid" "purple") 2 3  (empty-scene 160 90)))
(check-expect (piece+scene (make-ufo 40) (make-posn 2 3) (empty-scene 160 90))
              (place-image (triangle 40 "solid" " light purple") 2 3  (empty-scene 160 90)))

; Interpretation: A "movable" is a game piece with a location and a velocity,
; where the last two things are both 2D vectors. Velocity is specified
; in units of pixels-per-clock-tick, in computer-graphics coordinates.
; So a velocity of (make-posn 3 -4) means that every clock tick, the piece
; moves right three pixels, and up four pixels (up, not down, because this
; is computer-graphics coordinates).
(define BG-WIDTH  300)
(define BG-HEIGHT 300)
(define BG (empty-scene BG-WIDTH BG-HEIGHT))

; A Movable is a (make-movable GamePiece Vec Vec)
(define-struct movable [gp loc vel])             ; A moving game piece

;--A scene of a moveable to the scene at its current location
;--Movable Image-> Image
(define (movable+scene mov ms);--place the gp on the scene
  (piece+scene (movable-gp mov) (movable-loc mov) ms))

(check-expect (movable+scene (make-movable (make-ship 40)
                                            (make-posn 3 4)
                                            (make-posn 4 5)) BG)
              (place-image (square 40 "solid" "purple") 3 4 BG))
;--A moveable and makes an image onto a blank-background
;--Moveable-> Image
(define (movable->scene m);--calls moveable+scene 
  (movable+scene m BG))

(check-expect (movable->scene (make-movable (make-ship 40)
                                            (make-posn 3 4)
                                            (make-posn 4 5)))
              (place-image (square 40 "solid" "purple") 3 4 BG))
;--on-tick: moves movable one step by location and velocity
;--Moveable-> Movable
(define (next-movable nm)
  (make-movable (movable-gp nm)
                (vec+ (movable-loc nm)(movable-vel nm))
                (movable-vel nm)))
(check-expect (next-movable (make-movable (make-ship 40)
                                          (make-posn 3 4)
                                          (make-posn 4 5)))
                            (make-movable (make-ship 40)
                                          (vec+ (make-posn 3 4)(make-posn 4 5))
                                          (make-posn 4 5)))
;--Extra Credit (key event)------
;--Turns a key event into a new returning movable
;  (either a ship or ufo in the middle of the screen)
;--Movable "key event"(string)->New Movable
(define GPSIZE 20)
(define (maybe-new-movable m k)
  (cond [(key=? k "x")
         (make-movable (make-ship GPSIZE)
                       (make-posn (/ BG-WIDTH 2)(/ BG-HEIGHT 2))
                       (rand-vec -10 10 -10 10))]
        [(key=? k "y")
         (make-movable (make-ufo GPSIZE)
                       (make-posn (/ BG-WIDTH 2)(/ BG-HEIGHT 2))
                       (rand-vec -10 10 -10 10))]
        [else m]))
;--Movable -> movie ?
(define M1 (make-movable (make-ship GPSIZE)(rand-vec -10 10 -10 10)(rand-vec -10 10 -10 10)))
  (big-bang  M1
            [on-tick next-movable ]
            [to-draw movable->scene]
            [on-key maybe-new-movable])