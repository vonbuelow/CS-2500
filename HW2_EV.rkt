;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW2_EV) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
Emma Vonbuelow
Homework 2
|#
(require 2htdp/image)

;--Exercise 1
;--Check for duplicated characters within a string (pattern: xx)
;--String-> Boolean
(define (duplicated? s)
  (string=? (substring s 0 (round (/ (string-length s)2)))
            (substring s   (round (/ (string-length s)2)))))

(check-expect (duplicated? "yomamayomama") #t)
(check-expect (duplicated? "yomamaymama")  #f)

;--Exercise 2
;--Print an image of a pine tree with 5 triangle "branches" of increasing length starting with a and increasing by a factor of d by w/ a trunk 
;--Number-> Image
(define ANGLE 120)
(define (pine-tree a d)
  (above (isosceles-triangle a            ANGLE "solid" "forest green")
         (isosceles-triangle (+ a d)      ANGLE "solid" "forest green")
         (isosceles-triangle (+ a (* 2 d))ANGLE "solid" "forest green")
         (isosceles-triangle (+ a (* 3 d))ANGLE "solid" "forest green")
         (isosceles-triangle (+ a (* 4 d))ANGLE "solid" "forest green")
         (rectangle               a (* 1.5 a) "solid" "brown")))
;There could be constants for the color of the branches & the kind of outline

;--Exercise 3
;--a function subset-interval? to determine if the first interval [a,b] is a subset of [c, d]
;--Number-> Boolean
(define (subset-interval? a b c d)
  (cond
    [(> a b)                           #t]
    [(and (<= c a)(<= b d)(<= a b))    #t]
    [else                              #f]))

(check-expect (subset-interval? 5 4 5 8) #t)
(check-expect (subset-interval? 1 2 54 90) #f)
(check-expect (subset-interval? 55 78 54 90) #t)
(check-expect (subset-interval? 3 7 2 10) #t)
(check-expect (subset-interval? 32 7 2 10) #t)
(check-expect (subset-interval? 3 3 2 10) #t)
(check-expect (subset-interval? 3 7 2 10) #t)

;--Exercise 4
;--Return truth table for implication given two booleans x and y
;--Boolean-> Boolean 
(define (=> x y)
  (cond [x y]
        [else #t]))

(check-expect (=> #f #f) #t)
(check-expect (=> #f #t) #t)
(check-expect (=> #t #t) #t)
(check-expect (=> #t #f) #f)
;--(check-expect (=> (Boston capital of moon) (2 exams in Fundies)) #t)