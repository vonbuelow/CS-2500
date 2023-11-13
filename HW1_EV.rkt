;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW1_EV) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;Emma Vonbuelow -- Homework 1
(require 2htdp/image)
(require 2htdp/universe)
;--Exercise 1
#|
29550--tuition
5870--dorm
4090--meal plan
620--misc fee
37--lectures one semester
4--courses a semester
|#
(/ (+ 29550 5870 4090 620)(* 37 4));<- unrounded
(round (/ (+ 29550 5870 4090 620)(* 37 4)));<-rounded

;--Exercise 2
(define (poly1 x)(+ (- (*(expt x 3)(/ 1 100)) (* (sqr x) (/ 2 100))) 1 (* 2 x)))
;--Exercise 3
(define (poly2 x)(+ 1 (* x (+ 2(* x (+ (/ -2 100)(/ x 100)))))))
;;;tests
;(poly1 1)
;(poly2 1)
;(poly1 2)
;(poly2 2)

;--Exercise 4
;;;a
(define SCENE
  (empty-scene 500 500))
(define DOT
  (circle 5 "solid" "pink"))
;;;b
(define (poly-diff->scene x)
  (place-image DOT x (abs (+ 50 (- (poly1 x) (poly2 x))))
               SCENE))
;;;c
;(animate poly-diff->scene) <-interactions window----a dot going in a straight line

#|summarize findings:
[removed the bug]
the friend should be convinced the two polynomials are the same since the animation
shows there is no difference between the two functions as the dot moves in a continuous line,
but after introducing a bug the difference in the two functions causes a slope in the
dots movement. 
|#

;--Exercise 5
#|12 homeworks--30% total grade
30%/12=2.5%
11 homeworks--x
12 hw--different y
first homework should count less
HW02-HW12==
HW01--p[0,100]
30=x(12)
30=11x+(11x/)
11x + p/100 * x = 30
y = p/100 * x
|#
(define (hw02-12 p)(/ 3000 (+ 1100 p)))
;--Exercise 6
(define (hw01 p)(* (/ p 100)(hw02-12 p)))
;--Exercise 7
(define (histogram-bar h)
  (rectangle 30 (abs (- (* 50 h) 20)) "outline" "pink"));<- w/o the abs got an error following the original formula
;--Exercise 8
(define (histogram p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)
  (beside/align "bottom"
                (histogram-bar (hw01 p0))
                (histogram-bar (hw02-12 p1))
                (histogram-bar (hw02-12 p2))
                (histogram-bar (hw02-12 p3))
                (histogram-bar (hw02-12 p4))
                (histogram-bar (hw02-12 p5))
                (histogram-bar (hw02-12 p6))
                (histogram-bar (hw02-12 p7))
                (histogram-bar (hw02-12 p8))
                (histogram-bar (hw02-12 p9))
                (histogram-bar (hw02-12 p10))))
#|
The value of p must be within the range of [0, 100] in order to represent a clear
distibution of weight and regarding effort, besides the first hw assignment 
|#


