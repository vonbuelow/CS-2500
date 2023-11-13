;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname quiz2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;--Quiz 2

(define-struct pie [size topping])

;;; A Pizza is a (make-pie Number String)
;;; Interpretation:
;;; - The size field is the radius of the pizza in inches
;;; - A pizza can have one extra topping, e.g., "mushrooms"
;;;   or "pepperoni"

;;; Examples:
(define p1 (make-pie  6 "pepperoni")) ; Individual-size pizza with pepperoni
(define p2 (make-pie 16 "chicken"))  ; Large pizza with mushrooms

;any large pizza (size 12 inches or better)
;with chicken topping can be ordered with a discount

;Determine if a pizza order gets a discount (chicken and <=12 inches)
;special?: Pizza->Boolean
(define (special? p)
  (and (>= (pie-size p) 12)(string=? (pie-topping p) "chicken")))

(check-expect (special? p2) #t)

;sig
;purpose
;