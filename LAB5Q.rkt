;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname LAB5Q) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;--Quiz 5

;; A Point is a (make-posn Number Number)
;; Interpretation: Represents a point on a Cartesian plane.

;; Template:
(define (point-template pt)
  (... (posn-x pt) ... (posn-y pt) ...))


;; A LOP (List of Points) is one of:
;; - '()
;; - (cons Point LOP)

(define (lop-temp l)
  (cond [(empty? l)...]
        [else (point-template (first l))...
              (lop-temp (rest l))...]))

(define lop1 '())
(define lop2 (cons (make-posn 3 4)
                   (cons (make-posn 5 6)
                         (cons (make-posn 7 8) '()))))