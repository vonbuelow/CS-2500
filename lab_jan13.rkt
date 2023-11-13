;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab_jan13) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; lab 1--january 13th, 2023
(require 2htdp/image)
(require 2htdp/universe)

;--exercise 5 
(define (binomial a b)(expt (+ a b) 2));exp or a ^

;--exercise 7
(define (binomial.2 a b)(sqr (+ a b)))

;--exercise 8
(define (alternative-names first last)
  (string-append "'"first" "last"' or '"last", "first))
;(alternative-names "Joseph" "Aoun")

;--exercise 9
(define my-fname "Emma Rose");<- with middle name is still meaningful
(define my-lname "Vonbuelow")
;(alternative-names my-fname my-lname)

;--exercise 10
;(alternative-names my-fname my-lname);<-when clicking run expresses output
;--exercise 11: error message given needing a number not a string
;--entered in the interactions window

;--exercise 12: the step button?

;--exercise 13
(define SQR1
  (square 30 "solid" "red"))
(define SQR2
  (square 30 "solid" "black"))

;--exercise 14
(define chessB
  (above (beside SQR2 SQR1 SQR2)
  (beside SQR1 SQR2 SQR1)
  (beside SQR2 SQR1 SQR2)))


  
          


