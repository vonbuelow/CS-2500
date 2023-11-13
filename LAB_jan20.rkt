;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname LAB_jan20) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;--Emma Vonbuelow Lab 2
(require 2htdp/image)

;--Exercise 1
;--rewards: Natural-> Natural
;--How many points depending on the day

(define (rewards.2 day)
  (if (> day 5) 300
      (+ 100 (* 50 (- day 1)))))
(check-expect (rewards.2 1) 100)

;--Exercise 2
;--draw-house: number->image
;--given a size displays a house with its respective size
(define (draw-house size)
  (above (triangle size "solid" "black")
         (overlay/align "middle" "bottom"
                        (rectangle (* (/ 2 5)size)(* (/ 1 5)size) "solid" "yellow")
                        (square    size                           "solid" "red"))))

;--Exercise 3
;--pigify string-> string
;--pig latin translator 

(define (pigify word)
  (string-append (substring word 1)(substring word 0 1)"ay"))

(check-expect (pigify "fundies") "undiesfay")
;--Exercise 4
;--begins-with-vowels?: String-> Boolean
;--checks if the string starts with a vowel

(define (begins-with-vowel? word)
  (or (string=? (substring word 0 1)"a")
             (string=? (substring word 0 1)"e")
             (string=? (substring word 0 1)"i")
             (string=? (substring word 0 1)"o")
             (string=? (substring word 0 1)"u")))

(check-expect (begins-with-vowel? " ") #f)
(check-expect (begins-with-vowel? "appa") #t)
(check-expect (begins-with-vowel? "goober") #f)

;--Exercise 5
;--Consonant?: String-> Boolean
;--Returns a #t or #f if it doesnt have a vowel

(define (consonant? word)
  (not (begins-with-vowel? word)))

(check-expect (consonant? " ") #t)
(check-expect (consonant? "appa") #f)
(check-expect (consonant? "goober") #t)

;--Exercise 6
;--String-> String
;--New pig latin rules

(define (pigify.v2 word)
  (if (not (begins-with-vowel?l2 word))
      (string-append (substring word 2)(substring word 0 3)"ay")
      (if (begins-with-vowel? word)
          (string-append (substring word 1)"ay")
          (pigify word))))
  
(define (consonant?l2 word)
  (not (begins-with-vowel?l2 word)))
(define (begins-with-vowel?l2 word)
  (or        (string=? (substring word 1 2)"a")
             (string=? (substring word 1 2)"e")
             (string=? (substring word 1 2)"i")
             (string=? (substring word 1 2)"o")
             (string=? (substring word 1 2)"u")
             (string=? (substring word 0 1)"a")
             (string=? (substring word 0 1)"e")
             (string=? (substring word 0 1)"i")
             (string=? (substring word 0 1)"o")
             (string=? (substring word 0 1)"u")))
#|
(define (quiz s double?)
  (cond [(double?) (string-append s s)]
        [else (s)]))

(define (double-string s double?)
  (if double?
       (string-append s s)
       s))


|#     
                        