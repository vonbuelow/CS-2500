;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname LAB9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;-- Lab 9 Designing and Using Abstractions
;--


; find-first-match: (X) [List-of X][X-> Boolean] X -> X
; find first item in the list that meets the condtion
; or the default value if no such item is found 

(define (find-first-match lox test-item? default)
  (cond [(empty? lox) default]
        [(test-item? (first lox)) (first lox)]
        [else (find-first-match (rest lox) default)]))

(define (matching-x-posn pts x default)
  (find-first-match pts (λ (pt) (= (posn-x pt) x)) default))

(define (string-with-length strs len)
  (find-first-match strs (λ (str) (= (string-length str) len))
                    "no such string"))

(define-struct pair [x y])
 
; A StringPair  is a (make-pair String String)
; A NumBoolPair is a (make-pair Number Boolean)

; A (X)(Y)Pair is a (make-pair X Y)
; if X=Y XPAir

; A StringPair is a (make-pair X X) X=Y=String
; A NumBoolPair is a (make-pair X Y) X=Number Y=Boolean


;[Number-> String] [List-of [List-of Numbers]] Boolean -> Boolean
(define (f x y z)
  (or
;  [String-> Boolean][List-of String] -> Boolean
   (andmap string-numeric?
           ; [Number-> String][List-of Numbers]-> [List-of String]
              (map x
                   ; [Number -> Boolean] [List-of Numbers]-> [List-of Numbers]
                   (filter positive?
                           ; [X [List-of Number] -> [List-of Number]] [List-of Number] [List-of [List-of Numbers}]
                           ; -> [List-of Number]
                             (foldr append '() y)))) z))



;append: list of x and list of x to another list of x 
; y= list
; filter- list of positive numbers
; x= function LON-> LOS (map function (list of positive numbers or '())
; z= boolean

(define (string-lengths los)
  (map string-length los))

(define (close? lop d)
  (ormap (λ (pt) (<= (sqrt (+ (sqr (posn-x pt))
                              (sqr (posn-y pt))))
                     d)) lop))

(check-expect (close? '((make-posn 1 2)
                        (make-posn 3 4)
                        (make-posn 5 6)) 4) #t)

;-- (X Y)[X Y-> Y] Y [ListOf X] -> Y
(define (foldr.c op base xs)
  (if (empty? xs) base
      (op (first xs)
          (foldr op base (rest xs)))))
