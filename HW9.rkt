;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;-- Homework 9 Kushi Amara and Emma Vonbuelow

;--Exercise 1
;-- Does a string occur an even amt of times?
;-- str-in-list-even?/v1: String LOS -> Boolean
(define (str-in-list-even?/v1 los gs)
  (even? (length (filter (λ (s)
                           (string=? s gs))
                         los))))

(check-expect (str-in-list-even?/v1 '("foo" "bar" "fab" "fred" "foo") "foo") #t)
(check-expect (str-in-list-even?/v1 '() "foo") #t)
(check-expect (str-in-list-even?/v1 '("foo" "bar" "fab" "fred" "foo") "bar") #f)

;--Exercise 2
;-- Does a string occur an even amt of times?
;-- str-in-list-even?/v2: String LOS -> Boolean
;-- [ListOf Strings] String -> Boolean
(define (str-in-list-even?/v2 l gs)
  (foldr (λ (s prev-bool)
           (if (string=? s gs)
               (not prev-bool)
               prev-bool))
         #t l))

(define los1 '())
(define los2 '("dog" "dog" "cat" "dog" "frog" "cat" "eel" "cat" "cat"))

(check-expect (str-in-list-even?/v2 los1 "frog") #t)
(check-expect (str-in-list-even?/v2 los2 "dog") #f)
(check-expect (str-in-list-even?/v2 los2 "cat") #t)

;--Exercise 3
;--Applies the first argument function to each number
;--if passes predicate test, else add original X to the list
;--do-to-all-if: (X Y) [X -> Boolean] [X -> Y] [ListOf X] -> [ListOf Y]
(define (do-to-all-if test? f lon)
  (map (λ (n)
         (if (test? n)
             (f n)
             n))
       lon))

(check-expect (do-to-all-if negative? sub1 (list 10 -10 -20 20))
              (list 10 -11 -21 20))
(check-expect (do-to-all-if string? string-downcase (list "YELLOW" -10 "BeRt" 20))
              (list "yellow" -10 "bert" 20))
(check-expect (do-to-all-if false? not (list #t #f -20 #f))
              (list #t #t -20 #t))

;--Exercise 4
;--Join together all strings
;--concat-all-strs:[ListOf String] -> String
(define (concat-all-strs los)
  (foldr string-append "" los))

(check-expect (concat-all-strs (list "hello" "world" "!"))
              "helloworld!")
(check-expect (concat-all-strs '())
              "")
(check-expect (concat-all-strs (list "foo " "bar " "!"))
              "foo bar !")
;--Exercise 5
;--Largest number
;--biggest-number: [ListOf NonNegativeNumbers] -> Number
(define (biggest-number lon)
  (foldr max 0 lon))

(check-expect (biggest-number (list 3 432 532 890 20 234 21 1200))
              1200)
(check-expect (biggest-number (list 3 432 532 890 20 234 21 12))
              890)
(check-expect (biggest-number '())
              0)
;--Exercise 6
;--Volume number containing a given page
;--page->volume: Number [ListOf Number] -> Number
;(define (page->volume pg l)
;  (if page >= first < second ) -> index n-1)
(define (page->volume pg l)
  (foldr (λ (start-pg n)
           (if (< pg start-pg)
               n
               (+ 1 n)))
         0 l))
         
(check-expect (page->volume 150 (list 101 201 301)) 1)
(check-expect (page->volume 300 (list 101 201 301)) 2)
(check-expect (page->volume 301 (list 101 201 301)) 3)
(check-expect (page->volume 999 (list 101 201 301)) 3)
(check-expect (page->volume   1 (list 101 201 301)) 0) ; volume not found

;--Exercise 7
;--Are all items between the lower & upper limits
;--all-in-range?: (X)[X X -> Boolean] X X [ListOf X] -> Boolean
(define (all-in-range? range? lowlim uplim lx)
  (andmap (λ (x) (and (range? lowlim x)(range? x uplim))) lx))

(check-expect (all-in-range? < 5 10 (list 6 8 9)) #t)
(check-expect (all-in-range? < 5 10 (list 1 2 9)) #f)
(check-expect (all-in-range? string<=? "aardvark" "bananas" (list "axe" "bad"))
              #t)
(check-expect (all-in-range? string<=? "aardvark" "bananas" (list "axe" "cat"))
              #f)  

;--Exercise 8
;--Union of two sets, repetitions are not allowed 
;--union: [ListOf Number] [ListOf Number]
(define (union s1 s2)
  (foldr (λ (n s) (if (contains? n s) s (elt+set n s))) s2 s1))

   
(check-expect (union '(3 4 5 6 7 8 9) '(0)) '(3 4 5 6 7 8 9 0))
(check-expect (union '(3 4 5 6) '()) '(3 4 5 6))
(check-expect (union '() '()) '())
(check-expect (union '(1 2 3 4) '(2 3 4 5 6)) '(1 2 3 4 5 6))

; NumSet = [ListOf Number]
; Order of items in the list irrelevant; repetitions not allowed.
 
; NumSet Number -> Boolean
; Is the number a member of the set?
(define (contains? n s)
  (ormap (λ (elt) (= elt n)) s))

; Add the element e to set s.
; Number NumSet -> NumSet
(define (elt+set e s)
  (if (contains? e s) s
      (cons e s)))

