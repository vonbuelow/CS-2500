;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW3_EV) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;Emma Vonbuelow -- Homework 1
(require 2htdp/image)
(require 2htdp/universe)

;--Exercise 1
;--Time = (make-time number [11-0] number [59-00])
(define-struct time [hours minutes])

;--add one minute to a Time
;--minutes (Number)-> Number --helper
(define (add-m m)
  (cond    [(> (add1 m) 59) 0]
           [else (add1 m)]))

;--add one hour to a Time if minutes > 59
;--hours (Number) minutes (Number)-> hours (Number) --helper
(define (add-h h m)
  (cond    [(= (add-m m) 0)
            (cond [(=   (add1 h) 12) 0]
                  [else (add1 h)])]
           [else h]))

;--makes a Time after one minute has passed 
;--Time -> Time
(define (tock t)
  (make-time (add-h (time-hours t)(time-minutes t))(add-m (time-minutes t))))

(check-expect (tock (make-time 0 59))  (make-time 1 0))
(check-expect (tock (make-time 11 59)) (make-time 0 0))

;--take a Time and prints it out as an image of text, like an alarm clock w/ a ":"
;--Time -> Image
(define (time->text t)
  (text (string-append (display-hr (time-hours t))
                       ":"
                       (display-min(time-minutes t))) 40 "black"))

(check-expect (time->text (make-time 0 17))(text "12:17" 40 "black"))
(check-expect (time->text (make-time 0  7)) (text "12:07" 40 "black"))

;--Takes the minutes if min < 10 adding a "0" when displayed (left-padded)
;--Number (in minutes) -> String -- helper
(define (display-min min)
  (cond [(< min 10) (string-append "0" (number->string min))]
        [else (number->string min)]))

;--Takes the hour and displays in a readable way
;--Number (in hours) -> String -- helper 
(define (display-hr hr)
  (cond [(= hr 0) "12"]
        [else (number->string hr)]))

;--Exercise 2
;--Evaluating the following functions either plugin, builtin, and conditional
#|
(define (dist-to-O x y)       
  (sqrt (+ (* x x) (* y y))))
(dist-to-O 3 4)
step 1: (dist-to-O 3 4)             plugin
step 2: (sqrt (+ (* 3 3) (* 4 4)))  builtin 
step 3: (* 3 3)= 9                  builtin
step 4: (* 4 4)= 16                 builtin
step 5: (+ 9 16)= 25                builtin
step 6: (sqrt 25)= 5                builtin

(define-struct point [x y])
(define (dist-to-O1 p)
  (sqrt (+ (sqr (point-x p))
           (sqr (point-y p)))))
step 1:(dist-to-O (make-point 3 4))                                              plugin
step 2:(sqrt (+(sqr (point-x (make-point 3 4)))(sqr (point-y (make-point 3 4)))))builtin
step 3:(point-x (make-point 3 4))= 3                                             plugin
step 4:(sqr 3)=9                                                                 builtin
step 5:(point-y (make-point 3 4))= 4                                             plugin
step 6:(sqr 4)= 16                                                               builtin
step 7:(+ 9 16)= 25 step 8:(sqrt 25)= 5                                          builtin

(define (step x)
  (cond [(< 1 x)  (sqr x)]
        [(< 0 x)  (* 2 x)]
        [else     (sqr (+ x 1))]))
(step 0)
step 1:(step 0)                                                       plugin
step 2:(cond ((< 1 0) (sqr 0)) ((< 0 0) (* 2 0)) (else (sqr (+ 0 1))))cond.
step 3:(< 1 0) step 4: #false                                         builtin
step 5:(cond (#false (sqr 0))((< 0 0) (* 2 0))(else (sqr (+ 0 1))))   cond.
step 6:(cond ((< 0 0) (* 2 0))(else (sqr (+ 0 1))))                   cond.
step 7:(< 0 0) step 8: #false                                         builtin
step 9:(cond(#false (* 2 0))(else (sqr (+ 0 1))))                     cond.
step 10:(cond (else (sqr (+ 0 1))))                                   cond.
step 11:(cond (else (sqr (+ 0 1))))                                   cond.
step 12:(sqr (+ 0 1)) step 13:(+ 0 1)= 1 step 14:(sqr 1)=1            builtin|#

;--Exercise 3
;--Stocks = (make-stock String String Number Number)
(define-struct stock [cname symbol pricel priceh])

;--Compute the average price on a given day for a company
;--Stocks -> Number
(define (avprice s)
  (/ (+ (stock-priceh s)(stock-pricel s)) 2))

(check-expect (avprice (make-stock "zooinks"   "✩"  25 75))     50)
(check-expect (avprice (make-stock "google"    "G"   40 20))     30)
(check-expect (avprice (make-stock "Hallmark" "*H*"    0 0))      0)