;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW5_EV) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;Emma Vonbuelow--Homework 5
(require 2htdp/image)
(require 2htdp/universe)

;--HtDP/2e Problems
;--A LON (list of num.) is one of:
;  -'()
;  -(cons Number LON)
#;
(define (lon-temp lon)
  (cond [(empty? lon)...]
        [else.. (first lon)...
                (lon-temp (rest lon))...]))

;--163:
;--A LOM (list of measurements in fahrenheit (numbers)) is one of:
;  -'()
;  -(cons Number LOM)

#;
(define (lom-temp lom)
  (cond [(empty? lom)...]
        [else (first lom)...
              (lon-temp (rest lom))...]))

;--A list of measuments in Fahrenheit to list of Celcius measurements
;--List of Numbers-> List of Numbers
(define (convertFC l)
  (cond [(empty? l) '()]
        [else (cons (convert(first l))
                    (convertFC (rest l)))]))

(define L0 (cons 32 '()))

(check-expect (convertFC L0) (cons 0 '()))
(check-expect (convertFC '())         '())

;--Fahrenheit to Celcius
;--Number-> Number
(define (convert f)
  (* (/ 5 9)(- f 32)))

(check-expect (convert 32) 0)

;--164:
;--A list of USD prices (Numbers)-> a list of Euro prices (Numbers)
;--List of Numbers-> List of Numbers
(define (convert-euro l)
  (cond [(empty? l) '()]
        [else (cons (convertp (first l))
                    (convert-euro (rest l)))]))
;--Example:
(define L1 (cons 100 (cons 70 (cons 0 '()))))

(check-expect (convert-euro L1)(cons (* 100 0.631)
                                     (cons (* 70 0.631)
                                           (cons 0 '()))))
(check-expect (convert-euro '()) '())
;--USD $-> Euro
;--Number-> Number
(define (convertp p)
  (* 0.631 p))
(check-expect (convertp 100) 63.10)
(check-expect (convertp 90) (* 0.631 90))

;--A list of US currency w/ an exchange rate-> a list of European currency
;--List of Numbers Number-> List of Numbers
(define (convert-euro* l ex)
  (cond [(empty? l) '()]
        [else (cons (convertp.v2 (first l)  ex)
                    (convert-euro* (rest l) ex))]))

(check-expect (convert-euro* (cons 100 '()) 0.631) (cons 63.10 '()))
(check-expect (convert-euro* (cons 87  '()) 0.546) (cons (* 87 .546) '()))
(check-expect (convert-euro* '() 0) '())

;--USD $-> Currency??
;--Number-> Number 
(define (convertp.v2 l ex)
  (* ex l))

(check-expect (convertp.v2 67 .87)(* 67 .87))
(check-expect (convertp.v2 100 .54)(* 100 .54))

;--165:
;; A ListOfToyDescriptions (LOT) is one of:
;; - '()
;; - (cons String LOT)
;; where the String is a one-word description of the toy

;; lot-template : LOT -> ??
#;
(define (lot-template l)
  (cond [(empty? l)...]
        [else...(first l)
                (lot-template (rest l))]))

;; subst-robot : LOT -> LOT
;; replaces all occurrences of "robot" with "r2d2"

(define tl1 '())
(define tl2 (cons "fun" '()))
(define tl3 (cons "fun" (cons "robot" (cons "large" '()))))
(define tl4 (cons "fun" (cons "robot" (cons "large" (cons "robot" '())))))

(check-expect (subst-robot tl1) tl1)
(check-expect (subst-robot tl2) tl2)
(check-expect (subst-robot tl3) (cons "fun" (cons "r2d2" (cons "large" '()))))
(check-expect (subst-robot tl4) (cons "fun" (cons "r2d2" (cons "large" (cons "r2d2" '())))))

(define (subst-robot tl)
  (cond [(empty? tl) '()]
        [(string=? "robot" (first tl))
         (cons "r2d2" (subst-robot (rest tl)))]
        [else (cons (first tl)
                    (subst-robot (rest tl)))]))

;; substitute : LOT -> LOT
;; replace every occurrence of some string in a list with some other string

(define (substitute tl old new)
  (cond [(empty? tl) '()]
        [(string=? old (first tl))
         (cons new (substitute (rest tl) old new))]
        [else (cons (first tl)
                    (substitute (rest tl) old new))]))

(define tl5 (cons "fun" (cons "robot" (cons "large" (cons "fun" '())))))

(check-expect (substitute tl1 "fun" "exciting") tl1)
(check-expect (substitute tl2 "fun" "exciting") (cons "exciting" '()))
(check-expect (substitute tl3 "playful" "exciting") (cons "fun" (cons "robot" (cons "large" '()))))
(check-expect (substitute tl5 "fun" "exhilirating")
              (cons "exhilirating" (cons "robot" (cons "large" (cons "exhilirating" '())))))
;--167: 
;-- Position Constants  
(define posn1 (make-posn 1 2))
(define posn2 (make-posn 3 4))
(define posn3 (make-posn 5 6))

(define posn-list1 (cons posn1 '()))
(define posn-list2 (cons posn1 (cons posn2 '())))
(define posn-list3 (cons posn1 (cons posn2 (cons posn3 '()))))
(define empty-list '())

;-- Template
#;
(define (sump-temp h)
  (cond [(empty? h) ...]
        [else ... (+ (posn-x (first h)) ... (sum-temp (rest h)))]))

;--Takes a list of positions and produces the sum of all its x-coordinates
;--sum : List-of-Positions -> Number
(define (sum h)
  (cond [(empty? h) 0]
        [else (+ (posn-x (first h)) (sum (rest h)))]))

;-- Tests
(check-expect (sum posn-list1) 1)
(check-expect (sum posn-list2) 4)
(check-expect (sum posn-list3) 9)

;--Problem 1--------------------------------------------------

(define-struct ball [x y color])
; Ball = (make-ball Number Number Color)
; Color is one of 'red, 'yellow, 'blue, etc.
#;
(define (ball-temp b)
  (...(...(ball-x b))
      (...(ball-y b))
      (...(ball-color b))))

;--A LOB (list of Balls) is one of:
;  -'()
;  -(cons Ball LOB)

;--LOB-> ??
#;
(define (lob-temp l)
  (cond [(empty? l) ...]
        [else ... (ball-temp (first l))
              (lob-temp (rest l))]))

(define l1 (cons (make-ball 50 75 "red")
                 (cons (make-ball 150 50 "yellow")
                       (cons (make-ball 200 275 "blue") '()))))
(define l2 (cons (make-ball 50 75 "red")
                 (cons (make-ball 150 50 "yellow")
                       (cons (make-ball 400 50 "green")
                             (cons (make-ball 200 275 "blue") '())))))

;--How many balls are on a list of balls
;--LOB-> Number
(define (lob-length l)
  (cond [(empty? l) 0]
        [else (+ 1 (lob-length (rest l)))]))
(check-expect (lob-length l1) 3)
(check-expect (lob-length '()) 0)
(check-expect (lob-length (cons (make-ball 45 200 "purple") '())) 1)

;--All x-cords in a list of balls
;--LOB-> LON?
(define (lob-xs l)
  (cond [(empty? l) '()]
        [else (cons (ball-x (first l))
                    (lob-xs (rest l)))]))

(check-expect (lob-xs l1)(cons 50 (cons 150 (cons 200 '()))))
(check-expect (lob-xs '()) '())
(check-expect (lob-xs (cons (make-ball 45 200 "purple") '())) (cons 45 '()))

;--LOB to an empty scene as circles with respective colors and radius 3
;--LOB-> Image
(define (lob-draw l)
  (cond [(empty? l) (empty-scene 300 300)]
        [else (place-image (ball-image(first l))
                           (ball-x (first l)) (ball-y (first l))
                           (lob-draw (rest l)))]))

(check-expect (lob-draw l1)
              (place-image (circle 3 "solid" "red") 50 75
                           (place-image (circle 3 "solid" "yellow") 150 50
                                        (place-image (circle 3 "solid" "blue") 200 275
                                                     (empty-scene 300 300)))))  

;--Image of ball
;--Ball-> Image
(define (ball-image b)
  (circle 3 "solid" (ball-color b)))

(check-expect (ball-image (make-ball 10 10 "yellow"))(circle 3 "solid" "yellow"))

;--LOB of visible balls (within the scene of 300x300)
;--LOB-> LOB (specific x y cords)
(define (lob-visibles l)
  (cond [(empty? l) '()]
        [else (cond [(and (<= (ball-x (first l)) 300)
                          (<= (ball-y (first l)) 300))
                     (cons (first l) (lob-visibles (rest l)))]
                    [else (lob-visibles (rest l))])]))

(check-expect (lob-visibles l1) l1)
(check-expect (lob-visibles '()) '())
(check-expect (lob-visibles l2) l1)

;--Is a specific Ball in a LOB?
;--LOB Ball-> Boolean
(define (contains? lob b)
  (cond [(empty? lob) #f]
        [else (or (equal? (first lob) b)
                  (contains? (rest lob) b))]))

(check-expect (contains? l1 (make-ball 150 50 "yellow")) #t)
(check-expect (contains? '() (make-ball 150 50 "yellow")) #f)
(check-expect (contains? l2 (make-ball 150 50 "orange")) #f)

;--Problem 2-----------------------------------------------
; Txt = (make-txt String Number Number)
(define-struct txt [content x y])

(define (txt-temp t)
  (...(...(txt-content t))
      (...(txt-x t))
      (...(txt-y t))))
 
; LoTxt is one of: 
; – empty 
; – (cons Txt LoTxt)
#;
(define (lotxt-temp l)
  (cond [(empty? l)...]
        [else (txt-temp (first l))
              (lotxt-temp (rest l))]))

(define HEIGHT 400)
(define WIDTH  400)
(define BG (empty-scene HEIGHT WIDTH))

(define fulltext (cons (make-txt "On your mark." (/ WIDTH 2) 150)
                       (cons (make-txt "Get set." (/ WIDTH 2) 200)
                             (cons (make-txt "Go!" (/ WIDTH 2) 250) '()))))

; A World is a (make-world Image LoTxt)
(define-struct world [image hidden])

(define (world-temp w)
  (...(...(world-image w))
      (...(lotxt-temp (world-hidden w)))))

; intepretation: 
;  The world's image represents the image that the audience can see.
;  The world's list of Txt represents the yet-to-be-revealed elements.

;--Current image
;--World-> Image
(define (display w)
  (world-image w))

(check-expect (display WORLD-0) BG)

(define FONT-SIZE 20)
(define FONT-COLOR "Blue")


;--Next world revealing hidden text
;--World-> World
(define (next w)
  (cond [(empty? (world-hidden w)) w] ;-- LOT return the World BG
        [else (make-world (+txt (display w) (first (world-hidden w)));<-- Image 
                          (rest (world-hidden w)))]))

(define WORLD-0 (make-world BG fulltext))
(check-expect (next WORLD-0)(make-world
                             (place-image (text "On your mark." 20 "Blue") 200 150 BG)
                             (rest (world-hidden WORLD-0))))
(check-expect (next (make-world BG '()))(make-world BG '()))

;--Adds text to background (image)
;--Background (Image) Txt -> Image 
(define (+txt bg txt)
  (place-image (text (txt-content txt) FONT-SIZE FONT-COLOR)
               (txt-x txt)
               (txt-y txt)
               bg))

(check-expect (+txt BG (make-txt "yellow" 60 70))
              (place-image (text "yellow" FONT-SIZE FONT-COLOR) 60 70 BG))
(check-expect (+txt BG (make-txt "orange" 200 130))
              (place-image (text "orange" FONT-SIZE FONT-COLOR) 200 130 BG))

(big-bang WORLD-0
  (on-tick next 1)
  (on-draw display))
