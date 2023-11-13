;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname extra) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


#| Version 1 

(define (next w)
  (place-image
   (cond [(empty? (world-hidden w)) BG]
         [else
          (text (txt-content (first (world-hidden w)))
                (txt-x (first (world-hidden w)))
                (txt-y (first (world-hidden w)))
                BG)
          (rest l)])
   (txt-x (first (world-hidden w)))
   (txt-y (first (world-hidden w)))
   (display w))

;--The next string in a LoTxt
;--LoTxt-> String
(define (next-txt l)
  (cond [(empty? l) BG]
        [else (text ((txt-content (first l))
                    (txt-x (first l))
                    (txt-y (first l))
                    ))
              (next-txt (rest l))]))
|#

#|(define (next w)  
    (place-image  (text (txt-content (first (rest (world-hidden w)))) FONT-SIZE FONT-COLOR)
                  (txt-x (next (first (rest (world-hidden w)))))
                  (txt-y (next (first (rest (world-hidden w)))))
                  BG))|#


#|(define (next w)
  (cond [(empty? (world-hidden w)) (display w)]
        [else (place-image (text (txt-content (first (world-hidden w))) 20 "blue")
                           (txt-x (first (world-hidden w))) (txt-y (first (world-hidden w))
(display (make-world (next w) (rest (world-hidden w)))))]))|#














