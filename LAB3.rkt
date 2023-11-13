;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname LAB3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;--Emma Vonbuelow Lab 3

;--atomic data

(define PERCENT-0 0)
(define PERCENT-50 50)
(define PERCENT-100 100)

;(define (percent p)
;  (...p..))

;--TrafficLight->???
#|(define (traffic-templ t)
 (cond [(string=? TRAFFIC-R t)...]
       [(string=? TRAFFIC-Y t)...]
       [(string=? TRAFFIC-Y t)...]);<--no else
  )
|#

;structs

;--Employee is a (make-employee String String Number Number)
;--where fname is the employees first name
;--      lname is the employees last name
;--      wage is their hourly wage, in dollars
;--      and ssm is their social security number
;--and represents an employee
(define-struct employee [fname lname wage ssn])

(define E1 (make-employee "sue" "smith" 1000 123456789)); <-Examples 
(define E2 (make-employee "Abc" "Def"      2 987654321))

;--functions for an employee
;--Constructior:         make-employee
;--Recognizer/Predicate: employee?
;--Selector:             employee-fname, employee-lname, employee-wage, employee-ssn
;--                      (employee-fname E1)...

;--template for an Employee

;--employee-temp1: Employee-> ??
(define (employee-temp1 e)
  (...  (...(employee-fname e))
        (...(employee-lname e))
        (...(employee-wage e))
        (...(employee-ssn e))))


;--Exercise 1
; A Photo is a (make-photo Image String)
(define-struct photo [img tag])
; - where img is the actual image
; - and tag is an identifying label for the image

;--functions for a photo
;--Constructior:         make-photo
;--Recognizer/Predicate: photo?
;--Selector:             photo-img, photo-tage
;--                      (photo-imp p)...
 
; A 3D is a (make-3d Number Number Number)
(define-struct 3d [x y z])
; - where x is the x-coordinate of the point
; - y is the y-coordinate of the point
; - and z is the z-coordinate of the point

;--functions for 3D
;--Constructior:         make-3d
;--Recognizer/Predicate: 3d?
;--Selector:             3d-x, 3d-y, 3d-z
;--                      (3d-x d)...
 
; A TA is a (make-ta String String Number)
(define-struct ta [last given lab])
; - where last is the TA's last name
; - given is the TA's first name
; - and lab is the lab number this TA leads

;--functions for a ta
;--Constructior:         make-ta
;--Recognizer/Predicate: ta?
;--Selector:             ta-last, ta-given, ta-lab
;--                      (ta-last t)...

;--Exercise 2
; An Item is a (make-item String PositiveNumber)
(define-struct item [tag price])
; - where tag is the name of the item
; - and price is the price of an item

 
; A PHDStudent is a (make-phd String GrantId PositiveNumber)
(define-struct phd [name grant pay-rate])
; - where name is the full name of the student
; - grant is the grant ID number of their current grant
; - and pay-rate is their hourly wage
 
; A GrantId is one of:
; - "1-123"
; - "3-AB4"
; - "9-999"

(define phd1 (make-phd "glob bob" "1-123" 8907))
(define phd2 (make-phd "rob job" "3-AB4" 9028))

;--Exercise 3
;An Animal is a (make-animal String String Number Number [1-12] Number [1-12]
(define-struct animal [name species age bhour dhour])

;--Exercise 4
;--Construct a template for functions that process Photos.

; A Photo is a (make-photo Image String)
;(define-struct photo [img tag])
; - where img is the actual image
; - and tag is an identifying label for the image

;--functions for a photo
;--Constructior:         make-photo
;--Recognizer/Predicate: photo?
;--Selector:             photo-img, photo-tag
;--                      (photo-imp p)...

;--photo-temp1: Photo-> 
(define (photo-temp1 p)
  (...  (...(photo-img p))
        (...(photo-tag p))))

;--Exercise 5

; A 3D is a (make-3d Number Number Number)
;(define-struct 3d [x y z])
; - where x is the x-coordinate of the point
; - y is the y-coordinate of the point
; - and z is the z-coordinate of the point

;--functions for 3D
;--Constructior:         make-3d
;--Recognizer/Predicate: 3d?
;--Selector:             3d-x, 3d-y, 3d-z
;--                      (3d-x d)...

;--3d-temp1: 3d-> ??
(define (3d-temp1 d)
  (...  (...(3d-x d))
        (...(3d-y d))
        (...(3d-z d))))

;--Exercise 6
; A TA is a (make-ta String String Number)
;(define-struct ta [last given lab])
; - where last is the TA's last name
; - given is the TA's first name
; - and lab is the lab number this TA leads

;--functions for a ta
;--Constructior:         make-ta
;--Recognizer/Predicate: ta?
;--Selector:             ta-last, ta-given, ta-lab
;--                      (ta-last t)...

;--ta-temp1: TA-> ???

; A TA is a (make-ta String String Number)
;(define-struct ta [last given lab])
; - where last is the TA's last name
; - given is the TA's first name
; - and lab is the lab number this TA leads

;--functions for a ta
;--Constructior:         make-ta
;--Recognizer/Predicate: ta?
;--Selector:             ta-last, ta-given, ta-lab
;--                      (ta-last t)...


;--Exercise 

(define MIN-WAGE 10)
; paid-enough? : Employee -> Boolean
; Is this employee making more than minimum wage?

(check-expect (paid-enough? (make-employee "sam" "fisher" 7 123456789)) #f)
(check-expect (paid-enough? E1) #t)
(check-expect (paid-enough? (make-employee "tim" "jone" 10  777777777)) #f)

(define (paid-enough? e)
  (> (employee-wage e)MIN-WAGE))

;--Exercise 10


;re-assign: PHDStudent Grant-> PHDStudent (with a new grant)
(define (re-assign p gID)
  (make-phd (phd-name p) gID (phd-pay-rate p)))

(check-expect (re-assign phd1 "3-AB4")(make-phd (phd-name phd1)"3-AB4"(phd-pay-rate phd1)))

;--Exercise 11

;distance0: 3d-> Number
(define (distance0 3di)
  (sqrt (+ (sqr (3d-x 3di))(sqr (3d-y 3di))(sqr (3d-z 3di)))))

(check-expect (distance0 (make-3d 4 5 2)) 6.7)

















        
