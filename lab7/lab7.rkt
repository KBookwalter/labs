;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
LAB 7
KEVIN BOOKWALTER
JONATHAN ZHANG
|#


;;map: (X -> Y) (listof X) -> (listof Y)
;;filter: (X -> Boolean) (listof X) -> (listof X)
;;foldr (X Y -> Y) Y (listof X) -> Y


#|
PROBLEM 1
|#

;;some-function=?: (Number -> Number) (Number -> Number) -> Boolean
; check for values 1.2, 3, -5.775
(define (some-function=? f1 f2)
  (and
   (= (f1 1.2) (f2 1.2))
   (= (f1 3) (f2 3))
   (= (f1 -5.775) (f2 -5.775))))

(define (test1 x)
  x)
(define (test2 x)
  x)

(check-expect (some-function=? test1 test2) #t)

#|
function=?: (Number -> Number) (Number -> Number) -> Boolean
could be defined, but it would have to check for all possible
conditions.
For example, if given two functions, f and g,
where f returns (+ x 1)
and g returns (/ (* (+ x 1) (+ x 2)) (+ x 2)),
function=? must return #f since (g -2) is undefined
|#

#|
PROBLEM 2
|#

;;extreme: (Number Number -> Boolean) NELON -> Number
(define (extreme compare-fn lon)
  (cond
    [(empty? (rest lon)) (first lon)]
    [else
     (cond
       [(compare-fn (first lon)
    (extreme compare-fn (rest lon)))
 (first lon)]
[else
 (extreme compare-fn (rest lon))])]))

;;mini1: NELON -> Number
(define (mini1 lon)
  (extreme < lon))

;;maxi1: NELON 0-> Number
(define (maxi1 lon)
  (extreme > lon))

(check-expect (mini1 (list 1 2 3)) 1)
(check-expect (maxi1 (list 1 2 3)) 3)

(define LIST1 (list 3 7 6 2 9 8))
(define LIST2 (list 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
(define LIST3 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22))

(check-expect (mini1 LIST1) 2)
(check-expect (mini1 LIST2) 1)
(check-expect (mini1 LIST3) 1)
(check-expect (maxi1 LIST1) 9)
(check-expect (maxi1 LIST2) 22)
(check-expect (maxi1 LIST3) 22)

#|
Each number must be compared to every number that follows it, meaning that extreme has to be called
more for longer lists.  Everytime (rest l) is called, a new list is created, which takes a significant
amount of time.
|#