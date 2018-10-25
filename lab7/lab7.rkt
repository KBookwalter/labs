;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
LAB 7
KEVIN BOOKWALTER
JONATHAN ZHANG
|#

(require "reddit-service-offline.rkt")
(require 2htdp/image)

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

#|
(check-expect (some-function=? test1 test2) #t)
|#

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

#|
(check-expect (mini1 (list 1 2 3)) 1)
(check-expect (maxi1 (list 1 2 3)) 3)
|#

(define LIST1 (list 3 7 6 2 9 8))
(define LIST2 (list 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
(define LIST3 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22))

#|
(check-expect (mini1 LIST1) 2)
(check-expect (mini1 LIST2) 1)
(check-expect (mini1 LIST3) 1)
(check-expect (maxi1 LIST1) 9)
(check-expect (maxi1 LIST2) 22)
(check-expect (maxi1 LIST3) 22)
|#

#|
Each number must be compared to every number that follows it, meaning that extreme has to be called
more for longer lists.  Everytime (rest l) is called, a new list is created, which takes a significant
amount of time.
|#

#|
PROBLEM 4
|#

;;(define-struct post
;;(ups downs created subreddit id title author! text? nsfw? content permalink))

;;make-post : Number Number Number String String String String Boolean Boolean String String --> Post

(define P1 (make-post 0 0 0 "r/TheMrKnauss" "id" "Knauss" "mediumrarebanana" #t #f
                      "Knauss is in the house word1"
                      "https://www.reddit.com/r/TheMrKnauss/comments/8f7xai/celebrate/"))
(define P2 (make-post 0 0 0 "r/jonzhang" "id" "Jon" "Jonathan Zhang" #t #t
                      "hi i'm jonathan zhang word2"
                      "https://www.reddit.com"))
(define P3 (make-post 0 0 0 "r/subreddit" "id" "title" "author" #f #f
                      "post content"
                      "https://www.reddit.com"))

(define LOP1 (list P1 P2 P3))
(define LOP2 empty)

;;banned-author?: String (listof Post) -> Boolean
;;consumes: String (listof Post)
;;produces:
;; -true of a the author of a post is the given string
;; -false otherwise

(define (banned-author? author lop)
  (local
    [(define (p? current-post)
       (string=? author (post-author current-post)))]
    (ormap p? lop)))

#|
(check-expect (banned-author? "Jonathan Zhang" LOP1) #t)
(check-expect (banned-author? "name" LOP2) #f)
(check-expect (banned-author? "mediumrarebanana" LOP1) #t)
(check-expect (banned-author? "name" LOP1) #f)
|#

(define LOS1 (list "word1" "word2" "word3"))

;;select-posts: (listof String) (listof Post) -> (listof Post)
(define (select-posts los lop)
  (local
    [;;f: String -> Boolean
     (define (f current-post)
       (ormap
        (λ (current-string) (string-contains? current-string (post-content current-post)))
        los))
     ]
    (filter f lop)))

#|
(check-expect (select-posts LOS1 LOP1) (list P1 P2))
(check-expect (select-posts '("nope" "still nope") LOP1) empty)
|#

;;too-wide: Number (listof Post) -> Boolean
;;consumes: A max title length and a list of posts
;;produces:
;; - True if the title of any post is longer than the max length
;; - False otherwise
(define (too-wide max-length lop)
  (local
    [
     (define (f p)
       (> (string-length (post-title p)) max-length))
     ]
    (ormap f lop)))

#|
(check-expect (too-wide 10 LOP1) #f)
(check-expect (too-wide 2 LOP1) #t)
(check-expect (too-wide 6 LOP1) #f)
|#

#|
too-wide returns #t if any post title is longer than max-length,
and #f otherwise.  ormap works in the same way - it returns the
first non-false result of f without needing to check every
other title length.  ormap only returns #f if f never produces
#t.
|#

