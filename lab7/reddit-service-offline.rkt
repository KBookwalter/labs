#lang racket

;; this module provides functions and structures for extracting links from http://reddit.com

;; ---------------------------------------------------------------------------------------------------
;; interface

(provide
 (struct-out post)
 ;; Post = ...
 
 (struct-out comment)
 ;; Comment = ...
 
 get-posts
 
 )

;; ---------------------------------------------------------------------------------------------------
;; implementation: public 


; Load the internal libraries
(require net/url)
(require srfi/19)
(require srfi/6)
(require racket/port)
(require json)
(require net/uri-codec)
(require htdp/error)
(require racket/file)
(require 2htdp/image)

; Define the structs
(define-struct post (ups downs created subreddit id title author text? nsfw? content permalink))

(define-struct comment (ups downs created subreddit id author body body-html replies))

; Define the functions for building structs from json
(define (json->post jdata)
  (make-post (hash-ref (hash-ref jdata 'data) 'ups)
             (hash-ref (hash-ref jdata 'data) 'downs)
             (hash-ref (hash-ref jdata 'data) 'created)
             (hash-ref (hash-ref jdata 'data) 'subreddit)
             (hash-ref (hash-ref jdata 'data) 'id)
             (hash-ref (hash-ref jdata 'data) 'title)
             (hash-ref (hash-ref jdata 'data) 'author)
             (hash-ref (hash-ref jdata 'data) 'is_self)
             (hash-ref (hash-ref jdata 'data) 'over_18)
             (if (hash-ref (hash-ref jdata 'data) 'is_self)
                 (hash-ref (hash-ref jdata 'data) 'selftext)
                 (hash-ref (hash-ref jdata 'data) 'url))
             (hash-ref (hash-ref jdata 'data) 'permalink)))

(define (json->comment jdata)
  (make-comment (hash-ref (hash-ref jdata 'data) 'ups)
                (hash-ref (hash-ref jdata 'data) 'downs)
                (hash-ref (hash-ref jdata 'data) 'created)
                (hash-ref (hash-ref jdata 'data) 'subreddit)
                (hash-ref (hash-ref jdata 'data) 'id)
                (hash-ref (hash-ref jdata 'data) 'author)
                (hash-ref (hash-ref jdata 'data) 'body)
                (hash-ref (hash-ref jdata 'data) 'body_html)
                (if (equal? "" (hash-ref (hash-ref jdata 'data) 'replies))
                    empty
                    (map json->comment (drop-right (hash-ref (hash-ref (hash-ref (hash-ref jdata 'data) 'replies) 'data) 'children) 1)))))

(define (make-null-post)
  (make-post 0 0 0 "" 0 "" "" #t #f "" ""))

; Handle connections
(define CONNECTION false)
(define (disconnect-reddit-service)
  (set! CONNECTION false))
(define (connect-reddit-service)
  (set! CONNECTION true))

; Converts a raw binary string into a racket image
(define (image/raw raw)
  (local [(define img (make-temporary-file))
          (define o (open-output-file img 
                                      #:mode 'binary 
                                      #:exists 'truncate))]
    (begin (write-bytes (string->bytes/latin-1 raw) o)
           (close-output-port o)
           (local [(define return-image (bitmap/file img))]
             (begin (delete-file img)
                    return-image)))))

; Build Client Store
(define CLIENT_STORE (read-json (open-input-file "cache.json")))

; Generic helper functions
(define (boolean->string a-boolean)
  (if a-boolean
      "true"
      "false"))
(define (string->boolean a-string)
  (string=? a-string "true"))
(define (key-value pair)
  (string-append (symbol->string (car pair)) "=" (cdr pair)))
(define (convert-post-args data)
  (string->bytes/utf-8 (alist->form-urlencoded data)))
(define (convert-get-args url data)
  (string-append url "?" (string-join (map key-value data) "&")))
(define (post->json url full-data index-data)
  (if CONNECTION
      (port->string (post-pure-port (string->url url) (convert-post-args full-data)))
      (hash-ref CLIENT_STORE url "")))
(define (get->json url full-data index-data)
  (if CONNECTION
      (port->string (get-pure-port (string->url (convert-get-args url full-data))))
      (hash-ref CLIENT_STORE (string->symbol url) "")))

; Check if the sort-mode is a valid option
(define (valid-sorting-mode? sort-mode)
  (and (string? sort-mode) 
       (or (string-ci=? sort-mode "new")
           (string-ci=? sort-mode "top")
           (string-ci=? sort-mode "hot"))))

; Convert an individual image to a url
(define (bitmap/reddit-url url)
  (if CONNECTION
      (bitmap/url url)
      (image/raw (hash-ref CLIENT_STORE (string->symbol url)))))

(define (process-post post)
  (make-post (post-ups post)
             (post-downs post)
             (post-created post)
             (post-subreddit post)
             (post-id post)
             (post-title post)
             (post-author post)
             (post-text? post)
             (post-nsfw? post)
             (post-content post)
             ;(if (post-text? post) 
             ;    (post-content post)
             ;    (bitmap/reddit-url (post-content post)))
             (post-permalink post)))

; Define the services, and their helpers
(define (get-posts subreddit sort-mode)
  (check-arg 'get-posts (string? subreddit) 'string 1 subreddit)
  (check-arg 'get-posts (valid-sorting-mode? sort-mode) 'string 2 sort-mode)
  (local [(define post-data (get-posts/json (string-downcase subreddit) (string-downcase sort-mode)))]
    (cond [(eof-object? post-data) empty]
          [(and (string? post-data)
                (string=? "" post-data) "get-posts: the given subreddit is not available offline.")]
          [(and (string? post-data)
                (string-ci=? (substring post-data 0 9) "<!doctype"))
           (error "get-posts: temporary data error, try again")]
          [(or (number? post-data) 
               (and (hash? post-data) 
                    (hash-has-key? post-data 'error)))
           (error "get-posts: unable to find the subreddit")]
          [else (map (lambda (data) (process-post (json->post data)))
                     (string->jsexpr (hash-ref (hash-ref (get-posts/json subreddit sort-mode) 
                                         'data) 
                               'children)))])))

(define (get-posts/json subreddit sort-mode)
  (get-posts/string subreddit sort-mode))

(define (get-posts/string subreddit sort-mode)
  (get->json (string-append "http://www.reddit.com/r/" subreddit "/" sort-mode ".json") 
             (list)
             (list (cons 'sort_mode sort-mode) (cons 'subreddit subreddit))))

(define (get-comments id sort-mode)
  (local [(define DATA (get-comments/json id sort-mode))]
    (if (and (hash? DATA) (hash-has-key? DATA 'error))
        empty
        (map json->comment 
             (drop-right (hash-ref (hash-ref (second DATA) 'data) 'children) 1)))
    ))

(define (get-comments/json id sort-mode)
  (string->jsexpr (get-comments/string id sort-mode)))

(define (get-comments/string id sort-mode)
  (get->json (string-append "http://www.reddit.com/r/all/comments/" id "/" sort-mode ".json") 
             (list) 
             (list (cons 'id id) (cons 'sort_mode sort-mode))))


(define (get/string string-url)
  (bytes->string/latin-1 (port->bytes (get-pure-port (string->url string-url)))))
(define (post->key subreddit sort-mode)
  (string-append "http://www.reddit.com/r/" subreddit "/" sort-mode ".json"))
(define (filter-nsfw posts)
  (local [(define data (string->jsexpr posts))]
    (filter (lambda (post)
              (not (hash-ref (hash-ref post 'data) 'over_18)))
            (lookup-post data)))
  )

(define (lookup-post cache)
  (hash-ref (hash-ref cache 'data) 'children))

(define (generate-cache inputs limit)
  (local
    [(define new-cache (make-hash))]
    (map (lambda (input)
           (local [(define subreddit (first input))
                   (define sort-mode (second input))
                   (define posts (take (filter-nsfw (get-posts/string subreddit sort-mode)) limit))]
             (begin
               (hash-set! new-cache 
                          (string->symbol (post->key subreddit sort-mode))
                          (jsexpr->string posts))
               (map (lambda (post)
                      (if (hash-ref (hash-ref post 'data) 'is_self)
                          #f
                          (hash-set! new-cache
                                     (string->symbol (hash-ref (hash-ref post 'data) 'url))
                                     (get/string (hash-ref (hash-ref post 'data) 'url)))))
                    posts)
               )))
         inputs)
    new-cache))

(define (store-cache filename inputs limit)
  (begin 
    (define out (open-output-file filename #:mode 'binary #:exists 'replace))
    (write-json (generate-cache inputs limit)
                out
                #:encode 'all)
    (close-output-port out)))