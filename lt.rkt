#lang racket
(require json)
(require net/url)

(define base-addr "http://www.librarything.com/api_getdata.php")
(define base-url (string->url base-addr))

(define userid "welisc")
(define key 3366634915)
(define max 10)
(define query (format "?userid=~a&key=~a&responseType=json&max=~a" userid key max))

(define query-url (combine-url/relative base-url query))
(define js (read-json (get-pure-port query-url)))

;(display js)

(define books (hash-ref js 'books))
(define book-count (length (hash-keys books)))
;(display books)
(printf "~a books~n" book-count)

(define (print-book id book-data)
  (printf "~a " id)
  (printf "~a " (hash-ref book-data 'title))
  (printf " / ~a " (hash-ref book-data 'author_fl))
  (printf "~n"))


(hash-for-each books print-book)