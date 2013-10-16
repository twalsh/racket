#lang racket
(require json)
(require net/url)

(define userid "welisc")
(define key 3366634915)

(define base-addr "http://www.librarything.com/api_getdata.php")
(define base-url (string->url base-addr))

(define query (format "?userid=~a&key=~a" userid key))
(define query-url (combine-url/relative base-url query))

(define fetch (get-pure-port query-url))
(define s (substring (string-trim (read-line fetch) #px";LibraryThing.+") 20))
(define js (string->jsexpr s))

(display js)