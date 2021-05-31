#lang racket/base

(require json
         net/http-easy
         net/url

         racket/port
         racket/format
         racket/string
   
       "../tools.rkt"
         )

(provide do-collect
         get-news-in-page)



;获取某个版块的内容
(define (do-collect url)
  (define html
    (call/input-url (string->url (string-append url (number->string(current-milliseconds))))
                    get-pure-port
                    (lambda (ip)
                      (port->string ip))))
  (define prejs (string-append "["(car (regexp-match #px"\\{.*\\}" html))"]"))
  (string->jsexpr prejs))


;; Get news in `n` page
(define (get-news-in-page content_url )
  (define js
    (response-body (get content_url #:stream? #t)))
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (list(cons 'content (car (regexp-match #rx"<div class=\"content\">.+?</div>" js))))))
