#lang racket/base

(require 
  web-server/http
  json)

(provide (all-defined-out))


(define (response/cors/jsexpr jsexpr [total-count 1]) ;返回json及总页数  
  (response/jsexpr
   jsexpr
   #:headers (list (header #"Access-Control-Allow-Origin" #"*")
                   (header #"Access-Control-Allow-Headers" #"*")
                   (header #"Access-Control-Expose-Headers" #"X-Total-Count")
                   (header #"X-Total-Count"
                           (string->bytes/utf-8 (number->string total-count))))))


(define (response/cors/template t)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (header #"Access-Control-Allow-Origin" #"*"))
   (list (string->bytes/utf-8 t))))



(define (response/cors/options/400)
  (response/full
   400 #"Bad Request"
   (current-seconds) #"application/json"
   (list (header #"Access-Control-Allow-Origin" #"*")
         (header #"Access-Control-Allow-Headers" #"*")
         (header #"Access-Control-Allow-Methods" #"*"))
   (list (jsexpr->bytes (hash 'status "error"
                              'msg "找不到页面")))))


(define (response/cors/options/OK)
  (response/full
   200 #"Okay"
   (current-seconds) #"application/json"
   (list (header #"Access-Control-Allow-Origin" #"*")
         (header #"Access-Control-Allow-Headers" #"*")
         (header #"Access-Control-Allow-Methods" #"*"))
   (list (jsexpr->bytes (hash 'status "ok")))))



(define (response/cors/delete/OK)
  (response/full
   200 #"Okay"
   (current-seconds) #"application/json"
   (list (header #"Access-Control-Allow-Origin" #"*")
         (header #"Access-Control-Allow-Headers" #"*")
         (header #"Access-Control-Allow-Methods" #"DELETE"))
   (list (jsexpr->bytes (hash 'status "ok")))))


(define (response/cors/delete/400)
  (response/full
   400 #"Bad Request"
   (current-seconds) #"application/json"
   (list (header #"Access-Control-Allow-Origin" #"*")
         (header #"Access-Control-Allow-Headers" #"*")
         (header #"Access-Control-Allow-Methods" #"DELETE"))
   (list (jsexpr->bytes (hash 'status "ok")))))