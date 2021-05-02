#lang racket/base

(require 
  web-server/http
  json)

(provide response/cors/jsexpr
         response/cors/options/OK
         response/cors/template)

(define (response/cors/jsexpr jsexpr)
  (response/jsexpr
   jsexpr
   #:headers (list (header #"Access-Control-Allow-Origin" #"*")
                   (header #"Access-Control-Allow-Headers" #"*"))))

(define (response/cors/template t)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (header #"Access-Control-Allow-Origin" #"*"))
   (list (string->bytes/utf-8 t))))

(define (response/cors/options/OK)
  (response/full
   200 #"Okay"
   (current-seconds) #"application/json"
   (list (header #"Access-Control-Allow-Origin" #"*")
         (header #"Access-Control-Allow-Headers" #"*"))
   (list (jsexpr->bytes (hash 'status "ok")))))