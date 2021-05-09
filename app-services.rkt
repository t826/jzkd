#lang racket/base
(require racket/trace)
(require web-server/servlet-env
         web-server/dispatch
         web-server/configuration/responders
         web-server/http
         web-server/http/request-structs
         web-server/http/xexpr
         web-server/http/json
         web-server/http/bindings
         json
         net/url-structs
         racket/port
         "response-cors.rkt"
         "select-db.rkt"
         "user.rkt"
         "home.rkt")

(provide (all-defined-out))


;用户我的主页
(define (web-myhome req )
      (let* ([header (request-headers req) ]
             [userToken (cdr (assoc 'auth header)) ]
             [ad (myhome userToken)])
        
        (if ad
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'data ad ))
            (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "验证错误")))))
