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
         db
         "response-cors.rkt"
         "select-db.rkt"
         "user.rkt"
         "home.rkt"
         "xitong-db.rkt")

(provide (all-defined-out))

;我的主页模块

;用户我基本数据
(define (web-mymsg req)
      (let* ([header (request-headers req)]
             [userToken (cdr (assoc 'auth header)) ]
             [ad (myhome userToken)])
        (if ad
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'data ad ))
            (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "验证错误")))))



;;分销模块

;贡献排行
;贡献排行
 ;今日收入 ；昨日收入
 ;会员好友



(define BS #"123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
(define BS-LEN (bytes-length BS))
(define (random-code-n n)
  (cond
    [(= n 0) '()]
    [else
     (cons (bytes-ref BS (random BS-LEN))
           (random-code-n (- n 1)))]))


;(time (void (random-code-n 100000)))
;cpu time: 1046 real time: 1018 gc time: 281
;(time (void (random-code-n 1000000)))
;cpu time: 671 real time: 682 gc time: 296

