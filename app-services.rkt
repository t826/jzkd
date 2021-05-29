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
         "xitong-db.rkt"
         "associate.rkt")

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


(define (web-set-invite-id req)
  (let* ([binding (request-bindings req)]
         [header (request-headers req)]
         [userToken (cdr (assoc 'auth header))]
         [userId (table-query-col  "user" "id"  userToken "userToken") ]
         [shangji_id
          (table-query-col  "user" "id" (string->number(extract-binding/single 'shangji_id binding)))])
    (displayln userId)  (displayln shangji_id)  
       (if (and userId shangji_id (not (eq? userId  shangji_id)) (inserte-invite2 userId  shangji_id ))
           (response/cors/jsexpr (hasheq 'status "ok"))
           (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "该id已存在上下级关联")))))
         
           
;-----------------------------------------
;;分销模块
(define (web-get-all-level req)
  (let* ([header (request-headers req)]
         [userToken (cdr (assoc 'auth header))]
         [userId (table-query-col  "user" "id"  userToken "userToken")])
;贡献排行 (获取前三）


;昨日今日收入
     ;今日收入 ；昨日收入
(define (commission-time userId)
  (define yesterday(query-value xitong "select sum(rmb)from commission_log where userId=? and
  TIMESTAMPDIFF(DAY,date(create_time),now())=1"userId))
  (define today (query-value xitong "select sum(rmb)from commission_log where userId=? and
  TIMESTAMPDIFF(DAY,date(create_time),now())=0" userId))
  (values (cons 'yesterday_commission yesterday) (cons 'today_commission today)))
         
 ;会员好友 返回hasheq
(get-xiaji_id userId )))




;(define BS #"123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
;(define BS-LEN (bytes-length BS))
;(define (random-code-n n)
 ; (cond
 ;   [(= n 0) '()]
 ;   [else
 ;    (cons (bytes-ref BS (random BS-LEN))
  ;         (random-code-n (- n 1)))]))


;(time (void (random-code-n 100000)))
;cpu time: 1046 real time: 1018 gc time: 281
;(time (void (random-code-n 1000000)))
;cpu time: 671 real time: 682 gc time: 296

