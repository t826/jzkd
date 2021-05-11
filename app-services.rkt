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

;用户我的主页数据
(define (web-myhome req )
      (let* ([header (request-headers req) ]
             [userToken (cdr (assoc 'auth header)) ]
             [ad (myhome userToken)])
        
        (if ad
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'data ad ))
            (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "验证错误")))))  


;今日收入 返回一个值
(define (today-income userId)
 (query-value xitong (string-append "select sum( changeContent ) from monChangeLog  where userId = ? and date(changeTime) = curdate()" ) userId))
;累计收入 返回一个值
(define (get-allmon userId)
  (apply + (map (λ(x) (cdr x)) 
  (hash->list (table-query-one "monManage" #:id-name"userId" userId '(blanWithdraw waitWithdraw sucWithdraw refWithdraw))))))
;账户余额 返回一个值
(define (get-blanWithdraw userId)
  (table-query-col  "monManage" "blanWithdraw"  userId  "userId"))


;发起提现
;(define (post-waitWithdraw Amount userId)
;  (let ([blan (table-query-col  "monManage" "blanWithdraw"  userId "userId")]
;        [wait (table-query-col  "monManage" "waitWithdraw"  userId "userId")]))
;(cond [(and (> blan 0) (> Amount 0)) f
 
  
;账目明细 返回#hasheq
(define (get-monchangelog userId)
  (table-query-many "monChangeLog" #:id-name"userId" (list userId) '(changeProjet changeContent changeTime)))

    