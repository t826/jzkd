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

;;我的主页模块一

;今日收入 返回一个值
(define (today-income userId)
 (query-value xitong (string-append "select sum( changeContent ) from monChangeLog  where userId = ? and date(changeTime) = curdate()" ) userId))
;转发收入
;累计收入 返回一个值
(define (get-allmon userId)
  (apply + (map (λ(x) (cdr x)) 
  (hash->list (table-query-one "monManage" #:id-name"userId" userId '(blanWithdraw waitWithdraw sucWithdraw refWithdraw))))))
;账户余额 返回一个值
(define (get-blanWithdraw userId)
  (table-query-col  "monManage" "blanWithdraw"  userId  "userId"))
;团队贡献
;金币金额



;;我的主页模块二

;发起提现
(define (post-waitWithdraw Amount userToken)
  (let* ([userId (table-query-col  "user" "id"  userToken "userToken")]
        [blan (table-query-col  "monManage" "blanWithdraw"  userId "userId")]
        [wait (table-query-col  "monManage" "waitWithdraw"  userId "userId")])
(cond [(and (> blan 0) (> Amount 0) (>= (- blan Amount)0) )
       (table-update-one "monManage" #:id-name "userId" userId (list (cons 'blanWithdraw (- blan Amount)) (cons 'waitWithdraw  Amount))) ;余额转待提现
       (table-insert-one "monChangeLog" (list (cons 'userId userId) (cons 'changeProjet "blanWithdraw") (cons 'changeContent  (- Amount)))) ;更新余额日志                 
       (table-insert-one "monChangeLog" (list (cons 'userId userId) (cons 'changeProjet "waitWithdraw") (cons 'changeContent  Amount)))] ;更新待提现日志
      [else #f])))
       
  
;账目明细 返回#hasheq
(define (get-monchangelog userId)
  (table-query-many "monChangeLog" #:id-name"userId" (list userId) '(changeProjet changeContent changeTime)))

    