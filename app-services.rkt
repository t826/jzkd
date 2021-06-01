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
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (let* ([header (request-headers req)]
             [userToken (cdr (assoc 'auth header)) ]
             [ad (myhome userToken)])
        (if ad
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'data ad ))
            (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "验证错误"))))))

;个人主页填写邀请
(define (web-set-invite-id req)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
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
                                          'msg "该id已存在上下级关联"))))))
         
           

;新闻模块
(define (web-get-news req)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
  (let* ([bindings (request-bindings req)]
         [class-naem (extract-binding/single 'class-naem bindings)]
         [time (extract-binding/single 'time bindings)]
         [page (extract-binding/single 'page bindings)]
         [page-number (extract-binding/single 'page-number bindings)]
         [ad (get-new class-naem time  (string->number page) (string->number page-number))])
    (if ad  (response/cors/jsexpr ad)
        (response/cors/jsexpr (hasheq 'status "error"
                                      'msg "验证错误"))))))
;修改个人信息
(define (web-update-user-msg req )
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
  (let* ([header (request-headers req)]
         [userToken (cdr (assoc 'auth header))]
         [data (bytes->jsexpr(request-post-data/raw  req))]
         [name (hash-ref data 'name #f)]
         [account (hash-ref data 'account #f)]
         [password (hash-ref data 'password #f)]
         [old-password (hash-ref data 'old-password #f) ]
         [pair-lst (list (cons 'name name ) (cons 'account  account ) (cons 'password password ))])
    (define (m:hash-remove-all#f ht)
      (define mht (hash-copy ht))
      (hash-for-each
       mht
       (lambda (k v)
         (when (equal? v #f)
           (hash-remove! mht k))))
      mht)
    (define  new-lst (hash->list (m:hash-remove-all#f (make-hasheq pair-lst))))
    
    (if (and (not (null? new-lst)) (update-user-msg userToken  old-password new-lst ))
        
        (response/cors/jsexpr (hasheq 'status "ok"))
        (response/cors/jsexpr (hasheq 'status "error"
                                      'msg "密码或手机号错误"))))))
        

;获取下级id
(define (web-get-all-level req)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
  (let* ([header (request-headers req)]
         [userToken (cdr (assoc 'auth header))]
         [userId (table-query-col  "user" "id"  userToken "userToken")])
        ;会员好友 返回hasheq
    (get-xiaji_id userId ))))


;佣金转余额提现
(define (web-commission-to-blanWithdraw req)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
  (let* ([header (request-headers req)]
         [userToken (cdr (assoc 'auth header))]
         [data (bytes->jsexpr(request-post-data/raw  req))]
         [commission-to-blanWithdraw  (hash-ref data 'commission-to-blanWithdraw #f)]
         [commission (table-query-col "user" "userCommission" userToken "userToken")]
         [userId (table-query-col "user" "id" userToken "userToken") ])
    (if  (and  commission commission-to-blanWithdraw (number? commission-to-blanWithdraw)  (>= commission  commission-to-blanWithdraw))
         (begin
           (commission->blanWithdraw  commission-to-blanWithdraw userId )
            (response/cors/jsexpr (hasheq 'status "ok")))
        (response/cors/jsexpr (hasheq 'status "error"
                                      'msg "佣金余额不足"))))))
;发起提现
(define (web-post-waitWithdraw req)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
  (let* ([header (request-headers req)]
         [userToken (cdr (assoc 'auth header))]
         [data (bytes->jsexpr(request-post-data/raw  req))]
         [Amount  (hash-ref data 'post-waitWithdraw #f)]
         [userId (table-query-col  "user" "id"  userToken "userToken")])
    (if (and userId Amount (number?  Amount))
        (begin 
          (post-waitWithdraw Amount userId)
             (response/cors/jsexpr (hasheq 'status "ok")))
        (response/cors/jsexpr (hasheq 'status "error"
                                      'msg "账户余额不足"))))))
         
  












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

