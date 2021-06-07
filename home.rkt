#lang racket/base
(require db racket/format
         "user.rkt" "select-db.rkt" "xitong-db.rkt")
(provide (all-defined-out))
;平台首页

;获取基础配置接口
(define (get-allocation userToken)
  (define userId (table-query-col  "user" "id"  userToken "userToken"))
  (if (and  userId  (xitong-table "user" (list (cons 'id userId) (cons 'userToken userToken) (cons 'userType "rootUser"))))
  (table-query-row "allocation" 1 ) #f))

;基础配置修改接口
(define (update-allocation userToken meth id pair-lst )
  (if (equal? (table-query-col  "user" "userType"  userToken "userToken") "rootUser")
 (cond [(equal? meth #"GTE") (table-query-one "allocation" id (get-mame-cols "allocation"))]
        [(equal? meth #"PUT") (table-update-one "allocation"  id pair-lst)]
        [else #f]) #f))

;获取日志
(define (get-log table-name  userToken start end [sort-col "id"] [order "ASC"] )
  (if (equal? (table-query-col "user" "userType"  userToken "userToken") "rootUser");判断是否为管理员请求
  (begin
  (let ([id-lst (xitong-many-in-page table-name start end sort-col order )]
        [lst(map (λ (x) (string->symbol x))
            (get-mame-cols table-name))])
    (if (null? id-lst) '()
    (table-query-many table-name id-lst lst)))) #f))

;获取佣金日志
(define (get-commission_log ids )
(select-join-table "commission_log" "user" '(userId rmb Commission_content offer_id create_time )   '(name account ) "commission_log.userId=user.id" ids))

;-------------------------------------------------------------------------------------------------
;提现待审核所有id
(define (Audit-waitWithdraw  )
  (define id-lst (query-list xitong "select userId from monmanage where waitWithdraw != 0"))
  ;获取所有待审核的ids
  (if (null? id-lst ) (hasheq 'data null) 
       (select-join-table "monmanage" "user" '(userId blanWithdraw refWithdraw  sucWithdraw waitWithdraw) '( name account ) "user.id = monmanage.userId" id-lst )))
  
;获取单个id流水信息
(define (get-commission-id id start end)
(let* ([lst (query-list xitong  "select id from commission_log  where userId= ? " id)]
       [lengths  (length lst)]
       [all (table-query-many "commission_log" lst '(userId rmb Commission_content offer_id create_time) #:start start #:end end )]
       [sub-commission (query-maybe-value xitong  "select sum(rmb)  from commission_log  where userId= ? and Commission_content =?" id  "佣金->余额")]
       [sum-commission (query-maybe-value xitong  "select sum(rmb)  from commission_log  where userId= ? and Commission_content !=? " id  "佣金->余额")])
  (hasheq 'lengths lengths 'data all 'sub-commission sub-commission 'sum-commission sum-commission 'total (+ sum-commission sub-commission))))

;审核结果
(define (result-waitWithdraw userId Hash) ;'(#hasheq((confirm . #t) (id . 28)) #hasheq((confirm . #f ) (id . 29) (remark . "违规刷流量，此次不予提现"))                                                                        
  (map (λ(lst)
         (if (and (hash-has-key? lst 'id ) (hash-has-key? lst 'confirm ) (if (eq?(hash-ref lst 'confirm ) #f) (hash-ref lst 'remark #f) #t))
         (let* ([confirm (hash-ref lst 'confirm )]
                [id (hash-ref lst 'id )]
                [remark  (hash-ref lst 'remark #f )]
                [waitWithdraw (table-query-col  "monmanage" "waitWithdraw"  id "userId")]
                [refWithdraw (table-query-col  "monmanage" "refWithdraw"  id "userId")]
                [sucWithdraw (table-query-col  "monmanage" "sucWithdraw"  id "userId")])
           (unless (= waitWithdraw 0)
             (table-update-one "monmanage" #:id-name "userId" id      ;将待提现划转到拒绝提
                               (list (cons 'waitWithdraw 0)
                                     (if confirm (cons 'sucWithdraw (+ sucWithdraw waitWithdraw))  (cons 'refWithdraw  (+ refWithdraw waitWithdraw )))))
             ;添加财务日志
             (table-insert-one "monchangelog" (list (cons 'userId   id) (cons 'changeProjet "waitWithdraw" )
                                                    (cons 'changeContent (- waitWithdraw )) (if remark (cons 'remark remark ) (cons 'remark "提现->成功"))))
             (table-insert-one "monchangelog" (list (cons 'userId   id) (cons 'changeProjet "sucWithdraw" )
                                                    (cons 'changeContent waitWithdraw ) (if remark (cons 'remark remark ) (cons 'remark "提现->成功"))))
             ;添加操作日志
             (table-insert-one "operationlog"
                               (list
                                (cons 'operatorId   userId)
                                (cons 'byid id)
                                (cons 'operationInstruct
                                      (string-append  "(table-update-one \"monmanage\" #:id-name \"userId\" " 
                                                      (~v id (list (cons 'waitWithdraw 0)
         (if confirm (cons 'sucWithdraw (+ sucWithdraw waitWithdraw))  (cons 'refWithdraw  (+ refWithdraw waitWithdraw ))))))))))null)
         (hasheq 'id (hash-ref lst 'id #f) 'status "error")))
       Hash))
;------------------------------------------------------------------------------









;平台总用户数
(define (allUser)
  (get-numbers-col "user"))
;今日注册人数
(define (todayRegi)
  (query-value xitong "select count(createDate) from user where date(createDate) = curdate()"))

;平台在线人数



