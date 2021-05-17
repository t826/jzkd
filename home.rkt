#lang racket/base
(require db
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




  
;平台总用户数
(define (allUser)
  (get-numbers-col "user"))
;今日注册人数
(define (todayRegi)
  (query-value xitong "select count(createDate) from user where date(createDate) = curdate()"))

;平台在线人数



