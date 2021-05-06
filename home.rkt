#lang racket/base
(require db
         "user.rkt" "select-db.rkt" "xitong-db.rkt")
(provide (all-defined-out))
;平台首页

;获取基础配置接口
(define (get-allocation userId userToken)
  (if (xitong-table "user" (list (cons 'id userId) (cons 'userToken userToken) (cons 'userType "rootUser"))) 
  (table-query-row "allocation" 1 ) #f)) 


;平台总用户数
(define (allUser)
  (query-value xitong "select count(userType) from user where userType='ordUser'"))
;今日注册人数
(define (todayRegi)
  (query-value xitong "select count(createDate) from user where date(createDate) = curdate()"))

;平台在线人数



