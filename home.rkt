#lang Racket
(require db
         "user.rkt")
(provide (all-defined-out))


;平台首页



;平台总用户数
(define (allUser)
  (query-value xitong "select count(userType) from user where userType='ordUser'"))
;今日注册人数
(define (todayRegi)
  (query-value xitong "select count(createDate) from user where date(createDate) = curdate()"))

;平台在线人数

