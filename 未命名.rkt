#lang racket
(define a (list '(name . "张世涛") '(account ."17051006219") '(password . "safssf")  '(ip . "255.255.255.255")))
;(assoc  'account 
     ;   (list '(name "张世涛") '(account "17051006219") '(password "safssf")  '(ip "255.255.255.255")))

;(filter (assoc 'ip  a ) a)


;(cons 'A (+ 36 4))


(if #f 'a (values (+ 2 3) 's))

(define user-id (table-query-col "user" "id" (cdr (assoc 'account aa)) "account" ))
(define pair-list (list '(name ."张世涛") '(account ."17051006688") '(password . "safssf")  '(ip ."255.255.255.255")))
(table-insert-one "loginLog" (list  ;添加登录日志表
                                  (cons 'userId  user-id) (assoc 'name pair-list ) (assoc 'account pair-list ) (cons 'ipLog (cdr(assoc 'ip pair-list ))) (cons 'userType user-id)))
(table-insert-one "monManage" (list  ;添加个人账目表
                                   (cons 'userId  user-id) (assoc 'name pair-list ) (assoc 'account pair-list ) (cons 'userType (table-query-col "user" "userType" user-id))))
(userToken user-id));更新秘钥