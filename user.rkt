#lang racket/base 
(require db "xitong-db.rkt" "crypto.rkt" "select-db.rkt" ) 
(provide (all-defined-out))
;验证秘钥
(define (key-check id userToken ) ;返回 #t或#f
  (equal? (table-query-col  "user" "userToken" id) userToken))


; 用户管理模块

;添加用户接口
(define (addUser Boole pair-list) ; (list '(name ."张世涛"） '(account ."17051006218") '(password . "safssf")  '(ip ."255.255.255.255"))
 (define user-id (table-query-col "user" "id" (cdr (assoc 'account pair-list)) "account" ))
  (if (or (not Boole) user-id)  #f
      (begin
        (table-insert-one "user" (remove (assoc 'ipLog pair-list ) pair-list)) ;去ip项 创建用户表
        (set!  user-id (table-query-col "user" "id" (cdr (assoc 'account pair-list)) "account" ))
        (table-insert-one "loginLog" (list  ;添加登录日志表
                                      (cons 'userId  user-id) (assoc 'name pair-list ) (assoc 'account pair-list ) (cons 'ipLog (cdr(assoc 'ipLog pair-list ))) (cons 'userType (table-query-col "user" "userType" user-id))))
        (table-insert-one "monManage" (list  ;添加个人账目表
                                       (cons 'userId  user-id) (assoc 'name pair-list ) (assoc 'account pair-list ) (cons 'userType (table-query-col "user" "userType" user-id))))
        (userToken user-id);更新秘钥
      (hash 'userToken (table-query-col "user" "userToken" user-id)))));返回秘钥


;用户登录接口

(define (login boole lst)   ; '((account . "187654321") (password . "sdwe4545") (ipLog . "255.255.255.255"))
  (define new-lst (remove (assoc 'ipLog lst) lst))
    (define user (xitong-table "user" new-lst ))   ;验证账户
      (if (and boole user)
          (begin
           (table-insert-one "loginLog"  (list    ;添加登录日志表
                                           (cons 'userId (vector-ref user 0)) ;userid 
                                           (cons 'name (table-query-col "user" "name" (vector-ref user 0))) ;name
                                           (assoc 'account lst)
                                           (assoc 'ipLog lst)
                                           (cons 'userType (table-query-col "user" "userType" (vector-ref user 0)))))
            (userToken (vector-ref user 0)) ;更新秘钥
           (hash 'userToken (table-query-col "user" "userToken" (vector-ref user 0))
                 'userType  (table-query-col "user" "userType" (vector-ref user 0))
                 'adv_background_name (table-query-col "allocation" "adv_background_name" (list-ref (xitong-table "allocation") 0)))) ;返回秘钥
          #f))

;用户验证接口
(define (check-user userToken)
  (let ([id (table-query-col  "user" "id" userToken "userToken" )])
    (if id
    (table-query-one "user"  id (list 'name 'id 'userToken  'avatar 'userType )) #f)))
    
  
;用户我的主页接口
(define (myhome userToken)
  (define userId (table-query-col "user" "id" userToken "userToken")) ;
  (if userId 
      (select-join-table "user" "monManage" '(name account userType avatar) '(blanWithdraw  waitWithdraw sucWithdraw refWithdraw )"user.id=monManage.userId" (list userId)) #f))
       














    

;用户我的主页
(define (myHome id userToken )
  (if (key-check id userToken ) 
  (let*
      ([user (map (lambda (l1)
                    (if (sql-null? l1) #f l1))
                  (vector->list(query-row xitong"select id,account,name,myImg ,userCommission, userType,userToken from user where id =? and userToken =? "id userToken)))]           
       [monManage (vector->list (query-row xitong"select blanWithdraw,waitWithdraw,sucWithdraw,refWithdraw from monManage where userId =? "id ))]
       [monChangeLog (query-maybe-row xitong"select changeProjet, changeContent,changeTime from monChangeLog where userId =?"id)])    
       (define userdata (make-hash))
         (for ([i (list 'id  'account 'name 'myImg  'userCommission 'userType 'userToken)]
               [j user])
           (hash-set! userdata i j))
    userdata)
  #f))
  
   ; (printf "~a\n~a\n~a" user monManage monChangeLog)))



  ;(map lambda(x y) (hash  
;  (define allData (li3st user monManage monChangeLog))


;（以下为用户类型userType）
;  删除用户   delUser
;  普通用户   ordUser
;  平台广告商 advUser
;  平台管理员 rootUser
;  封号状态   sealUser

;管理员功能
;有条件查询用户
(define (select  col keyword)
  (query-rows xitong  (string-append "select * from user where " col "=?") keyword))





;用户账号 密码 昵称修改
;  密码例如：(updata "password"  1 1 "4567ijk")  ((updata "修改项"  权限id  修改的id "修改内容")
;  平台管理员可以修改任意项
(define (update userdate rootId  id  new-value)
  (if  (> (length(query-rows xitong "select * from user  where userType='rootUser' and id=?" rootId)) 0)
       (query-exec xitong
                   (string-append "UPDATE user SET " userdate " = ? where id = ?")new-value id)
       (cond [(= rootId id)
              (cond
                [(or (eq? userdate "name")
                     (eq? userdate "password")
                     (eq? userdate "account"))
                 (query-exec xitong(string-append "UPDATE user SET " userdate " = ? where id = ?")new-value id)]
                [else #f])]
             [else #f])))


      