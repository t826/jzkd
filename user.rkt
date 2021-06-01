#lang racket/base 
(require db racket/date db/util/datetime  racket/format "xitong-db.rkt" "crypto.rkt" "select-db.rkt" "associate.rkt"  "tools.rkt" ) 
(provide (all-defined-out))
;验证秘钥
(date-display-format 'iso-8601)
(define (sql-null->#f k)
  (cond [(sql-null? k) '()]
        [(sql-timestamp? k)
         (date->string (sql-datetime->srfi-date k) #t)]
        [else k]))


; 用户管理模块

;添加用户接口
(define (addUser Boole invite-id pair-list)  ; (list '(name ."张世涛") '(account ."17051006218") '(password . "safssf")) 
  (define user-id (null?(table-query-col "user" "id" (cdr (assoc 'account pair-list)) "account" )))
  (if (and Boole (not user-id))
      (begin
        (table-insert-one "user" pair-list) ;去ip项 创建用户表
        (set!  user-id (table-query-col "user" "id" (cdr (assoc 'account pair-list)) "account" ))
        (when invite-id (inserte-invite user-id invite-id)) ;获取注册用户id后建立分级代理层
        (table-insert-one "monManage" (list (cons 'userId  user-id)))  ;添加个人账目表
        (userToken user-id)            ;更新秘钥
        (hash 'userToken (table-query-col "user" "userToken" user-id))) #f));返回秘钥 


;用户登录接口 返回hash 
(define (login boole lst)   ; '((account . "18666227042") (password . "sdwe4545") (ipLog . "255.255.255.255"))
  (let* ([ new-lst (remove (assoc 'ipLog lst) lst)]
         [ user  (xitong-table "user"  (list (assoc 'account lst)))]
         [ userture? (equal? (table-query-col "user" "account" (cdr(assoc 'account lst)) "account") (cdr(assoc 'account lst)))];验证账户
         [login-user (xitong-table "user"  new-lst)]
         [disabledUser (equal? (table-query-col "user" "userType" (cdr(assoc 'account lst)) "account") "disabledUser")])
    (cond [(not userture?) #f] ;添加登录日志表
          [disabledUser #f]
          [(not login-user) (add-longlog user lst "密码错误")]
          [(not boole) (add-longlog user lst "验证码错误")]
          [else (add-longlog user lst "登录成功")])
    (if (and boole userture? (not disabledUser))
        (begin    
          (userToken (vector-ref user 0)) ;更新秘钥
          (hash 'userToken (table-query-col "user" "userToken" (vector-ref user 0))
                'userType  (table-query-col "user" "userType" (vector-ref user 0))
                'adv_background_name (table-query-col "allocation" "adv_background_name" (list-ref (xitong-table "allocation") 0)))) ;返回秘钥
        #f)))

;用户验证接口
(define (check-user userToken)
  (let ([id (table-query-col  "user" "id" userToken "userToken" )])
    (if id
        (table-query-one "user"  id (list 'name 'id 'userToken  'avatar 'userType )) #f)))
    
  
;用户基本数据接口
(define (myhome userToken)
  (define userId (table-query-col "user" "id" userToken "userToken")) ;
  (if userId 
      (select-join-table "user" "monManage" '(name id avatar shangji_id) '(blanWithdraw  waitWithdraw sucWithdraw refWithdraw )"user.id=monManage.userId" (list userId)) #f))
       

;-----------------------------------------------------------------------
;;我的主页模块

(define (user-home userToken )
  (define userId (table-query-col "user" "id" userToken "userToken"))
  (if userId
      (let* ([my-msg (table-query-one "user" userId '(name id avatar shangji_id))] ;基本信息
             [today-income;今日收入 返回一个值
              (query-value xitong "select sum(rmb)from commission_log where userId=? and
  TIMESTAMPDIFF(DAY,date(create_time),now())=0"userId)]
             [forwarding-mon (sql-null->#f (query-rows xitong "select sum(rmb) from commission_log where userId=? and offer_id=?" userId userId ))] ;转发收入
             [get-allmon ;累计收入 返回一个值
              (apply + (map (λ(x) (cdr x)) 
                            (hash->list (table-query-one "monManage" #:id-name"userId" userId '(blanWithdraw waitWithdraw sucWithdraw refWithdraw)))))]
             [get-blanWithdraw (table-query-col  "monManage" "blanWithdraw"  userId  "userId")];账户余额 返回一个值  
             [team-comission (sql-null->#f (query-rows xitong "select sum(rmb) from commission_log where userId=? and offer_id!=?" userId userId ))]) ;团队贡献
        (values  my-msg  today-income  forwarding-mon get-allmon get-blanWithdraw team-comission)) #f))
  
;-----------


;;用户我的主页模块二

;发起提现
(define (post-waitWithdraw Amount userId)
  (let* ([blan (table-query-col  "monManage" "blanWithdraw"  userId "userId")]
         [wait (table-query-col  "monManage" "waitWithdraw"  userId "userId")])
    (cond [(and (> blan 0) (> Amount 0) (>= (- blan Amount)0) )
           (table-update-one "monManage" #:id-name "userId" userId (list (cons 'blanWithdraw (- blan Amount)) (cons 'waitWithdraw (+ wait Amount) ))) ;余额转待提现
           (table-insert-one "monChangeLog" (list (cons 'userId userId)
                                                  (cons 'changeProjet "blanWithdraw")
                                                  (cons 'changeContent  (- Amount)) (cons 'remark "余额转提现"))) ;添加余额日志                 
           (table-insert-one "monChangeLog" (list (cons 'userId userId)
                                                  (cons 'changeProjet "waitWithdraw")
                                                  (cons 'changeContent  Amount) (cons 'remark "余额转提现")))] ;添加待提现日志
          [else #f])))
  
;账目明细 返回#hasheq
(define (get-monchangelog userId)
  (table-query-many "monChangeLog" #:id-name"userId" (list userId) '(changeProjet changeContent changeTime)))
;累积贡献
(define (contribution userId)
  (sql-null->#f (query-rows xitong "select userId, rmb, Commission_content, sum(rmb) from commission_log where userId!=? and offer_id=?" userId userId )))
;客服
;常见问题
;;用户我的主页模块三
;我的上传
;商务合作
;投诉建议+
;我的收藏

;--------------------------------------------------------------------------------------------------------
;首页模块
    
;新闻输出
(define (get-new class-naem time  page page-number)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (let* ([lst '(imgSrc title content  resume)]
           [vs  
            (query-rows xitong (string-append "select "(query-eles lst)" from newsdata  where newstime<=? and catename =? order by  newstime  desc limit "
                                              (number->string (* page-number (- page 1)))","(number->string page-number)) time class-naem)])    
      (map (lambda (v)
             (vector->hash lst v))
           vs))))

;视频
;------------------------------------------------------------------------------------------
;;分销模块

 ;会员好友 返回hasheq 完成

    ;昨日今日收入
    ;今日收入 ；昨日收入
    (define (commission-time userId)
      (define yesterday(query-value xitong "select sum(rmb)from commission_log where userId=? and
  TIMESTAMPDIFF(DAY,date(create_time),now())=1"userId))
      (define today (query-value xitong "select sum(rmb)from commission_log where userId=? and
  TIMESTAMPDIFF(DAY,date(create_time),now())=0" userId))
      (values (cons 'yesterday_commission yesterday) (cons 'today_commission today)))
         


;--------------------------------------------
;添加登录日志功能
(define (add-longlog user lst login_msg)
  (table-insert-one "loginLog"  (list (cons 'userId (vector-ref user 0)) ;userid           
                                      (cons 'name (table-query-col "user" "name" (vector-ref user 0))) ;name
                                      (assoc 'account lst)
                                      (assoc 'ipLog lst)
                                      (cons 'userType (table-query-col "user" "userType" (vector-ref user 0))) 
                                      (cons 'login_msg login_msg ))))


;-----------------------------------------------
;修改个人信息功能
(define (update-user-msg userToken  keyword pair-lst  [re #t])
  (when (assoc 'account pair-lst) ;验证手机号格式
    (unless  (regexp-match-exact? #px"^1[3-9]\\d{9}$"  (cdr(assoc 'account pair-lst))) (set! re #f))) 
  (let ([userId (query-maybe-value xitong "select id from user where userToken=? and password=?"userToken keyword)])
    (if  (and re userId)  (begin
                            (table-insert-one "operationlog" (list (cons 'operatorId userId) (cons 'byId userId)
                           (cons 'operationInstruct (string-append "(table-update-one \"user\"" " " (~v  userId pair-lst)")"))));添加操作日志
                            (table-update-one "user" userId pair-lst)) #f))) ;更新基本信息
  ;--------------------------------------------------------------------
;佣金转余额接口
(define (commission->blanWithdraw number userId)
  (query-exec xitong "update user set userCommission = userCommission - ? where id = ?" number userId) ;更新佣金
  (query-exec xitong "update monmanage set blanWithdraw = blanWithdraw + ? where id = ?" number userId) ;更新余额
 (table-insert-one "monchangelog" (list (cons 'userId userId) (cons 'changeProjet "blanWithdraw") (cons 'remark "佣金转余额" ) (cons 'changeContent number)))#t) ;添加财务余额日志
  
  
  





































;（以下为用户类型userType）
;  删除用户   delUser
;  普通用户   ordUser
;  平台广告商 advUser
;  平台管理员 rootUser
;  封号状态   sealUser






