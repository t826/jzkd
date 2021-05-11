#lang racket/base
(require db
         "user.rkt" "xitong-db.rkt")
(provide (all-defined-out))


(provide (all-defined-out))

;系统管理模块



;添加登陆日志
(define (addLoginLog userId name account ipLog loginMachine userType)
  (query-exec xitong "insert into loginLog (userId,name,account,ipLog,loginMachine,userType) values(?,?,?,?,?,?)"userId name account ipLog loginMachine userType))
       ;(addLoginLog 2 "陈权业" "15077090552" "255.255.255.255" "魅族6s" "ordUser")

;查询登陆日志
  ;keyword 可以是账号 用户id 用户name
  ;date代表关键字内容
  ;userType 默认ordUser 即查询是普通用户,advUser是广告商
(define (selLogin  keyword date [userType "odrUser"] )
  (cond [(or (eq? keyword "name")
             (eq? keyword "account")
             (eq? keyword "userId"))
             (query-rows xitong (string-append "select userId, account, name from loginLog where " keyword "=? and " "userType=? ")date userType)]
        [else #f]))




;管理员操作日志   operation
;	操作员id  operatorId
;	操作员姓名operatorName
;	操作员账号operatorAccount
;       被操作账号 byAccount
;       被操作的用户id byId
;       被操作用户姓名 byName
;	操作日期operationDate
;	操作指令operationInstruct
;
;添加管理员操作日志
(define (addOpera operatorId operatorName operatorAccount operationInstruct byId byName byAccount)
  (query-exec xitong "insert into  operationLog (operatorId,operatorName,operatorAccount,operationInstruct,byId, byName,byAccount) values(?,?,?,?,?,?,?)"
              operatorId operatorName operatorAccount operationInstruct byId byName byAccount))
;                    (addOpera 1 "涛涛" "15077090552" "fsdfsdf sdfjks" 2 "陈权业" "15678954445")

;查询管理员操作日志
;  可通过管理员的id 姓名 账号查询
;  可通过被操作用户的id 姓名 账号查询
(define (selOperaLog  keyword date)
  (cond [(or (eq? keyword "operatorId")
             (eq? keyword "operatorAccount")
             (eq? keyword "operatorName")
             (eq? keyword "byId")
             (eq? keyword "byAccount")
             (eq? keyword "byName"))
             (query-rows xitong (string-append "select * from operationLog where " keyword "=?")date) ]
        [else #f]))


;用户账目变更日志；
;添加用户账目变更日志
;               userId,name ,account changeProjet,changeContent userType
(define (addMonChange userId name account changeProjet changeContent userType )
  (query-exec xitong "insert into monChangeLog (userId, name, account, changeProjet, changeContent, userType) values(?,?,?,?,?,?)"
              userId name account changeProjet changeContent userType))
;                    addMonChange 2 "zgags" "17051006219" "提现余额" 50.5 "odrUser")

;查询用户账户变更日志
;  可通过管理员的id 姓名 账号查询
;  查询广告商必须需要额外添加参数 "advUser" 
(define (selMonChangeLog keyword data [userType "ordUser"])
  (cond [(or (eq? keyword "userId")
             (eq? keyword "name")
             (eq? keyword "account"))
             (query-rows xitong (string-append "select userId, name, account, changeProjet, changeContent, userType from monChangeLog where "keyword "=? and userType=?")data userType)]))

  
