#lang racket/base
(require web-server/servlet-env
         web-server/dispatch
         web-server/configuration/responders
         web-server/http
         web-server/http/request-structs
         web-server/http/xexpr
         web-server/http/json
         web-server/http/bindings
         json
         racket/trace
         "response-cors.rkt"
         "user.rkt"
         "home.rkt"
         "services.rkt"
         "app-services.rkt"
         "checkCode.rkt")

;;; Dispatches
(define-values (dispatcher url)
  (dispatch-rules
   [("") web-index] ;主页
   
   [("api" "login" "account" ) ;登录接口
    #:method (or "post" "options")
    web-login]
   
   [("api" "rigester") ;注册接口
    #:method (or "post" "options")
    web-rigester]

   [("api" "auth") ;用户接口验证
    #:method (or "post" "options")
    web-auth]
   
   [("api" "mymsg") ;用户基本信息接口
    #:method (or "post" "options")
    web-mymsg]
   
   [("api" "loginlogs") ;获取日志接口
    #:method (or "get" "options")
    web-logs]
   [("api" "monchangelogs") 
    #:method (or "get" "options")
    web-logs]
   [("api" "operationlogs") 
    #:method (or "get" "options")
    web-logs]
   
   [("api" "commission_logs") ;获取佣金日志接口
    #:method (or "get" "options")
    web-commission_log]

   [("api" "sendcode") ;发送短信接口
    #:method (or "post" "options")
   web-sendcode]

   [("api" "allocations") ;基础配置信息接口
    #:method (or "get" "options")
    web-allocation]
   [("api" "allocations" (integer-arg)) ;基础配置修改接口
    #:method (or "get" "put" "options")
    web-update-allocation]
   
   [("api" "monManage" ) ;财务管理
    #:method (or "get" "options")
    web-monManage]
   
     [("api" "allociate" ) ;分销树状图
    #:method (or "get" "options")
    web-associate]
      [("api" "allociate" (integer-arg)) ;分销树状图获取一个
    #:method (or "get" "options")
    web-associate-one]
    
   [("api" "set-invite-id" ) ;用户主页填写上级id
    #:method (or "get" "options")
    web-set-invite-id]
   
   [("api" "get-all-level" ) ;用户下级代理id
    #:method (or "get" "options")
    web-get-all-level]
   

   ;; 获取用户表接口
   [("api" "users")
    #:method (or "get" "options")
    web-users]
   ;; 修改用户接口
   [("api" "users" (integer-arg))
    #:method (or "get" "options")
    web-user]
   ;; 修改用户接口
   [("api" "users" (integer-arg))
    #:method (or "put" "options")
    web-modify-user]
   ;; 增加用户
   [("api" "users")
    #:method (or "post" "options")
    web-create-user]
   ;; 删除用户
   [("api" "users" (integer-arg))
    #:method (or "delete" "options")
    web-delete-user]


   ))








;; Setup The Servlet
(serve/servlet dispatcher
               #:command-line? #t
               
               #:listen-ip #f
               #:port 5000
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:extra-files-paths (list (build-path "htdocs"))
               #:ssl? #f
               #:stateless? #t
               #:log-file "jzkd-web.log")



