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
    web-register]

   [("api" "auth") ;用户接口验证
    #:method (or "post" "options")
    web-auth]
   
   [("api" "myhome") ;用户我的主页接口
    #:method (or "post" "options")
    web-myhome]
   
   [("api" "loginlogs") ;获取日志接口
    #:method (or "get" "options")
    web-logs]
   [("api" "monchangelogs") 
    #:method (or "get" "options")
    web-logs]
   [("api" "operationlogs") 
    #:method (or "get" "options")
    web-logs]

   [("api" "checkcode") ;发送短信验证接口
    #:method (or "pos" "options")
    web-sendcode]

   [("api" "allocations") ;基础配置信息接口
    #:method (or "get" "options")
    web-allocation]
   [("api" "allocations" (integer-arg)) ;基础配置修改接口
    #:method (or "get" "put" "options")
    web-update-allocation]


    ;; 获取用户表接口
    [("api" "users")
    #:method (or "get" "options")
    web-users]
    ;; 修改用户接口
    #;[("api" "users" (integer-arg))
    #:method (or "put" "options")
    web-change-users]


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



  