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
         "services.rkt")


;;; Dispatches
(define-values (dispatcher url)
  (dispatch-rules
   [("api" "login" "account" ) ;登录接口
    #:method (or "post" "options")
    web-login]
   
   [("api" "register") ;注册接口
    #:method (or "post" "options")
    web-register]

   [("api" "auth") ;用户接口验证
    #:method (or "post" "options")
    web-auth]
   
   [("api" "myhome") ;用户我的主页接口
    #:method (or "post" "options")
    (lambda (req )
      (let* ([header (request-headers req) ]
             [userToken (cdr (assoc 'auth header)) ]
             [ad (myhome userToken)])
        
        (if ad
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'data ad ))
            (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "验证错误")))))]
   [("api" "loginlogs") ;获取日志接口
    #:method (or "get" "options")
    web-logs]
   
   [("api" "monchangelogs") 
    #:method (or "get" "options")
    web-logs]

     [("api" "operationlog") 
    #:method (or "get" "options")
    web-logs]

   
;   [("api" "systemlogs" (integer-arg))
 ;   #:method ("get" "options")
  ;  (lambda (req id)
   ;  (let* ([header(request-headers req)]
    ;         [userToken (cdr (assoc 'auth header))])
    
      

   
   [("api" "allocation") ;基础配置接口
    #:method (or "post" "options")
    (lambda (req )
      (let*  ([header (extract-binding/single req)]
              [userToken (cdr (assoc 'auth header))]
              [userId (cdr (assoc 'id header))] 
              [ad (if userId (get-allocation userId userToken) #f)])
        (if ad
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'data ad ) 1)
            (response/cors/options/400))))]))








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



  