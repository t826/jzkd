#lang racket/base
 (require racket/port)
(require web-server/servlet-env
         web-server/dispatch
         web-server/configuration/responders
         web-server/http
         web-server/http/request-structs
         web-server/http/bindings
         web-server/http/xexpr
         web-server/http/json
         json
         "response-cors.rkt"
         "user.rkt" "home.rkt")


;;; Dispatches
(define-values (dispatcher url)
  (dispatch-rules
   [("api" "login" "account" ) ;登录接口
    #:method (or "post" "options")
    (lambda (req)
      (let* ([ip (request-host-ip req)]
             [pdata (request-post-data/raw req)]
             [jdata (with-input-from-bytes pdata (λ () (read-json)))]
             [account (hash-ref jdata 'account)]
             [password (hash-ref jdata 'password)]
             [user (login (list (cons 'account  account )
                                (cons 'password  password )
                                (cons 'ipLog ip)))])
        (if user
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'data user) 1)
            (response/cors/jsexpr (hasheq 'status "error"
                                     'msg "账号或密码错误")))))]
   
   [("api" "register") ;注册接口
    #:method (or "post" "options")
    (lambda (req)
      (let* ([ip (request-host-ip req)]
             [pdata (request-post-data/raw req)]
             [jdata (with-input-from-bytes pdata (λ () (read-json)))]
             [name (hash-ref jdata 'username)]
             [account (hash-ref jdata 'account)]
             [password (hash-ref jdata 'password)]
             [ad (addUser (list (cons 'name  name)
                                (cons 'account  account )
                                (cons 'password  password )
                                (cons 'ipLog ip)))])
        (if ad  
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'data ad ) 1)
            (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "账号已存在") 1))))]



   [("api" "auth") ;用户接口验证
    #:method (or "post" "options")
    (lambda (req )
      (let* ([header (request-headers req)]
             [userToken (cdr (assoc 'auth header)) ]
             [ad (check-user userToken)])
        
        (if ad
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'auth ad ) 1)
            (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "验证错误")1))))]

   
    [("api" "allocation") ;基础配置接口
    #:method (or "post" "options")
    (lambda (req )
     (let*  ([header (request-headers req)]
       [userToken (cdr (assoc 'auth header)) ]
       [bindings (request-bindings req)]
       [userId (if (exists-binding? 'id bindings)
           (extract-binding/single 'id bindings) #f)]
       [ad (if userId (get-allocation userId userToken) #f)])
           (if ad
               (response/cors/jsexpr (hasheq 'status "ok"
                                             'data ad ))
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



