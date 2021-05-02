#lang racket
(require racket/trace)
(require web-server/servlet-env
         web-server/dispatch
         web-server/configuration/responders
         web-server/http
         web-server/http/request-structs
         web-server/http/bindings
         web-server/http/xexpr
         web-server/http/json
         "response-cors.rkt"
         "user.rkt")


;;; Dispatches
(define-values (dispatcher url)
  (dispatch-rules
   [("api" "login" "account" (string-arg) (string-arg)) ;登录接口
    #:method (list "POST" "OPTIONS")
    (lambda (req account password)
      (let* ([ip (request-host-ip req)]
             [user (login account password ip)])
        (if user
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'id (vector-ref user 0)
                                          'userType (vector-ref user 1)
                                     'userToken (vector-ref user 2)))
            (response/cors/jsexpr (hasheq 'status "error"
                                     'msg "登录出错")))))]
   
   [("api" "register" (string-arg) (string-arg) (string-arg)) ;注册接口
    #:method (list "POST" "OPTIONS")
    (lambda (req  name account password)
      (let* ([ip (request-host-ip req)]
             [ad (addUser (list (cons 'name  name) (cons 'account  account ) (cons 'password  password ) (cons 'ipLog ip)))])
   
        (if ad 
            (response/cors/jsexpr (hasheq 'status "ok"
                                     'data ad ))
            (response/cors/jsexpr (hasheq 'status "error"
                                     'msg "账号已存在")))))]

   [("api" "currentUser" (string-arg) (string-arg)) ;个人主页接口
    #:method (list "POST" "OPTIONS")
   (lambda (req  id userToken)
     (let* ([home (myHome  id userToken)])
       (display home)
          (if home
              (response/cors/jsexpr )                  ;(hasheq (hash-set home (hash 'status "ok" ))))
              (response/cors/jsexpr (hasheq 'status "error"
                                     'msg "非法秘钥")))))]))
                               
                               









;; Setup The Servlet
(serve/servlet dispatcher
               #:command-line? #t
               #:listen-ip #f
               #:port 80
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:extra-files-paths (list (build-path "htdocs"))
               #:ssl? #f
               #:stateless? #f
               #:log-file "jzkd.log")


(trace dispatcher)