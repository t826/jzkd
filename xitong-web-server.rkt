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
         json
         "response-cors.rkt"
         "user.rkt")


;;; Dispatches
(define-values (dispatcher url)
  (dispatch-rules
   [("api" "login" "account" ) ;登录接口
    #:method (list "post" "options")
    (lambda (req account password)
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
                                          'data user))
            (response/cors/jsexpr (hasheq 'status "error"
                                     'msg "信息错误")))))]
   
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
                                          'data ad ))
            (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "账号已存在")))))]))



#|   [("api" "currentUser" (string-arg) (string-arg)) ;个人主页接口
    #:method (list "POST" "OPTIONS")
    (lambda (req  id userToken)
      (let* ([home (myHome  id userToken)])
        (display home)
        (if home
            (response/cors/jsexpr )                  ;(hasheq (hash-set home (hash 'status "ok" ))))
            (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "非法秘钥")))))]
                               
                               
|#






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


(trace dispatcher)
