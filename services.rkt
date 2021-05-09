#lang racket/base
(require racket/trace)
(require web-server/servlet-env
         web-server/dispatch
         web-server/configuration/responders
         web-server/http
         web-server/http/request-structs
         web-server/http/xexpr
         web-server/http/json
         web-server/http/bindings
         json
         net/url-structs
         racket/port
         "response-cors.rkt"
         "select-db.rkt"
         "user.rkt"
         "home.rkt")

(provide (all-defined-out))

;; 登陆
(define (web-login req)
      (if (equal? #"OPTIONS" (request-method req))
          (response/cors/options/OK)
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
                                              'msg "账号或密码错误"))))))
;; 注册
(define (web-register req)
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
                                          'msg "账号已存在")))))

;; 系统的三个日志表的getList
(define (web-logs req)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (let* ([binding (request-bindings req)]
             [header (request-headers req) ]           
             [userToken (cdr (assoc 'auth header))]
             [uri (request-uri req)]
             [url-path (list-ref (url-path uri) 1)]
             [des (path/param-path url-path)]  ;; des : loginlogs || monchangelogs ...
             [table-name (cond [(equal? des "loginlogs" ) "loginLog"]
                               [(equal? des "monchangelogs" ) "monChangeLog"]
                               [else "operationLog"])]) 
        (define-values (start end)
          ((λ(bingding) (values (string->number (extract-binding/single '_start binding))
                                (string->number  (extract-binding/single '_end binding)))) binding))
        (display table-name)
  (define ad (get-log table-name  userToken start end))
  (if ad
      (response/cors/jsexpr ad (get-numbers-col table-name ))
      (response/cors/jsexpr (hasheq 'status "error"
                                    'msg "验证错误"))))))
  

;; 验证
(define (web-auth req )
      (let* ([header (request-headers req)]
             [userToken (cdr (assoc 'auth header)) ]
             [ad (check-user userToken)])
        
        (if ad
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'auth ad ))
            (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "验证错误")))))

(trace web-logs)