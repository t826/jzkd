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
         web-server/templates
         json
         net/url-structs
         racket/port
         "response-cors.rkt"
         "select-db.rkt"
         "user.rkt"
         "home.rkt"
         "checkCode.rkt")

(provide (all-defined-out))

;; 主页
(define (web-index req)
  (response/cors/template (include-template "dist/index.html")))

;; 登陆
(define (web-login req)
      (if (equal? #"OPTIONS" (request-method req))
          (response/cors/options/OK)
          (let* ([ip (request-host-ip req)]
                 [pdata (request-post-data/raw req)]
                 [jdata (with-input-from-bytes pdata (λ () (read-json)))]
                 [account (hash-ref jdata 'account)]
                 [password (hash-ref jdata 'password)]
                 [user (login  (list (cons 'account  account )
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
             [checkCode (hash-ref jdata 'password)]
             [ad (addUser  (web-checkcode)
                           (list (cons 'name  name)
                                (cons 'account  account )
                                (cons 'password  password )
                                (cons 'ipLog ip)))])
        (if ad  
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'data ad ))
            (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "账号已存在")))))

;; 系统的三个日志表的 loginlogs || monchangelogs || operationLog
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
  (define ad (get-log table-name  userToken start end))
  (if ad
      (response/cors/jsexpr ad (get-numbers-col table-name ))
      (response/cors/jsexpr (hasheq 'status "error"
                                    'msg "验证错误"))))))
  

;; 验证
(define (web-auth req )
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (let* ([header (request-headers req)]
             [userToken (cdr (assoc 'auth header)) ]
             [ad (check-user userToken)])
        
        (if ad
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'auth ad ))
            (response/cors/jsexpr (hasheq 'status "error"
                                          'msg "验证错误"))))))

;基础配置接口
(define (web-allocation req )
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (let*  ([header (request-headers req)]
              [userToken (cdr (assoc 'auth header))]
              [ad (if userToken (get-allocation userToken) #f)])
        (if ad
            (response/cors/jsexpr (hasheq 'status "ok"
                                          'data ad ) 1)
            (response/cors/options/400)))))
;基础配置修改接口
(define (web-update-allocation req id)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (let*  ([header (request-headers req)]
              [userToken (cdr (assoc 'auth header))]
              [meth (request-method req)]
              [pdata (request-post-data/raw req)]
              [pair-lst (hash->list (with-input-from-bytes pdata (λ () (read-json))))])
        (display pair-lst)
        (define ad (update-allocation userToken meth id pair-lst ))
        (if ad
            (response/cors/jsexpr (hasheq 'status "ok"))
                                 
            (response/cors/options/400)))))
            
;  [ad (if userToken (get-allocation userToken) #f)])
   



;(trace web-logs)

