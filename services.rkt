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
         "checkCode.rkt"
         "tools2.rkt"
         "mdb.rkt")

(provide (all-defined-out))

;; 主页
(define (web-index req)
  (response/cors/template (include-template "dist/index.html")))

;;发送短信接口
(define (web-sendcode req)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (let* ([pdata (request-post-data/raw req)]
             [jdata (with-input-from-bytes pdata (λ () (read-json)))]
             [account (hash-ref jdata 'account)])
        (if (sendcode account)
             (response/cors/jsexpr (hasheq 'status "ok"))
             (response/cors/jsexpr (hasheq 'error "error"))))))

;; 登陆
(define (web-login req)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (let* ([ip (request-host-ip req)]
             [pdata (request-post-data/raw req)]
             [jdata (with-input-from-bytes pdata (λ () (read-json)))]
             [account (hash-ref jdata 'account)]
             [password (hash-ref jdata 'password)]
             [checkCode (hash-ref jdata 'checkCode)]
             [user (login (checkcode account checkCode) (list (cons 'account  account ) (cons 'password  password) (cons 'ipLog ip)))])
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
         [checkCode (hash-ref jdata 'checkCode)]
         [ad (addUser  (checkcode account checkCode)
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

;用户表接口
;(define (we-users 

   


;; /api/users
(define (web-users req)
  (cond
    [(equal? #"OPTIONS" (request-method req))
     (response/cors/options/OK)]
    [else
     (define bindings (request-bindings req))
     (define all-cols (get-all-cols "user"))
     (define pairs (check&get-bindings-list all-cols bindings))
     (define header (request-headers req))
     (define userToken (cdr (assoc 'auth header)))
     (if (not (user-check-permission userToken "rootUser"))
         (response/cors/options/401)
         (cond
           ;; getList
           [(and (exists-binding? '_start bindings)
                 (exists-binding? '_end bindings)
                 (exists-binding? '_order bindings)
                 (exists-binding? '_sort bindings))
            (cond
              [(null? pairs)
               (let* ([ids (xitong-many-in-page "user"
                                                (string->number (extract-binding/single '_start bindings))
                                                (string->number (extract-binding/single '_end bindings))
                                                (extract-binding/single '_sort bindings)
                                                (extract-binding/single '_order bindings))]
                      [hv (table-query-many "user" ids all-cols)])
                 (response/cors/jsexpr hv (get-numbers-col "user")))]
              [else
               ;; contain filters
               (let* ([ids (xitong-many-in-page "user"
                                                (string->number (extract-binding/single '_start bindings))
                                                (string->number (extract-binding/single '_end bindings))
                                                (extract-binding/single '_sort bindings)
                                                (extract-binding/single '_order bindings)
                                                #:filter-pairs pairs)]
                      [hv (table-query-many "user" ids all-cols)])
                 (response/cors/jsexpr hv (get-numbers-col "user")))])]
           [(assoc 'id pairs equal?)
            ;; getMany
            (let ([ids (map (lambda (v) (string->number v))
                            (extract-bindings 'id bindings))]
                  [cols (get-all-cols "user")])
              (response/cors/jsexpr (table-query-many "user" ids cols)))]
           [else
            ;; getManyReference
            (response/cors/options/OK)]))]))

;; getOne: /api/useres/{id}
(define (web-user req id)
  (cond
    [(equal? #"OPTIONS" (request-method req))
     (response/cors/options/OK)]
    [else
     (response/cors/jsexpr (table-query-row "user" id))]))

;; moidfy: /api/users/{id}
(define (web-modify-user req id)
  (cond
    [(equal? #"OPTIONS" (request-method req))
     (response/cors/options/OK)]
    [else
     (let* ([header (request-headers req)]
            [userToken (cdr (assoc 'auth header))])
       (if (not (user-check-permission userToken "rootUser"))
           (response/cors/options/401)
           (let* ([jdata (request-post-data/raw req)]
                  [jsexp (bytes->jsexpr jdata)])
             (if (table-update-one "user" id (hash->list jsexp))
                 (response/cors/jsexpr (table-query-row "user" id))
                 (response/cors/options/NotFound)))))]))

;; create: /api/users
(define (web-create-user req)
  (cond
    [(equal? #"OPTIONS" (request-method req))
     (response/cors/options/OK)]
    [else
     (let* ([jdata (request-post-data/raw req)]
            [jsexp (bytes->jsexpr jdata)]
            [pairs (remove '(createDate . "") (hash->list jsexp))])
       (table-insert-one "user" pairs)
       (define id (table-query-col "user" "id" (hash-ref jsexp 'account) "account"))
       (response/cors/jsexpr (table-query-one "user" id
                                              (map
                                               (lambda (s)
                                                 (string->symbol s))
                                               (get-mame-cols "user")))))]))

;; create
(define (web-delete-user req id)
  (cond
    [(equal? #"OPTIONS" (request-method req))
     (response/cors/options/OK)]
    [else
     (if (table-delete-one "user" id)
         (response/cors/options/OK)
         (response/cors/options/400))]))
