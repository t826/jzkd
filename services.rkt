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
        (cond [(not (regexp-match? #px"^1[3-9]\\d{9}$" account)) (response/cors/jsexpr (hasheq 'error "account?"))]
              [(sendcode account) (response/cors/jsexpr (hasheq 'status "ok"))]
              [else (response/cors/jsexpr (hasheq 'error "error"))]))))

;; 用户接口登陆
(define (web-login req)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (let* ([ip (request-client-ip req)]
             [pdata (request-post-data/raw req)]
             [jdata (with-input-from-bytes pdata (λ () (read-json)))]
             [account (hash-ref jdata 'account)]
             [password (hash-ref jdata 'password)]
             [checkCode (hash-ref jdata 'checkCode)]
             [boole (checkcode account checkCode)]
             ;[boole #t]
             [user (login checkCode (list (cons 'account  account ) (cons 'password  password) (cons 'ipLog ip)))])
        (cond [(not boole)(response/cors/jsexpr (hasheq 'status "error" 'msg "验证码与账号未适配"))]
              [user (response/cors/jsexpr (hasheq 'status "ok" 'data user))]
              [else (response/cors/jsexpr (hasheq 'status "error" 'msg "账号或密码错误"))]))))




;; 注册
(define (web-rigester req)
  (let* ([pdata (request-post-data/raw req)]
         [jdata (with-input-from-bytes pdata (λ () (read-json)))]
         [name (hash-ref jdata 'username)]
         [account (hash-ref jdata 'account)]
         [password (hash-ref jdata 'password)]
         [checkCode (hash-ref jdata 'checkCode)]
         [invite-id (hash-ref jdata 'invite-id #f)]
         [boole (checkcode account checkCode)]
         [ad (addUser boole invite-id (list (cons 'name  name) 
                                            (cons 'account  account )
                                            (cons 'password  password )))])
    (cond [(not boole)(response/cors/jsexpr (hasheq 'status "error" 'msg "验证码与账号未适配"))]
          [ad (response/cors/jsexpr (hasheq 'status "ok" 'data ad))]
          [else (response/cors/jsexpr (hasheq 'status "error" 'msg "账号或密码错误"))])))



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
            (response/cors/jsexpr (hasheq 'data ad 'total (get-numbers-col table-name) 'status "ok"))
        
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

;财务管理
(define (web-monManage req)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (let* ([bindings (request-bindings req)]
             [all-cols  (get-all-cols "monManage")]
             [pairs (check&get-bindings-list all-cols bindings)]
             [header (request-headers req)]
             [userToken (cdr (assoc 'auth header))])
        (if (not (user-check-permission userToken "rootUser"))
            (response/cors/options/401)
            (begin
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
                           [hv (select-join-table "user" "monmanage" '(id account name userType)  all-cols "user.id=monManage.userId" ids )])
                      ; [hv (table-query-many "monManage" ids all-cols)])
                      (response/cors/jsexpr (hasheq 'data hv 'total (get-numbers-col "user") 'status "ok")))]
                   [else
                    ;; contain filters
                    (let* ([ids (xitong-many-in-page "monManage"
                                                     (string->number (extract-binding/single '_start bindings))
                                                     (string->number (extract-binding/single '_end bindings))
                                                     (extract-binding/single '_sort bindings)
                                                     (extract-binding/single '_order bindings)
                                                     #:filter-pairs pairs)]
                           [hv (select-join-table "user" "monmanage" '(id account name userType)  all-cols "user.id=monManage.userId" ids )])
                      ; [hv (table-query-many "monManage" ids all-cols)])
                      (response/cors/jsexpr (hasheq 'data hv 'total (get-numbers-col "user") 'status "ok")))])]
                [(assoc 'id pairs equal?)
                 ;; getMany
                 (let* ([ids (map (lambda (v) (string->number v))
                                  (extract-bindings 'id bindings))]
                        [cols (get-all-cols "monManage")]
                        [hv (select-join-table "user" "monmanage" '(id account name userType)  all-cols "user.id=monManage.userId" ids )])
                   ; [hv (table-query-many "monManage" ids cols)])
                   (response/cors/jsexpr (hasheq 'data hv
                                                 'status "ok")))]
                [else
                 ;; getManyReference
                 (response/cors/options/OK)]))))))


;分销树状图
(define (web-associate req)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (let* ([bindings (request-bindings req)]
             [all-cols  (get-all-cols "associate")]
             [pairs (check&get-bindings-list all-cols bindings)]
             [header (request-headers req)]
             [userToken (cdr (assoc 'auth header))])
        (display userToken)
        (if (not (user-check-permission userToken "rootUser"))
            (response/cors/options/401)
            (begin                       
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
                           [hv (get-level-all ids )])
                      (response/cors/jsexpr (hasheq 'data hv 'total (get-numbers-col "user") 'status "ok")))]
                   [else
                    ;; contain filters
                    (let* ([ids (xitong-many-in-page "user"
                                                     (string->number (extract-binding/single '_start bindings))
                                                     (string->number (extract-binding/single '_end bindings))
                                                     (extract-binding/single '_sort bindings)
                                                     (extract-binding/single '_order bindings)
                                                     #:filter-pairs pairs)]
                           [hv (get-level-all ids )])
                      (response/cors/jsexpr (hasheq 'data hv 'total (length ids) 'status "ok")))])]
                [(assoc 'id pairs equal?)
                 ;; getMany
                 (let* ([ids (map (lambda (v) (string->number v))
                                  (extract-bindings 'id bindings))]
                        [cols (get-all-cols "user")]
                        [hv (get-level-all ids )])
                   (response/cors/jsexpr (hasheq 'data hv
                                                 'status "ok")))]
                [else
                 ;; getManyReference
                 (response/cors/options/OK)]))))))
;分销树状图获取一个id
(define (web-associate-one req id)
  (let* ([header (request-headers req)]
         [userToken (cdr (assoc 'auth header))])
    (cond
      [(not (user-check-permission userToken "rootUser"))
       (response/cors/options/401)]
      [(equal? #"OPTIONS" (request-method req))
       (response/cors/options/OK)]
      [else
       (response/cors/jsexpr (get-level-all  (list id)))])))
;余额提现审核接口1
(define (web-audit-waitWithdraw req )
  (let* ([headers (request-headers req)]
         [userToken (cdr (assoc 'auth headers))]
         [root-user  (equal? (table-query-col  "user" "userType"  userToken  "userToken") "rootUser")]
         [userId (table-query-col  "user" "id"  userToken  "userToken") ]
         [method (request-method req)]
         [bindings (request-bindings req)])
    (case method
      [(#"GET")
       (cond [(and (exists-binding? 'id bindings ) (exists-binding? 'start bindings ) (exists-binding? 'end bindings))
              (response/cors/jsexpr (get-commission-id (string->number(extract-binding/single 'id bindings))
                                 (string->number(extract-binding/single 'start bindings))
                                 (string->number(extract-binding/single 'end bindings))))]
             [else 
              (response/cors/jsexpr (Audit-waitWithdraw))])]
      [(#"POST")
       (define data (bytes->jsexpr(request-post-data/raw  req)))
       (displayln data )
       (define resul (filter hash? (result-waitWithdraw userId data)))
       (if (null? resul)
           (response/cors/jsexpr (hasheq 'status "ok"))
           (response/cors/jsexpr resul))]
      [(#"OPTIONS") (response/cors/options/OK)])))



;佣金日志
 (define (web-commission_log req)
  (if (equal? #"OPTIONS" (request-method req))
      (response/cors/options/OK)
      (let* ([bindings (request-bindings req)]
             [all-cols  (get-all-cols "commission_log")]
             [pairs (check&get-bindings-list all-cols bindings)]
             [header (request-headers req)]
             [userToken (cdr (assoc 'auth header))])
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
                  (let* ([ids (xitong-many-in-page "commission_log"
                                                   (string->number (extract-binding/single '_start bindings))
                                                   (string->number (extract-binding/single '_end bindings))
                                                   (extract-binding/single '_sort bindings)
                                                   (extract-binding/single '_order bindings))]
                         [hv (get-commission_log ids )])
                    (response/cors/jsexpr (hasheq 'data hv 'total (get-numbers-col "commission_log") 'status "ok")))]
                 [else
                  ;; contain filters
                  (let* ([ids (xitong-many-in-page "user"
                                                   (string->number (extract-binding/single '_start bindings))
                                                   (string->number (extract-binding/single '_end bindings))
                                                   (extract-binding/single '_sort bindings)
                                                   (extract-binding/single '_order bindings)
                                                   #:filter-pairs pairs)]
                         [hv (get-commission_log ids )])
                    (response/cors/jsexpr (hasheq 'data hv 'total (get-numbers-col "commission_log") 'status "ok")))])]
              [(assoc 'id pairs equal?)
               ;; getMany
               (let* ([ids (map (lambda (v) (string->number v))
                                (extract-bindings 'id bindings))]
                      [cols (get-all-cols "user")]
                      [hv (get-commission_log ids )])
                 (response/cors/jsexpr (hasheq 'data hv
                                               'status "ok")))]
              [else
               ;; getManyReference
               (response/cors/options/OK)])))))


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
                 (response/cors/jsexpr (hasheq 'data hv 'total (get-numbers-col "user") 'status "ok")))]
              [else
               ;; contain filters
               (let* ([ids (xitong-many-in-page "user"
                                                (string->number (extract-binding/single '_start bindings))
                                                (string->number (extract-binding/single '_end bindings))
                                                (extract-binding/single '_sort bindings)
                                                (extract-binding/single '_order bindings)
                                                #:filter-pairs pairs)]
                      [hv (table-query-many "user" ids all-cols)])
                 (response/cors/jsexpr (hasheq 'data hv 'total (get-numbers-col "user") 'status "ok")))])]
           [(assoc 'id pairs equal?)
            ;; getMany
            (let* ([ids (map (lambda (v) (string->number v))
                             (extract-bindings 'id bindings))]
                   [cols (get-all-cols "user")]
                   [hv (table-query-many "user" ids cols)])
              (response/cors/jsexpr (hasheq 'data hv
                                            'status "ok")))]
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

