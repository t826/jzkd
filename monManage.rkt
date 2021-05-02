#lang racket
(require db
       "xitong-db.rkt"  "user.rkt")
(provide (all-defined-out))
;财务管理模块


;  提现总览allWithdraw
;  等待提现 waitWithdraw
;  成功提现sucWithdraw
;  拒绝提现refWithdraw
;  广告ip数充值 advTopUp





;添加流水
;(define (monchangeLog userId userToken)
;account blanWithdraw waitWithdraw sucWithdraw refWithdraw advTopUp


#|(define (change-query-exec table-name name id p)
  (define v1 (query-maybe-value xitong (string-append "select " name " from " table-name " where userId = ?") id))
  (p)
  (define v2 (query-maybe-value xitong (string-append "select " name " from " table-name " where userId = ?") id))
  (query-exec xitong "insert into monChangeLog (userId, changeContent, changeProjet) values (?, ?, ?)" id (- v2 v1) "qaqa"))


(change-query-exec "monManage" "waitWithdraw" 28
                   (lambda ()
                     (query-exec xitong "update monManage set waitWithdraw = waitWithdraw + ?" 12)))
|#



;wo我的账户
(define (change-Money userId userToken projet number )
  (define x (query-maybe-row xitong  "select name,account,userType from user where id=? and userToken=?" userId userToken))
  (define before (query-maybe-row xitong "select blanWithdraw, waitWithdraw,sucWithdraw,refWithdraw,advTopUp from monManage where userId=?" userId ))

  


  
  
  (define  (now-change userId after) ;自动账目判断变化项  更新账目 添加日志 模块
   ; (display after ) (display before )
    (for  ([i after] [j before] [k (vector "blanWithdraw" "waitWithdraw" '"ucWithdraw" "refWithdraw" "advTopUp")]
       #:when (not (= (- i j) 0))
       [c (list (- i j))] )
      (define bbb (list k (- i j)))
      ;(display bbb)
      (query-exec xitong (string-append "update monManage set "(list-ref bbb 0) "="(list-ref bbb 0) "+"(number->string (list-ref bbb 1))" where userId =?") userId)  ;更新账户金额   
      (query-exec xitong "insert into monChangeLog (userId,name,account,changeProjet,changeContent,userType) values(?,?,?,?,?,?)"
                          userId (vector-ref x 0) (vector-ref x 1) (list-ref bbb 0) (list-ref bbb 1) (vector-ref x 2)))) ;添加日志
  
  (cond [(not x)  #f ] ;管理账目流向
        [(and (eq? projet  "blanWithdraw") (>= (+ (vector-ref before 0) number) 0))  ;判断余额是否够用
         (define after (vector (+ (vector-ref before 0) number) (vector-ref before 1) (vector-ref before 2) (vector-ref before 3) (vector-ref before 4)))
         (now-change userId after)]
        [(and (eq? projet  "waitWithdraw") (>= (- (vector-ref before 0) number) 0) (> number 0))
         (define after (vector (- (vector-ref before 0) number)  (+ (vector-ref before 1) number) (vector-ref before 2) (vector-ref before 3) (vector-ref before 4)))
         (now-change userId after) ]
        [(and (eq? projet  "sucWithdraw") (>= (- (vector-ref before 1) number) 0) (> number 0))
         (define after (vector (vector-ref before 0)  (- (vector-ref before 1) number) (+ (vector-ref before 2) number) (vector-ref before 3) (vector-ref before 4)))
         (now-change userId after)]
        [(and (eq? projet  "refWithdraw") (>= (- (vector-ref before 1) number) 0) (> number 0))
         (define after (vector (vector-ref before 0) (- (vector-ref before 1) number) (vector-ref before 2) (+ (vector-ref before 3) number) (vector-ref before 4)))
         (now-change userId after)]
        [(and (eq? projet  "ref->blan") (>= (- (vector-ref before 3) number) 0) (> number 0))
         (define after (vector (+ (vector-ref before 0) number) (vector-ref before 1) (vector-ref before 2) (- (vector-ref before 3) number) (vector-ref before 4)))
         (now-change userId after)]
        [(and (eq? projet  "advTopUP") (>= (+ (vector-ref before 4) number) 0) (> number 0))
         (define after (vector (vector-ref before 0) (vector-ref before 1)  (vector-ref before 2) (vector-ref before 3) (+ (vector-ref before 4) number)))
         (now-change userId after)]
        [else #f ]))





         
         




  ;(define after (query-row xitong "select blanWithdraw, waitWithdraw,sucWithdraw,refWithdraw,advTopUp from monManage where userId=?" userId ))

         
       ;  (query-exec xitong (string-append "update monManage set "projet "="projet "+? where userId =?") number userId)  ;添加项目 金额
       ;  (query-exec xitong "insert into monChangeLog (userId,name,account,changeProjet,changeContent,userType) values(?,?,?,?,?,?)"
                         ; userId (vector-ref x 0) (vector-ref x 1) projet number (vector-ref x 2)) #t ]  ;添加日志











        
;        [(eq? projet  "blanWithdraw")
 ;        (query-exec xitong (string-append "update monManage set "projet "="projet "+? where userId =?") number userId)  ;添加余额
  ;       (query-exec xitong "insert into monChangeLog (userId,name,account,changeProjet,changeContent,userType) values(?,?,?,?,?,?)"
   ;                       userId (vector-ref x 0) (vector-ref x 1) projet number (vector-ref x 2)) #t ]  ;添加日志


        
      #|  [(eq? projet  "waitWithdraw")
         (query-exec xitong (string-append "update monManage set "projet "="projet "+? where userId =?") number userId)
         (query-exec xitong "insert into monChangeLog (userId,name,account,changeProjet,changeContent,userType) values(?,?,?,?,?,?)"
                          userId (vector-ref x 0) (vector-ref x 1) projet number (vector-ref x 2))]))]



        
        [else (define before (query-row xitong "select blanWithdraw, waitWithdraw,sucWithdraw,refWithdraw,advTopUp from monManage where userId=?" userId ))
              (query-exec xitong (string-append "update monManage set "projet "="projet "+? where userId =?") number userId)
              (define after (query-row xitong "select blanWithdraw, waitWithdraw,sucWithdraw,refWithdraw,advTopUp from monManage where userId=?" userId ))
              (printf "~a ~a ~a" after before "\n")
              

              
              (query-exec xitong "insert into monChangeLog (userId,name,account,changeProjet,changeContent,userType) values(?,?,?,?,?,?)"
                          userId (vector-ref x 0) (vector-ref x 1) projet number (vector-ref x 2))]))
  
  ; (cond [(x
|#    ;          userId name account blanWithdraw waitWithdraw sucWithdraw refWithdraw advTopUp))

;流水查询 
(define (monWater x userId )
  (cond [(eq? x "allWithdraw")(query-rows xitong "select userId,name,account,blanWithdraw,waitWithdraw,sucWithdraw,refWithdraw from monManage where advTopUp <=0 ")]
        [(or (eq? x "waitWithdraw")
             (eq? x "sucWithdraw")
             (eq? x "refWithdraw")
             (eq? x "advTopUp"))
        (query-rows xitong (string-append"select userId,name,account,"x" from monManage where "x" > 0"))]
       [else #f]))
  
              
  
