#lang racket/base

(require db "select-db.rkt" "xitong-db.rkt" racket/trace  racket/match)
(provide (all-defined-out))
;----------------------------------------------------
;注册时人填写邀请  (此时注册人没有上下级代理关系）
(define (inserte-invite userId shangji_id [level 1] )
  (unless (or(eq? userId shangji_id) (not(eq? (table-query-col  "user" "shangji_id"  userId) -1))) ;查找useid是否已经存在上级id
    (table-update-one "user" userId (list (cons 'shangji_id shangji_id))) ;在user表填写上级id
    (table-insert-one "associate" (list (cons 'userId userId) (cons 'shangjiUserId shangji_id) (cons 'level level))) ;插入二级代理
    (build-class shangji_id) ;检查上级id是否获得团队称号
    (let loop ([shangji_id (table-query-col  "user" "shangji_id"  shangji_id)]
               [level level])
      (cond [(>= level 5) #t ]
            [(not (eq? shangji_id -1))
             (table-insert-one "associate" (list (cons 'userId userId) (cons 'shangjiUserId shangji_id) (cons 'level (+ level 1)))) ;插入多级代理
             (build-class shangji_id) ;检查上级id是否获得团队称号
             (loop (table-query-col  "user" "shangji_id"  shangji_id) (+ level 1))]
            [else #t]))))
;个人主页填写邀请表 
(define (inserte-invite2 userId shangji_id )
  (cond [(not(table-query-col  "user" "id"  shangji_id)) #f]
        [(not(eq?(table-query-col  "user" "shangji_id"  userId)-1)) #f ] ;判断是否有上级有上级返回#f
        [(null?(get-xiaji_id userId)) (inserte-invite userId shangji_id)] ;查询是否有下级代理 ,无下级代理可用注册邀请函数
        [(null? (table-query-rows "associate" '(userId shangjiUserId level) (list (cons 'shangjiUserId userId ) (cons 'userId shangji_id))));查询对方是否为自己的下级代理返回数据代表是 返回null代表无 可创建
         (get-level userId shangji_id) ]
        [else #f]))

;建立层级  (
(define (get-level userId shangji_id)
  (define a(query-rows xitong (string-append"select shangjiUserId, userId,level from associate where shangjiUserId="(number->string userId)" and level between 1 and 4")))
  (map (λ (x) (table-insert-one "associate"(list (cons 'shangjiUserId shangji_id) (cons 'userId (vector-ref x 1))  (cons 'level  (+(vector-ref x 2) 1)))));插入多级代理
       a)
  (table-update-one "user" userId (list (cons 'shangji_id shangji_id))) ;在user表填写上级id
  (table-insert-one "associate" (list (cons 'userId userId) (cons 'shangjiUserId shangji_id) (cons 'level 1))) ;插入1级代理
  (build-class shangji_id)
  #t)
   
;团队称号建立
(define (build-class shangji_id) 
  (define all-level1(query-value xitong "select count( userId ) from associate where shangjiUserId =? and level =?" shangji_id 1) )
  (define all-xiaji(query-value xitong "select count( userId ) from associate where shangjiUserId =?" shangji_id))
  (displayln  "建立称号") (displayln shangji_id) (displayln  all-level1) (displayln  all-xiaji) 
  (cond [(and (>= all-level1 300) (>= all-xiaji 2000)) (table-update-one "user" shangji_id '((class . "团长")))]
        [(and (>= all-level1 200) (>= all-xiaji 5000)) (table-update-one "user" shangji_id '((class . "营长")))]
        [(and (>= all-level1 150) (>= all-xiaji 3500)) (table-update-one "user" shangji_id '((class . "连长")))]
        [(and (>= all-level1 5) (>= all-xiaji 7)) (table-update-one "user" shangji_id '((class . "班长")))]
        [(and (>= all-level1 3) (>= all-xiaji 5)) (table-update-one "user" shangji_id '((class . "列兵")))]
        [else null]))
          
;查询下级所有id和级数 返回hash
(define (get-xiaji_id userId [level #f])
  (if level
      (table-query-rows "associate" '(userId shangjiUserId level) (list (cons 'shangjiUserId userId )(cons 'level level)))
      (table-query-many "associate" #:id-name "shangjiUserId" (list userId) '(userId shangjiUserId level))))
;-----------------------------------------------------------------------

(define (update-commission cny userId ) ;添加金额工具
  (query-exec xitong (string-append "update user set userCommission = userCommission + "(number->string cny)" where id  = ?")userId))
(define (add-commissionlog userId rmb content offer_id ) ;添加佣金日志工具
  (table-insert-one "Commission_log" (list (cons 'userId userId) (cons 'rmb rmb) (cons 'Commission_content content) (cons 'offer_id offer_id))))


;二级分佣模式
(define (level2-commission userId rmb )
  (update-commission rmb userId ) ;更新当前用户的佣金
  (add-commissionlog userId rmb "广告分享" userId );添加当前用户佣金日志
  ;查询当前userid一 二级代理 更新佣金
  (let* ([shangji_id1 (table-query-col  "user" "shangji_id"  userId)]
         [shangji_id2 (table-query-col  "user" "shangji_id"  shangji_id1)])
    
    (when (and (not (eq? shangji_id1 -1)) shangji_id1 )
      (team-commission userId) ;先执行团队奖励
      (update-commission (* rmb 0.1)   shangji_id1) ;一级代理分成10%更新佣金
      (add-commissionlog shangji_id1 (* rmb 0.1) "一级代理分成10%" userId )) ;添加一级代理佣金日志
    
    (when (and (not (eq? shangji_id2 -1)) shangji_id2 )
      (update-commission (* rmb 0.05)  shangji_id2);二级代理分成5%更新佣金
      (add-commissionlog shangji_id2 (* rmb 0.05) "二级代理分成5%" userId ))));添加二级代理佣金日志
   
;团队奖励模式
(define (team-commission userId )
  (define shangji (query-rows xitong (string-append "select u2.shangjiUserId ,u2.level,u.class from associate u2
inner join user u on u2.shangjiUserId = u.id  and u2.userId = "(number->string userId)" and u.class != ''")))
  (if (null? shangji ) #f   (begin 
                              (map (λ(group) (let* ([shangji_id (vector-ref group 0)]
                                                    [level (vector-ref group 1)]
                                                    [title (vector-ref group 2)]
                                                    [mon (case level [(1) 0.0065] [(2) 0.0055] [(3) 0.0045] [(4) 0.0035])])
                                               (cond [(and (equal? title "列兵") (eq? level 1))
                                                      (update-commission  mon shangji_id )
                                                      (add-commissionlog shangji_id mon "团队广告" userId )]
                                                     [(and (equal? title "班长") ( <= level 2))
                                                      (update-commission  mon shangji_id )
                                                      (add-commissionlog shangji_id mon "团队广告" userId )]
                                                     [(and (equal? title "连长")(<= level 3))
                                                      (update-commission  mon shangji_id )
                                                      (add-commissionlog shangji_id mon "团队广告" userId )]
                                                     [(and(equal? title "营长")(<= level 4))
                                                      (update-commission  mon shangji_id )
                                                      (add-commissionlog shangji_id mon "团队广告" userId )]
                                                     [(equal? title "团长") (update-commission 0.01 userId )]))) shangji))))
       
