#lang racket/base

(require db "select-db.rkt" "xitong-db.rkt")
(provide (all-defined-out))

;注册时人填写邀请  (此时注册人没有上下级代理关系）
(define (inserte-invite userId shangji_id [level 2] )
  (unless (or(eq? userId shangji_id) (not(eq? (table-query-col  "user" "shangji_id"  userId) -1))) ;查找useid是否已经存在上级id
  (table-update-one "user" userId (list (cons 'shangji_id shangji_id))) ;在user表填写上级id
  (table-insert-one "associate" (list (cons 'userId userId) (cons 'shangjiUserId shangji_id) (cons 'level level))) ;插入二级代理
  (let loop ([shangji_id (table-query-col  "user" "shangji_id"  shangji_id)]
              [level level])
    (cond [(eq? level 6) #t ]
          [(not (eq? shangji_id -1))
          (table-insert-one "associate" (list (cons 'userId userId) (cons 'shangjiUserId shangji_id) (cons 'level (+ level 1)))) ;插入多级代理
          (loop (table-query-col  "user" "shangji_id"  shangji_id) (+ level 1))]
          [else #t]))))

;查询下级所有id和级数 返回hash
(define (get-xiaji_id userId [level #f])
  (if level
      (table-query-rows "associate" '(userId shangjiUserId level) (list (cons 'shangjiUserId userId )(cons 'level level)))
      (table-query-many "associate" #:id-name "shangjiUserId" (list userId) '(userId shangjiUserId level))))
;个人主页填写邀请表
(define (inserte-invite2 userId shangji_id )
  (cond [(not(eq?(table-query-col  "user" "shangji_id"  userId)-1)) #f ] ;判断是否有上级有上级返回#f
        [(null?(get-xiaji_id userId)) (inserte-invite userId shangji_id)] ;查询是否有下级代理 ,无下级代理可用注册邀请函数
        [(null? (table-query-rows "associate" '(userId shangjiUserId level) (list (cons 'shangjiUserId userId ) (cons 'userId shangji_id))));查询对方是否为自己的下级代理返回数据代表是 返回null代表无 可创建
         (get-level userId shangji_id) ]
        [else #f]))



;查询工具
(define (get-level userId shangji_id)
 (define a 
(query-rows xitong (string-append"select shangjiUserId, userId,level from associate where shangjiUserId="(number->string userId)" and level between 2 and 4")))
   (map (λ (x) (vector-set! x 0 shangji_id)  (vector-set! x 2 (+ (vector-ref x 2 )1))
           (table-insert-one "associate"(list (cons 'shangjiUserId (vector-ref x 0)) (cons 'userId (vector-ref x 1))  (cons 'level  (vector-ref x 2)))));插入多级代理
        a)
  (table-update-one "user" userId (list (cons 'shangji_id shangji_id))) ;在user表填写上级id
  (table-insert-one "associate" (list (cons 'userId userId) (cons 'shangjiUserId shangji_id) (cons 'level 2))) #t);插入二级代理
   