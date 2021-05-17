#lang racket/base

(require "select-db.rkt")
(provide (all-defined-out))

;注册时人填写邀请
(define (inserte-invite userId shangji_id [level 2] )
  (unless (or (table-query-col  "user" "shangji_id"  userId) (eq? userId shangji_id))
  (table-update-one "user" userId (list (cons 'shangji_id shangji_id))) 
  (table-insert-one "associate" (list (cons 'userId userId) (cons 'shangjiUserId shangji_id) (cons 'level level))) ;插入二级代理
  (let loop ([shangji_id (table-query-col  "user" "shangji_id"  shangji_id)]
              [level level])
    (cond [(eq? level 6) "调整完毕" ]
          [shangji_id 
          (table-insert-one "associate" (list (cons 'userId userId) (cons 'shangjiUserId shangji_id) (cons 'level (+ level 1)))) ;插入多级代理
          (loop (table-query-col  "user" "shangji_id"  shangji_id) (+ level 1))]
          [else "调整完毕"]))))

;查询下级所有id和级数 返回hash
(define (get-xiaji_id userId [level #f])
  (if level
      (table-query-rows "associate" '(userId shangjiUserId level) (list (cons 'shangjiUserId userId )(cons 'level level)))
      (table-query-many "associate" #:id-name "shangjiUserId" (list userId) '(userId shangjiUserId level))))
;个人主页填写邀请表
(define (inserte-invite2 userId shangji_id )
  (cond [(table-query-col  "user" "shangji_id"  userId) #f ]
  [(null? (table-query-rows "associate" '(userId shangjiUserId level) (list (cons 'shangjiUserId userId ) (cons 'userId shangji_id))))
   (inserte-invite userId shangji_id)];查询对方是否为自己的下级代理返回数据代表是 返回null代表无 可创建
  [else #f]))