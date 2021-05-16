#lang racket/base

(require "select-db.rkt")

;注册时人填写邀请
(define (inserte-invite userId shangji_id [level 2] )
  (unless (table-query-col  "user" "shangji_id"  userId)
  (table-update-one "user" userId (list (cons 'shangji_id shangji_id))) 
  (table-insert-one "associate" (list (cons 'userId userId) (cons 'shangjiUserId shangji_id) (cons 'level level))) ;插入二级代理
  
  (let loop ([shangji_id (table-query-col  "user" "shangji_id"  shangji_id)]
              [level level])
    (cond [shangji_id 
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
  (table-query-rows "associate" '(userId shangjiUserId level) (list (cons 'shangjiUserId userId )(cons 'userId shangji_id))))