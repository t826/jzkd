#lang racket
(require racket/trace) (provide (all-defined-out))
(require db
         "tools.rkt"
         "xitong-db.rkt")



; 传表名 查指针 返回所有id  
(define (xitong-table table-name [pair-lst #f]) ;列表 例 (xitong-table "user"  (list '(account ."17051006278") '(password . "safssf")))--->'#(53 "17051006278" "safssf")
  (define (result table-name pair-lst) 
  (define-values   (str-k lst-v) (update->kv pair-lst))    
  (apply query-maybe-row
         (append
          (list xitong (string-append "select id,"(string-replace str-k "=?" "")" from " table-name " where "(string-replace str-k "," " and ")))
          lst-v)))
  (if pair-lst
      (result table-name pair-lst)
      (query-list xitong (string-append "select id from " table-name))))
 ;查询一个指定页数的表 返回所有id 
(define (xitong-table-in-page table n page-size ) 
  (query-list xitong (string-append "select id from "table" limit ?,? ") (* (- n 1) page-size) (* n page-size )))


;查询列的单个值  传表名 id 返回value            也可用于查询是否已存在的值 例  (table-query-col  "user"  "account"  "187654321" "account") --->> "187654321"
(define (table-query-col  table col  id [col-name "id"]) ;常见用法 例 (table-query-col  "user"  "account"  28)---->>"187654321"
  (sql-null->#f  (query-maybe-value xitong (string-append "select " col " from "table" where "col-name" = ?") id)))


; 查询单行多个值 传表名 id [id-name]  列名(symblo)表    返回 #hash
(define (table-query-one table #:id-name[id-name "id"] id lst) 
  (define v (query-maybe-row xitong (string-append "select " (query-eles lst)" from "table" where "id-name" = ?") id))
  (if v (vector->hash lst v) v))

;查询多行对应的多列 提供 多个id 列名表 返回list  list内为 #hash
(define (table-query-many table #:id-name[id-name "id"] id-lst lst) ;查询多个id在这个表内的列 (lst 传要查的 列名symbo类型) 返回 #hash
  (define vs (apply query-rows (append
                                (list xitong (string-append "select " (query-eles lst) " from "table" where " id-name " in (" (strappend (length id-lst) "?" )")"))
                                id-lst)))
  (map (lambda (v)
         (vector->hash lst v))
       vs))

; 查询单行全部值 传表名 id [id-name]     返回 #hash
(define (table-query-row table #:id-name[id-name "id"] id )
 (define lst (map (λ(x)
                 (append (string->symbol(vector->values x)))) (query-rows xitong "select Column_name from information_schema.COLUMNS where TABLE_NAME = ?" table)))
  (define v (query-maybe-row xitong (string-append "select " (query-eles lst)" from "table" where "id-name" = ?") id))
  (if v (vector->hash lst v) v))



;插入数据 传表名 list（list为多个pari数据类型） 
(define (table-insert-one table lst) ;选择插入的表 给par类型list
  (define-values (str question val)
    (pair-list->kv lst))
  (apply query-exec (append
                      (list xitong (string-append "insert into " table "(" str ") values (" question ")"))
                      val)))

;更新数据
(define (table-update-one table-name #:id-name [id-name "id"] id lst) ;选择更新的表 给par类型list更新多个值 默认第二个参数"id"
 (define-values  (k v) (update->kv lst))
  (apply query-exec (append
                     (list xitong (string-append "update " table-name " set " k " where " id-name " = ? "))
                     v (list id))))

;删除一行 传id
(define (table-delete-one table id )
  (query-exec xitong (string-append "delete from "table" where id =?") id))
;删除多行 传多个id
(define (table-delete-many  table list-id )
  (apply query-exec (append
                     (list
                     xitong (string-append "delete from "table" where id in("(strappend (length list-id) "?" )")"))
                     list-id )))
;删除整个表内的数据
(define (table-delete-all  table )
  (query-exec xitong (string-append "delete from "table)))


;添加user表 的用户
(define (User-add lst) ; 例子:(User-add '(("account" . "12899323") ("password" . "323890slf")))
  ;; 获取所有的key，然后组合成一个list
  (define eles
    (map (lambda (v) (car v)) lst))
  (define qe (query-eles eles))
  (define vs (map (lambda (p) (cdr p)) lst))
  (define qv (query-eles (string* (length lst) "?")))
  (apply query-exec
         (append
          (list xitong (string-append "insert into user (" qe ") values (" qv ")"))
          vs)))
