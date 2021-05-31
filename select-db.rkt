#lang racket/base
(require racket/trace racket/vector) (provide (all-defined-out))(require racket/string) 
(require db
         racket/port
         racket/contract 
         "tools.rkt"
         "tools2.rkt"
         "xitong-db.rkt")



; 传表名 查指针 只能返回一列id 
(define (xitong-table table-name [pair-lst #f]) ;列表 例 (xitong-table "user"  (list '(account ."17051006278") '(password . "safssf")))--->'#(53 "17051006278" "safssf")
  (define (result table-name pair-lst) 
  (define-values   (str-k str-and lst-v) (pair-lst->and pair-lst))   
  (apply query-maybe-row
         (append
          (list xitong (string-append "select id," str-k " from " table-name " where "str-and))
          lst-v)))
  (if pair-lst
      (result table-name pair-lst)
      (query-list xitong (string-append "select id from " table-name))))

;查询返回多行值 传入查询条件[pair-list] 要返回的列名值col-lst  返回类型为hasheq
(define (table-query-rows table col-lst pair-list) ;例 (table-query-rows "associate" '(userId shangjiUserId level) '((level . 2) (shangjiUserId . 28)))
  (define-values   (str-k lst-v) (update->kv pair-list))
  (define vs
   (apply query-rows (append
                     (list xitong (string-append"select "(query-eles col-lst)" from " table" where "(string-replace str-k "," " and ")))
                      lst-v)))
 (map (lambda (v)
         (vector->hash col-lst v))
       vs))


 ;查询一个指定页数的表 返回所有id 
(define (xitong-table-in-page table n page-size ) 
  (query-list xitong (string-append "select id from "table" limit ?,? ") (* (- n 1) page-size) (* n page-size )))


;查询列的单个值  传表名 id 返回value            也可用于查询是否已存在的值 例  (table-query-col  "user"  "account"  "187654321" "account") --->> "187654321"
(define (table-query-col  table col  id [col-name "id"]) ;常见用法 例 (table-query-col  "user"  "account"  28)---->>"187654321"
  (sql-null->#f  (query-maybe-value xitong (string-append "select " col " from "table" where "col-name" = ?") id)))


; 查询单行多个值 传表名 id [id-name]  列名(symblo)表    返回 #hash
(define (table-query-one table #:id-name[id-name "id"] id lst) 
  (define v (query-maybe-row xitong (string-append "select " (query-eles lst)" from "table" where "id-name" = ?")id))
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
(define (table-query-row table-name #:id-name[id-name "id"] id )
 (define lst (map (λ(x)
                 (append (string->symbol x))) (get-mame-cols table-name)))
  (define v (query-maybe-row xitong (string-append "select " (query-eles lst)" from "table-name" where "id-name" = ?") id))
  (if v (vector->hash lst v) v))


;连表查询(左连接表) 传两个表名 list【表1要查的列名 symbol类型】 list【表2要查的列名 symbol类型】 连接两表关键字 表1查询的list【id】可以多个id
(define (select-join-table table1 table2 table1-select-keyword-lst  table2-select-keyword-lst join-key [table1-id-lst #f] )
  (define new-lst (append table1-select-keyword-lst table2-select-keyword-lst))
  (define lst-pair
  (if table1-id-lst
      (begin
        (apply query-rows (append
                           (list xitong (string-append "select " (sb->str table1 table1-select-keyword-lst) ","(sb->str table2 table2-select-keyword-lst) " from
"table1" left join "table2" on "join-key" where "(id->str table1 table1-id-lst))) table1-id-lst)))
      (begin    
        (query-rows xitong  (string-append "select " (sb->str table1 table1-select-keyword-lst) ","(sb->str table2 table2-select-keyword-lst) " from
"table1" left join "table2" on "join-key)))))
(map (λ (x) (vector->hash new-lst x)) lst-pair))


      
;查询表中所有列名
(define (get-mame-cols table-name)
  (query-list xitong "select Column_name from information_schema.COLUMNS where TABLE_NAME = ?" table-name))
;根据列统计个数
(define (get-numbers-col table-name [col "id"] )
   (query-value xitong (string-append "select count(" col ") from "table-name)))
;根据某列相加求和 pair-lst为筛选条件
#;(define (get-sum-col table-name  col pair-lst)
  (query-value xitong (string-append "select sum(" col ") from "table-name " where " )))


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
  
  (with-handlers ([exn:fail? (lambda (e) (writeln e) #f)])
    (apply query-exec (append
                       (list xitong (string-append "update " table-name " set " k " where " id-name " = ? "))
                       v (list id)))
    #t))

;删除一行 传id
(define (table-delete-one table id )
  (with-handlers ([exn:fail? (lambda (e) (writeln e) #f)])
    (query-exec xitong (string-append "delete from "table" where id =?") id)
    #t))
;删除多行 传多个id
(define (table-delete-many  table list-id)
  (with-handlers ([exn:fail? (lambda (e) (writeln e) #f)])
    (apply query-exec (append
                       (list
                        xitong (string-append "delete from "table" where id in("(strappend (length list-id) "?" )")"))
                       list-id ))
    #t))
;删除整个表内的数据
(define (table-delete-all  table )
  (with-handlers ([exn:fail? (lambda (e) (writeln e) #f)])
    (query-exec xitong (string-append "delete from "table))
    #t))





;分页查询
(define (xitong-many-in-page table-name start end [sort-col "id"] [order "ASC"]
                             #:filter-pairs [pairs #f])
  (cond
    [pairs
     (let-values ([(cols vs) (pair-list-values pairs)])
       (let ([cols-s (symbol-list->string-with-suffix cols " AND " #:suffix "=?")]
             [formated-vs (sql-format vs)])
         (apply query-list
                (append
                 (list
                  xitong
                  (string-append "SELECT id FROM " table-name
                                 " WHERE " cols-s
                                 " ORDER BY " sort-col " " order
                                 " LIMIT ?, ?"))
                 formated-vs
                 (list start (- end start))))))]
    [else
     (query-list xitong
                 (string-append "SELECT id FROM "table-name " ORDER BY " sort-col " "  order
                                " LIMIT ?, ?")
                 (number->string start)
                 (number->string (- end start)))]))

;----------------------------------------------------
;分销查询工具 
(define (get-level-all ids)
  (for/list ([id ids]) id
    (define (table-query-one-as)
    (define  (query-one-as  )
      (query xitong "select u.id , u.account , u.name , u.shangji_id , u2.name shangji_name, u2.account shangji_account from user u
inner join user u2 on u2.id = u.shangji_id where  u.id = ? "id))
     (cond [(null? (rows-result-rows (query-one-as)))  (query xitong "select id,account,  name , shangji_id from user where id=?"id)]
           [else (query-one-as )]))             
    (define (table-query-one-as2 )
      (query xitong "select u2.userId ,u2.level, u3.name ,u3.account from user u inner join associate u2 on u2.shangjiUserId = u.id
inner join user u3 on  u3.id = u2.userId where u.id =? " id))
    (define (get-all-level pairs)
      (let* ([k-lst (map (λ (k) (string->symbol (cdr (assoc 'name  k)))) (rows-result-headers pairs))]
             [vs (rows-result-rows pairs)])
        (cond [(null? vs) null]
              [(eq? (length vs) 1) (list (make-hasheq (for/list ([k k-lst] [v (car vs)]) (cons k v))))]             
              [else (map (λ (group) (make-hasheq group))     
                    (map (λ (v-vec) 
                           (let loop ([star 0]
                                      [end (- (length  k-lst) 1 ) ]
                                      [k-lst k-lst])
                             (cond [(eq? star  end) (list (cons (car k-lst) (vector-ref v-vec end)))]
                                   [else  (apply cons (list  (cons (car k-lst) (vector-ref v-vec star)) (loop  (+ star 1) end (cdr k-lst))))
                                          ])))vs))])))
               (define result (car(get-all-level (table-query-one-as))))
               (hash-set! result  'xiaji (get-all-level (table-query-one-as2)))
               result))
;----------------------------------------------
