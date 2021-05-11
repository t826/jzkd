#lang racket/base

(provide query-eles strappend list->values string* vector->hash pair-list->kv update->kv sql-null->#f  sb->str id->str)
(require racket/trace db)  (require db/util/datetime)  (require racket/date)
         

(define (query-eles lst) ;symbol->string
  (cond
    [(null? lst) ""]
    [else
     (if (null? (cdr lst))
         (symbol->string(car lst))
         (string-append (symbol->string(car lst)) "," (query-eles (cdr lst))))]))


(define-syntax list->values
  (syntax-rules ()
    [(_ '(e1 ...))
     (values e1 ...)]))


(define (string* n s) 
  (cond
    [(= n 0) '()]
    [else (cons s (string* (- n 1) s))]))



(define (strappend n s) ; (strappend 3 "?")--->  "?,?,?"
  (cond
    [(= n 0) '()]
    [(= n 1) s]
    [else (string-append (string-append s ",") (strappend (- n 1) s))]))




;(define b (vector "陈权业" "15077090552" "MTYxOTQxMDEzNzEyOC43MzM2"))
;(define a (list 'name 'account 'userTkoen))
;(define number -1)
(date-display-format 'iso-8601)

(define (sql-null->#f k)
  (cond [(sql-null? k) #f]
        [(sql-timestamp? k)
         (date->string (sql-datetime->srfi-date k) #t)]
        [else k]))

(define (vector->hash list-key vector-value)
  (apply hasheq
         (let loop ([lk list-key]
                    [vv vector-value]
                    [index (- (length list-key) 1)])
           (cond
             [(= index -1) (hasheq)]
             [(= index 0) (list (list-ref lk index)
                                (sql-null->#f (vector-ref vv index)))]
             [else
              (append (list (list-ref lk index) (sql-null->#f (vector-ref vv index)))
                     (loop lk vv (- index 1)))]))))


;传pair类型list 返回key 或者values 列表
;    '((a . 3) (b . 6)) -->  "a,b"   "?,?"    '(3 6)
(define (pair-list->kv lst ) ;插入数据用  
  
  (define str-lst 
           (map (lambda(x)
        (string-append (symbol->string (car x)))) lst))  
  (define (key-str str-lst)
     (cond [(null? str-lst) null]
          [(null? (cdr str-lst)) (car str-lst)]
          [else
           (string-append (car str-lst) "," (key-str (cdr str-lst)))]))

  (define value-lst (map (lambda(x)
        (append (cdr x))) lst))
  (values (key-str str-lst) (strappend (length lst) "?") value-lst))


;更新数据用
(define (update->kv lst) ;--> "name = ?， account = ?, userToken = ?"            
  (define key-lst 
    (map (lambda(x)
           (string-append (symbol->string (car x)))) lst))
  (define val-lst (map (lambda(x)
                         (append (cdr x))) lst))
  (define (str-k key-lst )
  (cond [(null? key-lst) null]
        [(null? (cdr key-lst)) (string-append (car key-lst) "=?" )]
        [else
         (string-append (car key-lst) "=?," (str-k (cdr key-lst)))]))
  
  (values (str-k key-lst) val-lst))


;连接表查询用
(define (sb->str table-name lst) ;(sb->str "user" '(name id account)) -->"user.name,user.id,user.account"
  (cond [(null? lst) null]
        [(null? (cdr lst)) (string-append table-name "." (symbol->string (car lst)))]
        [else (string-append table-name "." (symbol->string (car lst)) "," (sb->str table-name (cdr lst)))]))
(define (id->str table-name id-lst) ;(id->str "user" '(1 2 3 4)) -->"user.id=? or user.id=? or user.id=? or user.id=?"
  (let loop ([indx (- (length id-lst)1)]
             [table-name table-name])
 (cond [(eq? indx 0) (string-append table-name ".id=?" )]
       [else (string-append  table-name ".id=? or " (loop (- indx 1) table-name))])))

       
    

;; 字符转utf-8
(define (string->%unicode s)
  (let loop ([s s]
             [index 0])
    (cond
      [(= index (string-length s)) ""]
      [else
       (let ([i (char->integer (string-ref s index))])
         (if (> i 128)
             (string-append "%u"
                            (number->string i 16)
                            (loop s (+ index 1)))
             (string-append "%"
                            (number->string i 16)
                            (loop s (+ index 1)))))])))

               





   