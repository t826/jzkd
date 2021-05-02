#lang racket

(provide query-eles strappend list->values string* vector->hash pair-list->kv update->kv sql-null->#f )
(require racket/trace db) 
         

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


(define (sql-null->#f k)
  (if (sql-null? k) #f k))

(define (vector->hash list-key vector-value)
  (apply hash
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
;    '((a . 3) (b . 6))
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
        






   