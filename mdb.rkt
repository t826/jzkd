#lang racket/base

(require db
         "xitong-db.rkt"
         "select-db.rkt"
         "tools2.rkt")

(provide (all-defined-out))

(define (get-all-cols table-name)
  (define rs (query-list xitong "select Column_name from information_schema.COLUMNS where TABLE_NAME = ?" table-name))
  (map (lambda (v)
         (string->symbol v))
       rs))


(define (user-check-permission user-token user-type)
  (define rs
    (query-maybe-value xitong
                       (string-append "SELECT userType FROM user"
                                      " WHERE userToken = ?")
                       user-token))
  (equal? user-type rs))

  