#lang racket

(require db)
(provide (all-defined-out))

;连接数据库
(define xitong
  (virtual-connection
   (connection-pool
    (lambda ()
      (mysql-connect #:server "127.0.0.1"
                     #:port 3306
                     #:database "xitong"
                     #:user "xitong"
                     #:password "1234Abc")))))
