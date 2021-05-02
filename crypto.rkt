#lang racket

(require crypto db base64
         "xitong-db.rkt")
(require crypto/libcrypto)
(crypto-factories (list libcrypto-factory))
(provide userToken)


;更新用户秘钥
;根据账号用户id 更新秘钥
(define (userToken id)
  (query-exec xitong (string-append"update user set userToken =? where id= ?")
              (base64-encode(number->string (current-inexact-milliseconds))) id))
