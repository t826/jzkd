#lang racket/base

(require net/uri-codec
         net/url
         racket/port db
         "tools.rkt" "select-db.rkt" "xitong-db.rkt")
(provide (all-defined-out))



;校验短信
(define (checkcode phone code-str)
  (if (equal? code-str(query-maybe-value xitong "select code from check_verify where account = ? and TIMESTAMPDIFF(minute,create_date ,now())<5" phone))
  #t #f))

;发送短信
(define (sendcode phone )
  (if (query-maybe-value xitong "select code from check_verify where account = ? and TIMESTAMPDIFF(minute,create_date ,now())<5" phone) #f
      
  (let* ([code-str (number->string (random 10000 99999))]
         [pre-str "您好,您的验证码是:"]
         [last-str "(5分钟内有效) ,请勿泄漏给他人。如非本人操作,请忽略本条消息!"]
         [msg (string-append pre-str code-str last-str)])
    (define rs
      (call/input-url
       (string->url
        (string-append
         "https://mb345.com/ws/BatchSend2.aspx?CorpID=NJLKJ0006419&Pwd=www15136029076&Mobile="
         phone "&Content="(string->%unicode msg)))
       get-pure-port
       (lambda (ip)
         (port->string ip))))
    (if (> (string->number rs) 0)
        (begin
        (table-insert-one "check_verify" (list (cons 'account phone ) (cons 'code code-str))) #t)
        #f))))