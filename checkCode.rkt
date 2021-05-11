#lang racket/base

(require net/uri-codec
         net/url
         racket/port
         "tools.rkt")
(provide (all-defined-out))


;发送短信
(define (web-sendcode)#t)
;校验短信
(define (web-checkcode )#t)


(define (send-verify-code phone code-str)
  (let* ([pre-str "您好,您的验证码是:"]
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
        code-str
        #f)))