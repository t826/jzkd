#lang racket/base

(require db
         "collect-core.rkt"
         "../xitong-db.rkt"
         "../select-db.rkt"
         net/http-easy
         )

;;;;;;;;;;; 使用示例 ;;;;;;;;;;;;;;;;;

(define newclass (hasheq '首页 "http://news.jstv.com/pc/c/list1.js?_=" ;首页
                         '社会  "http://news.jstv.com/app/k/app_list.js?_=" ;社会
                         '国内  "http://news.jstv.com/app/i/app_list.js?_=";国内
                         '国际  "http://news.jstv.com/app/j/app_list.js?_=";国际
                         '军事  "http://news.jstv.com/app/r/app_list.js?_=";军事
                         '体育  "http://news.jstv.com/app/m/app_list.js?_=";体育
                         '财经  "http://news.jstv.com/app/l/app_list.js?_=" ;财经
                         '娱乐  "http://news.jstv.com/app/c/app_list.js?_=" ;娱乐
                         '房产  "http://news.jstv.com/app/q/app_list.js?_=" ;房产
                         '科技  "http://news.jstv.com/app/p/app_list.js?_="));科技




(define (get-data newclass)
  (thread
   (λ ()
     (let loop ([newclass newclass])
       (for ([(k v) newclass]) ;遍历各个版块
         (with-handlers ([exn:fail? (lambda (e) (writeln e))])
           (map (λ (group) ;遍历整个版块
                  (define group_pair (hash->list group))
                  (let ([elet (get-news-in-page (hash-ref group 'linkHref ))])
                    (unless (or (not elet)
                                (table-query-col  "newsdata" "title"  (hash-ref group 'title) "title"))
                      (table-insert-one "newsdata"
                                        (append (remove (assoc 'type group_pair) (remove (assoc 'linkHref group_pair) group_pair))
                                                elet
                                                (list (cons 'newstime (number->string(current-milliseconds))) (cons 'catename (symbol->string k))))))))
                (do-collect v))))
       (sleep 120)
       (loop newclass))))) ;休眠一分半从新遍历各个版块
  
  

(get-data newclass)


#|

;; 遍历 uri中的所有页
(define (start-collect [i 0])
  (thread (λ ()
            (let loop ([n2s (get-news-in-page i)]
                       [i i])
              (for ([n2s n2s])
                (define rs (do-collect (hash-ref n2s 'uri)))
                (unless (or (not rs) (table-query-col  "newsdata" "title"  (hash-ref n2s 'title) "title"))
                  (displayln (hash-ref rs 'newstime))
                  (table-insert-one "newsdata" `((title . ,(hash-ref n2s 'title))
                                                  (pic . ,(hash-ref n2s 'pic))
                                                  (befrom . ,(hash-ref rs 'befrom))
                                                  (catename . ,(hash-ref rs 'catename))
                                                  (content . ,(hash-ref rs 'content))
                                                  (newstime . ,(hash-ref rs 'newstime))))))
              (when (>= i 10)
                (set! i 0)
                (sleep 80))
              (loop (get-news-in-page (+ 1 i)) i))))

  #;(thread (λ ()
            (let delete()
              (displayln "delete")
              (query-exec xitong "delete from  newsdata where timestampdiff(day ,newstime,now())>=2")
              (sleep 86400)
              (delete)))))

(start-collect 0)


|#