#lang racket
( require web-server/http/xexpr
         web-server/http/json)



#;(for  ([i (vector 40.0 96.0 20.0 0.0 0.0)]
       [j (vector 0.0 96.0 0.0 0.0 0.0)]
       [k (vector "blanWithdraw" "waitWithdraw" '"sucWithdraw" "refWithdra" "advTopUp")]
        #:when  (not(= (- i j) 0))
         [c (list (- i j))])
  (display(list k (- i j ))))


#;(display (map hash-set!
    (list 'id  'account 'name 'myImg  'userCommission 'userType 'userToken)
    (list 28  "15077090433" "张世涛" "<sql-null>" "<sql-null>" "ordUser" "MTYxOTU3NTE1OTIyMi4xMjA2")))

(let abc ([rs (make-hash)]
          [l1 (list 'id  'account 'name 'myImg  'userCommission 'userType 'userToken)]
          [l2 (list 28  "15077090433" "张世涛" "<sql-null>" "<sql-null>" "ordUser" "MTYxOTU3NTE1OTIyMi4xMjA2")])
  (cond
    [(or (null? l2) (null? l1)) rs]
    [else
     (hash-set! rs (car l1) (car l2))
     (abc rs (cdr l1) (cdr l2))]))

(define rs (make-hash))
(for ([v1 (list 'id  'account 'name 'myImg  'userCommission 'userType 'userToken)]
      [v2 (list 28  "15077090433" "张世涛" "<sql-null>" "<sql-null>" "ordUser" "MTYxOTU3NTE1OTIyMi4xMjA2")])
  (hash-set! rs v1 v2))
(display rs)


(for ([i '(1 2 3)]
        [j "abc"]
        #:final (not (odd? i))
        [k #(#t #f)])
    (display (list i j k)))
;(response/jsexpr 