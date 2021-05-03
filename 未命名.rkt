#lang racket
(define a (list '(name . "张世涛") '(account ."17051006219") '(password . "safssf")  '(ip . "255.255.255.255")))
;(assoc  'account 
     ;   (list '(name "张世涛") '(account "17051006219") '(password "safssf")  '(ip "255.255.255.255")))

;(filter (assoc 'ip  a ) a)


;(cons 'A (+ 36 4))


(if #f 'a (values (+ 2 3) 's))





(define vv '((account . "17051006219") (password . "fsfsaf") (ipLog . "255.255.255.255")))
(remove  (assoc 'ipLog vv) vv) 