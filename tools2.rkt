#lang racket/base

(require racket/date
         racket/class)

(provide (all-defined-out))


(define (list-contains? lst a)
  (cond
    [(null? lst) #f]
    [else
     (or (equal? a (car lst))
         (list-contains? (cdr lst) a))]))


(define (string-append-n-with-suffix s suffix-s n)
  (cond
    [(<= n 0) ""]
    [(= n 1) s]
    [else
     (string-append s
                    suffix-s
                    (string-append-n-with-suffix s suffix-s (- n 1)))]))


(define (symbol-list->string-with-suffix lst extra-s #:suffix [suffix-s ""])
  (cond
    [(null? lst) ""]
    [else
     (if (null? (cdr lst))
         (string-append (symbol->string (car lst)) suffix-s)
         (string-append (string-append (symbol->string (car lst)) suffix-s)
                        extra-s
                        (symbol-list->string-with-suffix (cdr lst) extra-s #:suffix suffix-s)))]))


(define (rows->hasheq lst vec)
  (let loop ([vec vec]
             [lst lst]
             [index (- (length lst) 1)])
    (cond
      [(= index -1) (hasheq)]
      [(= index 0) (cons (list-ref lst index)
                         (cons (vector-ref vec index) '()))]
      [(= index (- (length lst) 1))
       (apply hasheq
              (cons (list-ref lst index)
                    (cons (vector-ref vec index)
                          (loop vec lst (- index 1)))))]
      [else
       (cons (list-ref lst index)
             (cons (vector-ref vec index)
                   (loop vec lst (- index 1))))])))


(define (jsexpr-test jsexpr key-lst)
  (cond
    [(null? key-lst) #t]
    [else
     (and (hash-ref-key jsexpr (car key-lst) #f)
          (jsexpr-test jsexpr (cdr key-lst)))]))


(define (pair-list-values pairs)
  (values
   (map (lambda (p)
          (car p))
        pairs)
   (map (lambda (p)
          (cdr p))
        pairs)))


(define (hash-kv-values ht #:remove-keys [rm-keys #f])
  (define keys (hash-keys ht))
  (when rm-keys (set! keys (rembers keys rm-keys)))
  (define vs
    (let loop-values ([keys keys])
      (cond
        [(null? keys) '()]
        [else
         (cons (hash-ref ht (car keys))
               (loop-values (cdr keys)))])))
  (values keys vs))


(define (jsexpr-values jsexpr key-lst)
  (apply values
         (let loop ([jsexpr jsexpr]
                    [key-lst key-lst])
           (cond
             [(null? key-lst) '()]
             [else
              (cons (hash-ref jsexpr (car key-lst) #f)
                    (loop jsexpr (cdr key-lst)))]))))


(define (sql-format lst)
  (cond
    [(null? lst) '()]
    [(equal? (car lst) 'null)
     (cons "" (sql-format (cdr lst)))]
    [else
     (cons (car lst) (sql-format (cdr lst)))]))


(define (check&get-bindings-list cols bindings)
  (cond
    [(null? bindings) '()]
    [(list-contains? cols (caar bindings))
     (cons (cons (caar bindings) (cdar bindings))
           (check&get-bindings-list cols (cdr bindings)))]
    [else
     (check&get-bindings-list cols (cdr bindings))]))


(define (rembers lat ls)
  (cond
    [(null? lat) '()]
    [(list-contains? ls (car lat))
     (rembers (cdr lat) ls)]
    [else
     (cons (car lat) (rembers (cdr lat) ls))]))



