#lang racket
(provide pred-p)
(provide single-digit-p)
(provide single-alphabet-p)
(provide seq)
(provide alt)
(provide epsilon-p)
(provide zero-or-more)
(provide one-or-more)
(provide whitespace-p)
(provide number-p)
(provide identifier-p)
(provide variable-p)
(provide term-p)
(provide expression-p)
(provide assignment-p)
(define-struct gnode (sym list) #:transparent)
(define-struct ident (str) #:transparent)
(define-struct num (val) #:transparent)

(define (pred-p p)
  (λ (str)
    (if (not (equal? str ""))
        (let [(a (string-ref str 0))]
          (if (p a)
              (cons a (substring str 1))
              'fail))
        'fail)))
(define single-digit-p
  (pred-p
   (λ (c)
     (let [(n (char->integer c))]
       (and (>= n 48) (<= n 57))))))

(define single-alphabet-p
  (pred-p
   (λ (c)
     (let [(n (char->integer c))]
       (or (and (>= n 97) (<= n 122)) (and (>= n 65) (<= n 90)))))))
(define (combine-cc char1 char2)
  (list->string (list char1 char2)))
(define (combine-sc str char)
  (list->string (append (string->list str) (list char))))
(define (combine-cs char str)
  (list->string (cons char (string->list str))))
(define (combine-ss str1 str2)
  (list->string (append (string->list str1) (string->list str2))))

(define (seq p1 p2 f)
  (λ (str)
    (if  (not (eq? (p1 str) 'fail))
         (if (not (eq? (p2 (substring str 1)) 'fail))
             (cons (f (car (p1 str)) (car (p2 (substring str 1)))) (cdr (p2 (substring str 1))))
             'fail)
         'fail)))
(define (alt p1 p2)
  (λ (str)
    (if (eq? (p1 str) 'fail)
        (if (eq? (p2 str) 'fail)
            'fail
            (p2 str))
        (p1 str))))
(define epsilon-p
  (λ (str)
    (cons "" str)))
(define x 0)
(define (zero-or-more p f)
  (define s "")  
  (λ (str)
    (define (zero-helper i)
      (if (equal? (p (substring str i)) 'fail)
          (begin (set! s (substring str i))
                     "") 
          (f (car (p (substring str i))) (if (eq? (substring str (+ i 1)) "") ""
                                             (zero-helper (+ i 1)))))) 
      (if (equal? (p str) 'fail)
          (epsilon-p str)
          (cons (zero-helper 0) s))))

(define (one-or-more p f)
  (λ (str)
    (if (equal? (p str) 'fail)
        'fail
        ((zero-or-more p f) str))))
(define whitespace-p
  (λ (str)
    (define (whitespace-helper i)
      (cond [(equal? "" (substring str i)) (cons "" "")] 
            [(char-whitespace? (string-ref str i))
             (whitespace-helper (+ i 1))]
            [else (cons "" (substring str i))]))
    (if (equal? "" str)
        (cons "" "")
        (whitespace-helper 0))))

(define number-p
  (λ (str1)
    (let* [(str (cdr (whitespace-p str1)))
          (str2 ((zero-or-more single-digit-p combine-cs) str))]
      (if (not (equal? (single-digit-p str) 'fail))
          (cons (make-num (string->number (car str2))) (cdr str2))
          'fail))))

(define identifier-p
  (λ (str1)
    (let* [(str (cdr (whitespace-p str1)))
          (str2 ((zero-or-more (alt single-digit-p single-alphabet-p)
                                             combine-cs) str))] 
      (if (not (equal? (single-alphabet-p str) 'fail))
          (cons (make-ident (car str2)) (cdr str2))
          'fail))))
               
(define (comp-sq-brac-find str i count)
    (cond [(= count -1) (- i 1)]
          [(equal? (substring str i) "") 'fail]
          [(equal? (string-ref str i) #\[) (comp-sq-brac-find str (+ i 1) (+ count 1))]
          [(equal? (string-ref str i) #\]) (comp-sq-brac-find str (+ i 1) (- count 1))]
          [else (comp-sq-brac-find str (+ i 1) count)]))

(define (comp-paran-find str i count)
    (cond [(= count -1) (- i 1)]
          [(equal? (substring str i) "") 'fail]
          [(equal? (string-ref str i) #\() (comp-paran-find str (+ i 1) (+ count 1))]
          [(equal? (string-ref str i) #\)) (comp-paran-find str (+ i 1) (- count 1))]
          [else (comp-paran-find str (+ i 1) count)]))

(define variable-p
  (λ (str1)
    (let* [(str (identifier-p str1))] 
      (if  (equal? 'fail str) 'fail
           (let*  [(str2 (cdr (whitespace-p (cdr str))))]
             (cond [(and (not (equal? "" str2))
                         (equal? (string-ref str2 0) #\[)
                         (not (equal? (comp-sq-brac-find str2 1 0) 'fail)))
                    (let* [(a (comp-sq-brac-find str2 1 0))
                           (str3 (expression-p (substring str2 1  a )))]
                      (if (equal? 'fail str3) str
                          (if (equal? (cdr (whitespace-p (cdr str3))) "")
                              (cons (make-gnode 'ARRAY (list (car str) (car str3)))
                                    (substring str2 (+ a 1)))
                              str)))]
                   [else str]))))))

(define term-p
  (λ (str1)
    (let [(str (cdr (whitespace-p str1)))]
      (cond [(not (equal? 'fail (single-digit-p str))) (number-p str)]
            [(not (equal? 'fail (single-alphabet-p str))) (variable-p str)]
            [(equal? str "") 'fail]
            [(equal? #\( (string-ref str 0))
             (let [(a (comp-paran-find str 1 0))]
               (if (equal? 'fail a) 'fail
                   (let [(main (expression-p (substring str 1 a)))]
                     (if (equal? "" (cdr (whitespace-p (cdr main))))
                         (cons (car main) (substring str (+ a 1)))
                         'fail))))]
            [else 'fail]))))
(define expression-p
  (λ (str1)
    (let* [(str (cdr (whitespace-p str1)))
           (str2 (term-p str))]
      (if (equal? 'fail str2) 'fail
          (let* [(castr2 (car str2))
                 (cdstr2 (cdr str2))
                 (str3 (cdr (whitespace-p cdstr2)))]
            (cond [(equal? "" (cdr (whitespace-p str3))) str2]
                  [(equal? #\+ (string-ref str3 0))
                   (let* [(str4 (cdr (whitespace-p (substring str3 1))))
                          (str5 (expression-p str4))]
                     (if (or (equal? "" str4)
                             (equal? (term-p str4) 'fail))
                         str2
                         (if (equal? 'fail str5)
                             'fail
                             (cons (make-gnode 'PLUS
                                               (list castr2
                                                     (car str5)))
                           (cdr str5)))))]
                  [else str2]))))))
(define assignment-p
  (λ (str1)
    (let* [(str (cdr (whitespace-p str1)))
           (str2 (variable-p str))]
      (if (equal? 'fail str2)
          'fail
          (let [(str3 (cdr (whitespace-p (cdr str2))))]
            (cond [(equal? "" str3) 'fail]
                  [(equal? #\= (string-ref str3 0))
                   (let* [(str4 (cdr (whitespace-p (substring str3 1))))
                          (str5 (expression-p str4))]
                    (if (equal? 'fail str5)
                        'fail
                        (cons (make-gnode 'ASSIGN
                                 (list (car str2)
                                       (car str5)))
                          (cdr str5))))]))))))