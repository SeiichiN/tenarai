;; Scheme手習い
;; p16
(define lat?
          (lambda (l)
            (cond
              ((null? l) #t)
              ((atom? (car l))(lat? (cdr l)))
              (else #f))))

;; p23
(define member?
          (lambda (a lat)
            (cond
              ((null? lat) nil)
              (else (or (eq? (car lat) a)
                        (member? a (cdr lat)))))))

;; p35
(define rember
  (lambda (a lat)
    (cond
     ((null? lat)(quote ()))
     (else (cond
            ((eq? (car lat) a)(cdr lat))
            (else (cons (car lat)
                        (rember a (cdr lat)))))))))

;; p46
(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote()) )
     (else (cons (car (car l))
                 (firsts (cdr l)))))))

;; p52
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat)(quote ()))
     (else
      (cond
       ((eq? (car lat) old)
        (cons old (cons new (cdr lat))))
       (else (cons (car lat)
                   (insertR new old (cdr lat)))))))))


