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

;; 52
(define insertL
          (lambda (new old lat)
            (cond
              ((null? lat)(quote()))
              (else (cond
                      ((eq? (car lat) old)
                       (cons new lat))
                      (else (cons (car lat)
                                  (insertL new old (cdr lat)))))))))

;; 53
(define subst
          (lambda (new old lat)
            (cond
              ((null? lat)(quote()))
              (else (cond
                      ((eq? (car lat) old)
                       (cons new (cdr lat)))
                      (else (cons (car lat)
                                  (subst new old (cdr lat)))))))))

;; 53
(define subst2
          (lambda (new o1 o2 lat)
            (cond
              ((null? lat)(quote()))
              (else (cond
                      ((or (eq? (car lat) o1)(eq? (car lat) o2))
                       (cons new (cdr lat)))
                      (else (cons (car lat)
                                  (subst2 new old (cdr lat)))))))))

;; p54
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat)(quote ()))
     (else (cond
            ((eq? (car lat) a)(multirember a (cdr lat)))
            (else (cons (car lat) (multirember a (cdr lat)))))))))

;; p57
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat)(quote ()))
     (else
      (cond
       ((eq? (car lat) old)
        (cons old (cons new (multiinsertR new old (cdr lat)))))
       (else (cons (car lat)
                   (multiinsertR new old (cdr lat)))))))))

;; 58
(define multiinsertL
          (lambda (new old lat)
            (cond
              ((null? lat)(quote()))
              (else (cond
                      ((eq? (car lat) old)
                       (cons new (cons old (multiinsertL new old (cdr lat)))))
                      (else (cons (car lat)
                                  (multiinsertL new old (cdr lat)))))))))

;; 59
(define multisubst
          (lambda (new old lat)
            (cond
              ((null? lat)(quote()))
              (else (cond
                      ((eq? (car lat) old)
                       (cons new (multisubst new old (cdr lat))))
                      (else (cons (car lat)
                                  (multisubst new old (cdr lat)))))))))

;; p61
(define add1
          (lambda (n)
            (+ n 1)))

(define sub1
          (lambda (n)
            (- n 1)))

;;p62
(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

;; p63
(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))
